open Json_encoding
open Types
module ST = SolidityTypes
module SV = SolidityValue

type event = {name: string; args: SV.t list}
type named = {name: string; t: ST.t; indexed: bool}

let of_named {t; _} = t

let extended =
  let open ST in
  let open Angstrom in
  choice
    [ (Parser.t >>| fun x -> `Simple x); (string "tuple" >>| fun _ -> `Tup);
      (string "tuple[" *> Parser.numopt <* char ']' >>| fun n -> `Tups n) ]

let typ =
  conv
    (fun _ -> assert false)
    (fun x ->
      Angstrom.parse_string ~consume:All extended x
      |> function Error msg -> failwith msg | Ok x -> x)
    string

let named x =
  let construct (name, typ, components, indexed, _) =
    match typ with
    | `Simple t -> {name; t; indexed}
    | `Tup -> {name; t= Tuple (List.map of_named components); indexed}
    | `Tups None ->
        {name; t= VArray (Tuple (List.map of_named components)); indexed}
    | `Tups (Some n) ->
        {name; t= FArray (n, Tuple (List.map of_named components)); indexed}
  in
  conv
    (function _ -> assert false)
    construct
    (obj5 (req "name" string) (req "type" typ)
       (dft "components" (list x) [])
       (dft "indexed" bool false)
       (dft "internalType" string ""))

let named = mu "named" named

module Fun = struct
  type t =
    { kind: kind;
      name: string;
      inputs: named array;
      outputs: named array;
      mutability: mutability }

  and kind = Function | Constructor | Receive | Fallback

  and mutability = Pure | View | Nonpayable | Payable

  let kind =
    string_enum
      [ ("function", Function); ("constructor", Constructor);
        ("receive", Receive); ("fallback", Fallback) ]

  let mutability =
    string_enum
      [ ("pure", Pure); ("view", View); ("nonpayable", Nonpayable);
        ("payable", Payable) ]

  let encoding =
    conv
      (fun _ -> assert false)
      (fun ((), (kind, name, inputs, outputs, mutability)) ->
        {kind; name; inputs; outputs; mutability})
      (merge_objs unit
         (obj5 (req "type" kind) (dft "name" string "")
            (dft "inputs" (array named) [||])
            (dft "outputs" (array named) [||])
            (req "stateMutability" mutability)))

  let is_constructor = function {kind= Constructor; _} -> true | _ -> false
end

module Evt = struct
  type t = {name: string; inputs: named array; anonymous: bool}

  let encoding =
    conv
      (fun _ -> assert false)
      (fun ((), name, inputs, anonymous) -> {name; inputs; anonymous})
      (obj4
         (req "type" (constant "event"))
         (dft "name" string "")
         (dft "inputs" (array named) [||])
         (req "anonymous" bool))
end

type t = Fun of Fun.t | Event of Evt.t

let event = function Fun _ -> None | Event x -> Some x
let func = function Fun x -> Some x | Event _ -> None

let encoding =
  union
    [ case Fun.encoding (function Fun x -> Some x | _ -> None) (fun x -> Fun x);
      case Evt.encoding
        (function Event x -> Some x | _ -> None)
        (fun x -> Event x) ]

(* -------------------------------------------------------------------------------- *)
(* Computing function selectors *)

let string_of_signature name inputs =
  let inputs = Array.to_list inputs in
  let inputs = List.map (fun {t; _} -> t) inputs in
  name ^ ST.to_string (Tuple inputs)

let keccak str = Cryptokit.(hash_string (Hash.keccak 256) str)

let keccak_4_bytes str =
  Cryptokit.(hash_string (Hash.keccak 256) str) |> fun s -> String.sub s 0 4

let method_id {Fun.name; inputs; _} =
  keccak_4_bytes (string_of_signature name inputs)
  |> Bitstring.bitstring_of_string

let event_id {Evt.name; inputs; anonymous= _} =
  keccak (string_of_signature name inputs) |> Bitstring.bitstring_of_string

let create2 ~addr ~salt ~initCode =
  let open SV in
  tuple [nbytes "\xff"; address addr; nbytes salt; nbytes initCode]
  |> packed |> Bitstring.string_of_bitstring
  |> fun s -> String.sub (keccak s) 12 20

(* -------------------------------------------------------------------------------- *)

let to_0x b =
  let (`Hex res) = Hex.of_string (Bitstring.string_of_bitstring b) in
  "0x" ^ res

let event_of_log abis log =
  let codes =
    List.fold_left
      (fun acc abi ->
        match abi with
        | Event event_abi ->
            let id = to_0x (event_id event_abi) in
            (id, event_abi) :: acc
        | _ -> acc)
      [] abis in
  let topics = log.Log.topics in
  let data = log.data in
  (* Check whether /at most one/ topic corresponds to an event *)
  let relevant =
    List.filter
      (fun hash -> List.mem_assoc hash codes)
      (Array.to_list topics :> string list) in
  let event =
    match relevant with
    | [hash] -> List.assoc hash codes
    | _ -> failwith "0 or > 1 matching event for topic, aborting" in
  let types = Array.to_list event.inputs in
  let ty = ST.Tuple (List.map (fun {t; _} -> t) types) in
  let fields =
    match SV.decode ty (Bitstring.bitstring_of_string data) with
    | {v= SV.Tuple values; _} -> values
    | exception _ -> failwith "decode_events: error while decoding"
    | _ -> failwith "decode_events: bug found" in
  {name= event.name; args= fields}
