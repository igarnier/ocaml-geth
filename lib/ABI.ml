open Json_encoding
open Types
module ST = SolidityTypes
module SV = SolidityValue

type event = {name: string; args: SV.t list} [@@deriving show]
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
let keccak str = Cryptokit.(hash_string (Hash.keccak 256) str)

let keccak_4_bytes str =
  Cryptokit.(hash_string (Hash.keccak 256) str) |> fun s -> String.sub s 0 4

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

  let signature {name; inputs; _} =
    let inputs = Array.to_list inputs in
    let inputs = List.map (fun {t; _} -> t) inputs in
    name ^ ST.to_string (Tuple inputs)

  let selector x = keccak_4_bytes (signature x)
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

  let signature {name; inputs; _} =
    let inputs = Array.to_list inputs in
    let inputs = List.map (fun {t; _} -> t) inputs in
    name ^ ST.to_string (Tuple inputs)

  let of_log evts log =
    let open CCOpt.Infix in
    let selector x = keccak (signature x) |> Hash256.of_binary in
    let sign = log.Log.topics.(0) in
    let evts = List.map (fun e -> (selector e, e)) evts in
    List.assoc_opt sign evts
    >|= fun {name; inputs; anonymous= _} ->
    let _, _, args =
      let nonIndexed =
        List.filter_map
          (fun x -> if x.indexed then None else Some x.t)
          (Array.to_list inputs) in
      let data = ST.Tuple nonIndexed in
      let indexedV =
        match (SV.decode data (Bitstring.bitstring_of_string log.data)).v with
        | SV.Tuple x -> x
        | _ -> assert false in
      Array.fold_left
        (fun (i, iv, a) {t; indexed; _} ->
          if indexed then
            (* TODO: support non immediate values. *)
            (succ i, iv, SV.decode t (Hash256.to_bitstring log.topics.(i)) :: a)
          else match iv with [] -> assert false | h :: t -> (i, t, h :: a))
        (0, indexedV, []) inputs in
    {name; args= List.rev args}
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

let create2 ~addr ~salt ~initCode =
  let open SV in
  tuple [nbytes "\xff"; address addr; nbytes salt; nbytes initCode]
  |> packed |> Bitstring.string_of_bitstring
  |> fun s -> String.sub (keccak s) 12 20

(* -------------------------------------------------------------------------------- *)

let to_0x b =
  let (`Hex res) = Hex.of_string (Bitstring.string_of_bitstring b) in
  "0x" ^ res
