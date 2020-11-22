open Types
module ST = SolidityTypes

type value = {desc: desc; typ: ST.t}

and desc =
  | Int of Z.t
  | Bool of bool
  | String of string
  | Address of Address.t
  | Tuple of value list
  | Func of {selector: string; address: Address.t}

type event = {name: string; args: value list}

open Json_encoding

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
        {name; t= DArray (Tuple (List.map of_named components)); indexed}
    | `Tups (Some n) ->
        {name; t= SArray (n, Tuple (List.map of_named components)); indexed}
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
  let encodings = Array.map (fun {t; _} -> ST.to_string t) inputs in
  let elts = String.concat "," (Array.to_list encodings) in
  name ^ "(" ^ elts ^ ")"

let keccak str =
  let hash = Cryptokit.Hash.keccak 256 in
  let resl = Cryptokit.hash_string hash str in
  Bitstring.bitstring_of_string resl

let keccak_4_bytes str =
  let hash = Cryptokit.Hash.keccak 256 in
  let resl = Cryptokit.hash_string hash str in
  let head = String.sub resl 0 4 in
  Bitstring.bitstring_of_string head

let method_id {Fun.name; inputs; _} =
  keccak_4_bytes (string_of_signature name inputs)

let event_id {Evt.name; inputs; anonymous= _} =
  keccak (string_of_signature name inputs)

(* -------------------------------------------------------------------------------- *)

let header_size_of_type typ =
  let open ST in
  match typ with
  | SArray (length, _) when not (ST.is_dynamic typ) -> 32 * length
  | Tuple typs when not (ST.is_dynamic typ) -> 32 * List.length typs
  | _ -> 32

let header_size typs =
  List.fold_left (fun acc typ -> acc + header_size_of_type typ) 0 typs

module Encode = struct
  open Bitstring

  let pack where s =
    match String.length s with
    | 0 -> zeroes_bitstring 32
    | len -> (
      match len mod 32 with
      | 0 -> bitstring_of_string s
      | rem -> (
          let pad = 32 - rem in
          match where with
          | `Front -> concat [zeroes_bitstring pad; bitstring_of_string s]
          | `Back -> concat [bitstring_of_string s; zeroes_bitstring pad] ) )

  let address (s : Address.t) =
    String.init 32 (fun i -> if i < 12 then '\x00' else (s :> string).[i - 12])
    |> bitstring_of_string

  let int x =
    let%bitstring x = {|x: 31|} in
    let pad = zeroes_bitstring (256 - bitstring_length x) in
    concat [pad; x]

  let z x =
    let len = Z.numbits x in
    assert (len < 257) ;
    let bs = Bitstring.zeroes_bitstring len in
    for i = 0 to len - 1 do
      if Z.testbit x i then Bitstring.set bs i
    done ;
    let pad = 256 - len in
    concat [zeroes_bitstring pad; bs]

  let rec encode {typ; desc} =
    match (desc, typ) with
    | Int v, UInt _ when Z.sign v > 0 -> z v
    | Int v, Int _ -> z v
    | Bool true, Bool -> int 1
    | Bool false, Bool -> int 0
    | Address v, Address -> address v
    | String v, NBytes _ -> pack `Back v
    | String v, String | String v, Bytes ->
        Bitstring.concat [int (String.length v); pack `Back v]
    | Tuple values, Tuple typs ->
        let encode_heads v offset =
          if not (ST.is_dynamic v.typ) then encode v else int offset in
        let encode_tails v =
          if not (ST.is_dynamic v.typ) then zeroes_bitstring 0 else encode v
        in
        (* The types are implicitly contained in the values. *)
        (* compute size of header *)
        let headsz = header_size typs in
        (* convert tail values to bitstrings (possibly empty if not dynamic) *)
        let tails = List.map encode_tails values in
        (* for each value, compute where its dynamic data is stored as an offset,
           taking into account header size. *)
        let _, offsets =
          List.fold_left
            (fun (offset, acc) bitstr ->
              let byte_len = bitstring_length bitstr / 8 in
              let next_offset = offset + byte_len in
              (next_offset, offset :: acc))
            (headsz, []) tails in
        let offsets = List.rev offsets in
        let heads = List.map2 encode_heads values offsets in
        concat (heads @ tails)
    | _ ->
        (* TODO: static/dynamic arrays *)
        failwith "encode: error"
end

(* -------------------------------------------------------------------------------- *)
(* Convenience functions to create ABI values *)

let int w z = {desc= Int z; typ= ST.int w}
let uint w z = {desc= Int z; typ= ST.uint w}
let uint256 z = {desc= Int z; typ= ST.uint 256}
let string v = {desc= String v; typ= ST.string}
let bytes v = {desc= String v; typ= ST.bytes}
let bool v = {desc= Bool v; typ= ST.Bool}
let address v = {desc= Address v; typ= ST.address}

let tuple vals =
  {desc= Tuple vals; typ= ST.Tuple (List.map (fun v -> v.typ) vals)}

let static_array vals typ =
  {desc= Tuple vals; typ= ST.SArray (List.length vals, typ)}

let dynamic_array vals typ = {desc= Tuple vals; typ= ST.DArray typ}

let to_0x b =
  let (`Hex res) = Hex.of_string (Bitstring.string_of_bitstring b) in
  "0x" ^ res

module Decode = struct
  open Bitstring

  let notstring =
    CCString.map (fun c ->
        CCChar.to_int c |> lnot |> Int.logand 0xff |> CCChar.of_int_exn)

  let unsigned b = string_of_bitstring b |> CCString.rev |> Z.of_bits

  let signed b =
    string_of_bitstring b |> CCString.rev |> notstring |> Z.of_bits
    |> Z.add Z.one |> Z.neg

  let rec decode b t =
    (* Printf.eprintf "decoding %s with data %s\n" (ST.print t) (Bitstr.Hex.as_string (Bitstr.uncompress b)); *)
    match (t : ST.t) with
    | UInt w -> unsigned b |> uint w
    | Int w -> signed b |> int w
    | Address ->
        address
          (Address.of_binary (subbitstring b 12 20 |> string_of_bitstring))
    | Bool -> bool (get b 255 <> 0)
    | Fixed _ | UFixed _ ->
        failwith "decode_atomic: fixed point numbers not handled yet"
    | NBytes n -> bytes (String.sub (string_of_bitstring b) 0 n)
    | Bytes ->
        let n = takebits 256 b |> unsigned |> Z.to_int in
        let b = dropbits 256 b in
        bytes (String.sub (string_of_bitstring b) 0 n)
    | String ->
        let n = takebits 256 b |> unsigned |> Z.to_int in
        let b = dropbits 256 b in
        string (String.sub (string_of_bitstring b) 0 n)
    | Function ->
        let address =
          takebits 160 b |> string_of_bitstring |> Address.of_binary in
        let selector = takebits 32 (dropbits 160 b) |> string_of_bitstring in
        {desc= Func {selector; address}; typ= ST.Function}
    | SArray (n, t) ->
        static_array (decode_tuple b (List.init n (fun _ -> t))) t
    | DArray t ->
        let n = takebits 256 b |> unsigned |> Z.to_int in
        let b = dropbits 256 b in
        dynamic_array (decode_tuple b (List.init n (fun _ -> t))) t
    | Tuple typs -> tuple (decode_tuple b typs)

  and decode_tuple b typs =
    (* Printf.eprintf "decoding tuple %s with data = %s\n"  *)
    (*   (ST.print (ST.Ttuple typs))
     *   (Bitstr.Hex.to_string (Bitstr.uncompress b))
     * ; *)
    let _, values =
      List.fold_left
        (fun (header_chunk, values) ty ->
          let chunk = takebits 256 header_chunk in
          let rem = dropbits 256 header_chunk in
          if ST.is_dynamic ty then
            let offset = unsigned chunk in
            let offset = Z.to_int offset in
            (* offsets are computed starting from the beginning of [b] *)
            let tail = dropbits (offset * 8) b in
            let value = decode tail ty in
            (rem, value :: values)
          else
            let value = decode chunk ty in
            (rem, value :: values))
        (b, []) typs in
    List.rev values

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
      | [] | _ :: _ :: _ ->
          failwith "0 or > 1 matching event for topic, aborting"
      | [hash] -> List.assoc hash codes in
    let types = event.inputs in
    let fields =
      match
        decode
          (Bitstring.bitstring_of_string data)
          (Tuple (List.map (fun {t; _} -> t) (Array.to_list types)))
      with
      | {desc= Tuple values; _} -> values
      | exception _ -> failwith "decode_events: error while decoding"
      | _ -> failwith "decode_events: bug found" in
    {name= event.name; args= fields}
end
