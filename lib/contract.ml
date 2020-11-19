open Basic

(* https://solidity.readthedocs.io/en/develop/abi-spec.html *)

module SolidityTypes = struct
  type atom =
    | UInt of int
    | Int of int
    | Address
    | Bool
    | Fixed of int * int
    | UFixed of int * int
    | NBytes of int
    | Bytes
    | String
    | Function

  and t = Atom of atom | SArray of int * t | DArray of t | Tuple of t list
  [@@deriving eq]

  let atom x = Atom x
  let uint w = Atom (UInt w)
  let int w = Atom (Int w)
  let string = Atom String
  let bytes = Atom Bytes
  let address = Atom Address

  let rec is_dynamic = function
    | Atom Bytes | Atom String | DArray _ -> true
    | Tuple typs -> List.exists is_dynamic typs
    | SArray (_, typ) when is_dynamic typ -> true
    | _ -> false

  let string_of_atom = function
    | UInt 256 -> "uint"
    | Int 256 -> "int"
    | UInt w -> "uint" ^ string_of_int w
    | Int w -> "int" ^ string_of_int w
    | Address -> "address"
    | Bool -> "bool"
    | Fixed (128, 18) -> "fixed"
    | UFixed (128, 18) -> "ufixed"
    | Fixed (m, n) -> "fixed" ^ string_of_int m ^ "x" ^ string_of_int n
    | UFixed (m, n) -> "ufixed" ^ string_of_int m ^ "x" ^ string_of_int n
    | NBytes n -> "bytes" ^ string_of_int n
    | Bytes -> "bytes"
    | String -> "string"
    | Function -> "function"

  module Parser = struct
    open Angstrom

    let num =
      take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

    let mxn = num >>= fun m -> char 'x' *> num >>| fun n -> (m, n)
    let numopt = option None (num >>| Option.some)

    let int =
      option false (char 'u' >>| fun _ -> true)
      >>= fun unsigned ->
      string "int" *> option 256 num
      >>| fun n -> if unsigned then UInt n else Int n

    let fixed =
      option false (char 'u' >>| fun _ -> true)
      >>= fun unsigned ->
      string "fixed" *> option (128, 18) mxn
      >>| fun (m, n) -> if unsigned then UFixed (m, n) else Fixed (m, n)

    let bytes = string "bytes" *> option Bytes (num >>| fun n -> NBytes n)
    let addr = string "address" >>| fun _ -> Address
    let bool = string "bool" >>| fun _ -> Bool
    let str = string "string" >>| fun _ -> String
    let func = string "function" >>| fun _ -> Function

    let tl =
      string "[]"
      >>| (fun _ -> `Dynamic)
      <|> (char '[' *> num <* char ']' >>| fun n -> `Static n)

    let atm = map ~f:atom (choice [int; fixed; bytes; addr; bool; str; func])

    let tpl expr =
      char '(' *> sep_by (char ',') expr <* char ')' >>| fun x -> Tuple x

    let expr expr =
      atm <|> tpl expr
      >>= fun hd ->
      many tl
      >>| List.fold_left
            (fun a -> function `Static n -> SArray (n, a)
              | `Dynamic -> DArray a)
            hd

    let t = fix expr
  end

  let of_string = Angstrom.parse_string ~consume:All Parser.t

  let of_string_exn s =
    match of_string s with Ok x -> x | Error err -> failwith err

  let rec to_string = function
    | Atom atom -> string_of_atom atom
    | SArray (length, typ) -> to_string typ ^ "[" ^ string_of_int length ^ "]"
    | DArray typ -> to_string typ ^ "[]"
    | Tuple types -> "(" ^ String.concat "," (List.map to_string types) ^ ")"

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module ABI = struct
  type value = {desc: value_desc; typ: SolidityTypes.t}

  and value_desc =
    | Int of int64
    | BigInt of Z.t
    | Bool of bool
    | String of string
    | Address of Types.Address.t
    | Tuple of value list
    | Func of {selector: string; address: Bitstr.Hex.t}

  type event = {event_name: string; event_args: value list}

  open Json_encoding

  type named = {name: string; t: SolidityTypes.t; indexed: bool}

  let of_named {t; _} = t

  let extended =
    let open SolidityTypes in
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

  let is_constructor = function
    | {Fun.kind= Constructor; _} -> true
    | _ -> false

  let encoding =
    union
      [ case Fun.encoding
          (function Fun x -> Some x | _ -> None)
          (fun x -> Fun x);
        case Evt.encoding
          (function Event x -> Some x | _ -> None)
          (fun x -> Event x) ]

  (* -------------------------------------------------------------------------------- *)
  (* Convenience functions to create ABI values *)

  let type_of (v : value) = v.typ
  let int256_val (v : int64) = {desc= Int v; typ= SolidityTypes.int 256}
  let uint256_val (v : int64) = {desc= Int v; typ= SolidityTypes.uint 256}
  let string_val (v : string) = {desc= String v; typ= SolidityTypes.string}
  let bytes_val (v : string) = {desc= String v; typ= SolidityTypes.bytes}
  let bool_val (v : bool) = {desc= Bool v; typ= SolidityTypes.address}

  let address_val (v : Types.Address.t) =
    {desc= Address v; typ= SolidityTypes.address}

  let tuple_val vals =
    {desc= Tuple vals; typ= SolidityTypes.Tuple (List.map type_of vals)}

  let static_array_val vals typ =
    {desc= Tuple vals; typ= SolidityTypes.SArray (List.length vals, typ)}

  let dynamic_array_val vals typ =
    {desc= Tuple vals; typ= SolidityTypes.DArray typ}

  (* -------------------------------------------------------------------------------- *)
  (* Computing function selectors *)

  let string_of_signature name inputs =
    let encodings = Array.map (fun {t; _} -> SolidityTypes.to_string t) inputs in
    let elts = String.concat "," (Array.to_list encodings) in
    name ^ "(" ^ elts ^ ")"

  let keccak str =
    let hash = Cryptokit.Hash.keccak 256 in
    let resl = Cryptokit.hash_string hash str in
    Bitstr.Bit.of_string resl

  let keccak_4_bytes str =
    let hash = Cryptokit.Hash.keccak 256 in
    let resl = Cryptokit.hash_string hash str in
    let head = String.sub resl 0 4 in
    Bitstr.Bit.of_string head

  let method_id {Fun.name; inputs; _} =
    keccak_4_bytes (string_of_signature name inputs)

  let event_id {Evt.name; inputs; anonymous= _} =
    keccak (string_of_signature name inputs)

  (* -------------------------------------------------------------------------------- *)

  let header_size_of_type typ =
    let open SolidityTypes in
    match typ with
    | SArray (length, _) when not (SolidityTypes.is_dynamic typ) -> 32 * length
    | Tuple typs when not (SolidityTypes.is_dynamic typ) ->
        32 * List.length typs
    | _ -> 32

  let header_size typs =
    let sum =
      List.fold_left (fun acc typ -> acc + header_size_of_type typ) 0 typs in
    Bytes.int sum

  module Encode = struct
    let zero_pad_string_to_mod32 s =
      let len = String.length s in
      let result =
        if len = 0 then String.make 32 '\000'
        else
          let rem = len mod 32 in
          if rem = 0 then s
          else
            let pad = 32 - rem in
            s ^ String.make pad '\000' in
      Bitstr.Bit.of_string result

    (* Encoding of values *)
    let int64_as_uint256 (i : int64) =
      if i < 0L then
        failwith "encode_int: cannot encode negative integer as unsigned int"
      else Bitstr.Bit.of_bigint 256 (Z.of_int64 i)

    let int64_as_int256 (i : int64) = Bitstr.Bit.of_bigint 256 (Z.of_int64 i)

    let address (s : Types.Address.t) =
      let encoded = Bitstr.compress s in
      Bitstr.Bit.zero_pad_to ~dir:`left ~bits:encoded
        ~target_bits:(Bits.int 256)

    let bytes_static s n =
      if String.length s < n then invalid_arg "bytes_static" ;
      zero_pad_string_to_mod32 (String.sub s 0 n)

    let bytes_dynamic s =
      let len = String.length s in
      let elen = int64_as_uint256 (Int64.of_int len) in
      (* Printf.printf "debug: encoding string of length %d: encoding of length=%s, string = %s\n"
       *   len
       *   Bitstr.(Hex.as_string (uncompress elen))
       *   Bitstr.(Hex.as_string (uncompress (zero_pad_string_to_mod32 s)))
       * ; *)
      Bitstr.Bit.concat [elen; zero_pad_string_to_mod32 s]

    let rec encode {typ; desc} =
      match (desc, typ) with
      | Int v, Atom (UInt _) -> int64_as_uint256 v
      | Int v, Atom (Int _) -> int64_as_int256 v
      | Bool v, Atom Bool ->
          let i = if v then 1L else 0L in
          int64_as_uint256 i
      | Address v, Atom Address -> address v
      | String v, Atom (NBytes n) -> bytes_static v n
      | String v, Atom String | String v, Atom Bytes -> bytes_dynamic v
      | Tuple values, Tuple typs ->
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
                let byte_len = bits_to_bytes (Bitstr.Bit.length bitstr) in
                let next_offset = Bytes.(offset + byte_len) in
                (next_offset, offset :: acc))
              (headsz, []) tails in
          let offsets = List.rev offsets in
          let heads = List.map2 encode_heads values offsets in
          Bitstr.Bit.concat (heads @ tails)
      | _ ->
          (* TODO: static/dynamic arrays *)
          failwith "encode: error"

    and encode_heads (value : value) (offset : Bytes.t) =
      if not (SolidityTypes.is_dynamic (type_of value)) then encode value
      else int64_as_uint256 (Int64.of_int (Bytes.to_int offset))

    and encode_tails (value : value) =
      if not (SolidityTypes.is_dynamic (type_of value)) then
        Bitstr.Bit.of_string ""
      else encode value
  end

  module Decode = struct
    let rec decode b t =
      (* Printf.eprintf "decoding %s with data %s\n" (SolidityTypes.print t) (Bitstr.Hex.as_string (Bitstr.uncompress b)); *)
      let open SolidityTypes in
      match t with
      | Atom at -> decode_atomic b at
      | SArray (length, typ) -> decode_static_array b length typ
      | DArray typ -> decode_dynamic_array b typ
      | Tuple typs -> tuple_val (decode_tuple b typs)

    and decode_atomic b at =
      (* Printf.eprintf "decoding atomic %s\n" (SolidityTypes.print (SolidityTypes.Tatomic at));       *)
      match at with
      | UInt w -> decode_uint b w
      | Int w -> decode_int b w
      | Address -> address_val (decode_address b)
      | Bool ->
          let z = Bitstr.Bit.to_unsigned_bigint b in
          bool_val (Z.gt z Z.zero)
      | Fixed _ | UFixed _ ->
          failwith "decode_atomic: fixed point numbers not handled yet"
      | NBytes n ->
          let bytes, _ = Bitstr.Bit.take_int b (n * 8) in
          bytes_val (Bitstr.Bit.to_string bytes)
      | Bytes ->
          let len, rem = Bitstr.Bit.take b (Bits.int 256) in
          let len = Z.to_int (Bitstr.Bit.to_unsigned_bigint len) in
          let bytes, _ = Bitstr.Bit.take rem (bytes_to_bits (Bytes.int len)) in
          bytes_val (Bitstr.Bit.to_string bytes)
      | String ->
          let len, rem = Bitstr.Bit.take b (Bits.int 256) in
          let len = Z.to_int (Bitstr.Bit.to_unsigned_bigint len) in
          let bytes, _ = Bitstr.Bit.take rem (bytes_to_bits (Bytes.int len)) in
          string_val (Bitstr.Bit.to_string bytes)
      | Function -> decode_function b

    and decode_address b =
      let addr, _ = Bitstr.Bit.take b (Bits.int 160) in
      Bitstr.uncompress addr

    and decode_function b =
      let content, _ = Bitstr.Bit.take b (Bits.int (160 + 32)) in
      let address, selector = Bitstr.Bit.take content (Bits.int 160) in
      let address = decode_address address in
      let selector = Bitstr.Bit.to_string selector in
      {desc= Func {selector; address}; typ= SolidityTypes.(Atom Function)}

    and decode_static_array b length t =
      static_array_val (decode_tuple b (List.init length (fun _ -> t))) t

    and decode_dynamic_array b t =
      let length, _content = Bitstr.Bit.take b (Bits.int 256) in
      let length = Z.to_int (Bitstr.Bit.to_unsigned_bigint length) in
      dynamic_array_val (decode_tuple b (List.init length (fun _ -> t))) t

    and decode_tuple b typs =
      (* Printf.eprintf "decoding tuple %s with data = %s\n"  *)
      (*   (SolidityTypes.print (SolidityTypes.Ttuple typs))
       *   (Bitstr.Hex.to_string (Bitstr.uncompress b))
       * ; *)
      let _, values =
        List.fold_left
          (fun (header_chunk, values) ty ->
            let chunk, rem = Bitstr.Bit.take header_chunk (Bits.int 256) in
            if SolidityTypes.is_dynamic ty then
              let offset = Bitstr.Bit.to_unsigned_bigint chunk in
              let offset = Z.to_int offset in
              (* offsets are computed starting from the beginning of [b] *)
              let _, tail =
                Bitstr.Bit.take b (bytes_to_bits (Bytes.int offset)) in
              let value = decode tail ty in
              (rem, value :: values)
            else
              let value = decode chunk ty in
              (rem, value :: values))
          (b, []) typs in
      List.rev values

    and decode_uint (b : Bitstr.Bit.t) w =
      (* Printf.eprintf "decode_uint: %s\n" (Bitstr.Hex.to_string (Bitstr.uncompress b)); *)
      let z = Bitstr.Bit.to_unsigned_bigint b in
      if Z.fits_int64 z then
        {desc= Int (Z.to_int64 z); typ= SolidityTypes.uint w}
      else {desc= BigInt z; typ= SolidityTypes.uint w}

    and decode_int b w =
      (* Printf.eprintf "decode_int: %s\n" (Bitstr.Hex.to_string (Bitstr.uncompress b)); *)
      let z = Bitstr.Bit.to_signed_bigint b in
      if Z.fits_int64 z then
        {desc= Int (Z.to_int64 z); typ= SolidityTypes.uint w}
      else {desc= BigInt z; typ= SolidityTypes.uint w}

    let decode_events abis receipt =
      let open Types.Tx in
      let codes =
        List.fold_left
          (fun acc abi ->
            match abi with
            | Event event_abi ->
                let id = Bitstr.uncompress (event_id event_abi) in
                (id, event_abi) :: acc
            | _ -> acc)
          [] abis in
      List.fold_left
        (fun acc log ->
          let topics = log.Log.topics in
          let data = log.data in
          (* Check whether /at most one/ topic corresponds to an event *)
          let relevant =
            List.filter (fun hash -> List.mem_assoc hash codes) topics in
          let event =
            match relevant with
            | [] | _ :: _ :: _ ->
                failwith "0 or > 1 matching event for topic, aborting"
            | [hash] -> List.assoc hash codes in
          let types = event.inputs in
          let fields =
            match
              decode
                (Bitstr.compress (Bitstr.Hex.of_string data))
                (Tuple (List.map (fun {t; _} -> t) (Array.to_list types)))
            with
            | {desc= Tuple values; _} -> values
            | exception _ -> failwith "decode_events: error while decoding"
            | _ -> failwith "decode_events: bug found" in
          {event_name= event.name; event_args= fields} :: acc)
        [] receipt.logs
  end
end

open Json_encoding

type t = {contracts: (string * contract) list; version: string}

and contract = {abi: ABI.t list; bin: Bitstring.t}

let find_function {abi; _} name =
  List.find_map
    (function ABI.Fun x when String.equal x.name name -> Some x | _ -> None)
    abi

let hex = conv Bitstr.Bit.to_string Bitstr.Bit.of_string string

module X = Json_encoding.Make (Json_repr.Yojson)

let contract e =
  conv
    (fun {abi; bin} -> ((), (abi, bin)))
    (fun ((), (abi, bin)) -> {abi; bin})
    (merge_objs unit
       (obj2 (req "abi" e) (dft "bin" hex Bitstring.empty_bitstring)))

let simple = contract (list ABI.encoding)

let combined =
  let abis =
    conv
      (fun _ -> assert false)
      (fun x -> X.destruct (list ABI.encoding) (Yojson.Safe.from_string x))
      string in
  conv
    (fun {contracts; version} -> (contracts, version))
    (fun (contracts, version) -> {contracts; version})
    (obj2 (req "contracts" (assoc (contract abis))) (dft "version" string ""))
