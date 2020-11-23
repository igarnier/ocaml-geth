open Bitstring
open Types
module ST = SolidityTypes

type t = {t: SolidityTypes.t; v: value}

and value =
  | Int of
      (Z.t[@printer fun ppf v -> Format.pp_print_string ppf (Z.to_string v)])
  | Bool of bool
  | String of string
  | Address of Address.t
  | Tuple of t list
  | Func of {selector: string; address: Address.t}
[@@deriving show, eq]

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

let z pad x =
  let len = Z.numbits x in
  let padf =
    match Z.sign x with -1 -> ones_bitstring | _ -> zeroes_bitstring in
  assert (len < 257) ;
  let bs = padf len in
  for i = 0 to len - 1 do
    if Z.testbit x i then set bs i
  done ;
  match pad - len with x when x <= 0 -> bs | x -> concat [padf x; bs]

let rec encode x =
  match (x.v, x.t) with
  | Int v, ST.UInt _ when Z.sign v > 0 -> z 256 v
  | Int v, Int _ -> z 256 v
  | Bool true, Bool -> int 1
  | Bool false, Bool -> int 0
  | Address v, Address -> address v
  | String v, NBytes _ -> pack `Back v
  | String v, String | String v, Bytes ->
      Bitstring.concat [int (String.length v); pack `Back v]
  | Tuple values, Tuple typs ->
      let encode_heads x offset =
        if ST.is_dynamic x.t then int offset else encode x in
      let encode_tails x =
        if ST.is_dynamic x.t then encode x else zeroes_bitstring 0 in
      let header_size typs =
        let header_size_of_type typ =
          let open ST in
          match typ with
          | FArray (length, _) when not (ST.is_dynamic typ) -> 32 * length
          | Tuple typs when not (ST.is_dynamic typ) -> 32 * List.length typs
          | _ -> 32 in
        List.fold_left (fun acc typ -> acc + header_size_of_type typ) 0 typs
      in
      (* The types are implicitly contained in the values. *)
      (* compute size of header *)
      (* convert tail values to bitstrings (possibly empty if not dynamic) *)
      let tails = List.map encode_tails values in
      (* for each value, compute where its dynamic data is stored as an offset,
         taking into account header size. *)
      let _, offsets =
        List.fold_left
          (fun (offset, acc) bitstr ->
            (offset + (bitstring_length bitstr / 8), offset :: acc))
          (header_size typs, [])
          tails in
      let offsets = List.rev offsets in
      let heads = List.map2 encode_heads values offsets in
      concat (heads @ tails)
  | _, FArray (n, t) -> encode {x with t= Tuple (List.init n (fun _ -> t))}
  | Tuple vs, VArray t ->
      let n = List.length vs in
      concat [int n; encode {x with t= Tuple (List.init n (fun _ -> t))}]
  | _ -> invalid_arg "encode"

let rec packed x =
  match (x.v, x.t) with
  | Int v, ST.UInt n when Z.sign v > 0 -> z n v
  | Int v, Int n -> z n v
  | Bool true, Bool -> Bitstring.ones_bitstring 1
  | Bool false, Bool -> Bitstring.zeroes_bitstring 1
  | Address v, Address -> Bitstring.bitstring_of_string (v :> string)
  | String v, NBytes _ -> Bitstring.bitstring_of_string v
  | String v, String | String v, Bytes -> Bitstring.bitstring_of_string v
  | Tuple values, Tuple _typs -> concat (List.map packed values)
  | _, FArray (n, t) -> packed {x with t= Tuple (List.init n (fun _ -> t))}
  | Tuple vs, VArray t ->
      let n = List.length vs in
      concat [int n; packed {x with t= Tuple (List.init n (fun _ -> t))}]
  | _ -> invalid_arg "packed"

(* -------------------------------------------------------------------------------- *)
(* Convenience functions to create ABI values *)

let notstring =
  CCString.map (fun c ->
      CCChar.to_int c |> lnot |> Int.logand 0xff |> CCChar.of_int_exn)

let unsigned b = string_of_bitstring b |> CCString.rev |> Z.of_bits

let signed b =
  match Bitstring.get b 0 with
  | 0 -> unsigned b
  | _ ->
      string_of_bitstring b |> CCString.rev |> notstring |> Z.of_bits
      |> Z.add Z.one |> Z.neg

let int w z = {v= Int z; t= ST.int w}
let uint w z = {v= Int z; t= ST.uint w}
let uint256 z = {v= Int z; t= ST.uint 256}
let string v = {v= String v; t= ST.string}
let bytes v = {v= String v; t= ST.bytes}
let nbytes v = {v= String v; t= ST.NBytes (String.length v)}
let bool v = {v= Bool v; t= ST.Bool}
let address v = {v= Address v; t= ST.address}
let tuple vals = {v= Tuple vals; t= ST.Tuple (List.map (fun v -> v.t) vals)}
let farray vals t = {v= Tuple vals; t= ST.FArray (List.length vals, t)}
let varray vals t = {v= Tuple vals; t= ST.VArray t}

let rec decode t b =
  (* Printf.eprintf "decoding %s with data %s\n" (ST.print t) (Bitstr.Hex.as_string (Bitstr.uncompress b)); *)
  match (t : ST.t) with
  | UInt w -> uint w (unsigned b)
  | Int w -> int w (signed b)
  | Address ->
      address (Address.of_binary (subbitstring b 12 20 |> string_of_bitstring))
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
      let address = takebits 160 b |> string_of_bitstring |> Address.of_binary in
      let selector = takebits 32 (dropbits 160 b) |> string_of_bitstring in
      {v= Func {selector; address}; t= ST.Function}
  | FArray (n, t) -> farray (decode_tuple b (List.init n (fun _ -> t))) t
  | VArray t ->
      let n = takebits 256 b |> unsigned |> Z.to_int in
      let b = dropbits 256 b in
      varray (decode_tuple b (List.init n (fun _ -> t))) t
  | Tuple typs -> tuple (decode_tuple b typs)

and decode_tuple b typs =
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
          (rem, decode ty tail :: values)
        else (rem, decode ty chunk :: values))
      (b, []) typs in
  List.rev values
