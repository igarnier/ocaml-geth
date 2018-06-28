open Batteries
open Basic

(* This is an external library whose name clashes with a module defined here. *)
module ExtHx = Hex

module Hex =
struct

  type t = string

  let char_is_hex = function
    | '0' .. '9'
    | 'A' .. 'F'
    | 'a' .. 'f' -> true
    | _ -> false

  let is_hex (x : string) =
    let rec loop i len x acc =
      if i = len then
        acc
      else
        loop (i+1) len x (acc && char_is_hex x.[i])
    in
    x.[0] = '0'
    && (x.[1] = 'x' || x.[1] = 'X')
    && (loop 2 (String.length x) x true)


  let of_int i =
    let hexstr = 
      Printf.sprintf "%x" i
    in
    if String.length hexstr mod 2 = 0 then
      "0x"^hexstr
    else
      "0x0"^hexstr

  let of_bigint (z : Z.t) =
    let hexstr = Z.format "%x" z in
    if String.length hexstr mod 2 = 0 then
      "0x"^hexstr
    else
      "0x0"^hexstr

  let of_char c =
    of_int (Char.code c)

  let of_string string =
    if not (is_hex string) then
      failwith "hex_of_string: "^string^" not a hex string"
    else
      string

  let of_int64 (i : int64) =
    let hexstr = 
      Printf.sprintf "%Lx" i
    in
    if String.length hexstr mod 2 = 0 then
      "0x"^hexstr
    else
      "0x0"^hexstr

  let as_string x = x

  let length (x : t) =
    Bytes.int ((String.length x - 2) / 2)

end


let compress_ (x : Hex.t) : string =
  assert (Hex.is_hex x);
  let stripped = String.tail x 2 in
  try ExtHx.to_string (`Hex stripped)
  with Invalid_argument s ->
    raise (Invalid_argument (s^": "^x))


module Bit =
struct

  type t = string

  let of_char c =
    compress_ (Hex.of_char c)

  let of_int64 i =
    compress_ (Hex.of_int64 i)

  let of_string (s : string) =
    (s : t)

  let as_string (s : t) =
    (s : string)

  type pad_direction = [ `left | `right ]

  let length (x : t) =
    Bits.int ((String.length x) * 8)

  let zero_padding ~(dir:pad_direction) ~(bits : t) ~(zeroes : Bits.t) =
    let zeroes = Bits.to_int zeroes in
    if zeroes mod 8 != 0 then
      failwith "Bitstr.Bit.zero_padding: error, can only pad modulo 8"
    else
      let zero_chars_num = zeroes / 8 in
      let padding = String.make zero_chars_num '\x00' in
      match dir with
      | `left ->
        padding^bits
      | `right ->
        bits^padding

  let one_padding ~(dir:pad_direction) ~(bits : t) ~(ones : Bits.t) =
    let ones = Bits.to_int ones in
    if ones mod 8 != 0 then
      failwith "Bitstr.Bit.one_padding: error, can only pad modulo 8"
    else
      let ff_chars_num = ones / 8 in
      let padding = String.make ff_chars_num '\xff' in
      match dir with
      | `left ->
        padding^bits
      | `right ->
        bits^padding

  let zero_pad_to ~dir ~(bits : t) ~(target_bits : Bits.t) =
    let target_bits = Bits.to_int target_bits in
    let len = Bits.to_int (length bits) in
    if len >= target_bits then
      bits
    else
      zero_padding ~dir ~bits ~zeroes:(Bits.int (target_bits - len))

  let one_pad_to ~dir ~(bits : t) ~(target_bits : Bits.t) =
    let target_bits = Bits.to_int target_bits in
    let len = Bits.to_int (length bits) in
    if len >= target_bits then
      bits
    else
      one_padding ~dir ~bits ~ones:(Bits.int (target_bits - len))

  let concat (l : t list) =
    String.concat "" l

  let take (bits : t) (n : Bits.t) =
    if Bits.to_int n mod 8 <> 0 then
      failwith "Bit.take: can only take bits modulo 8";
    let bytes = Bytes.to_int (bits_to_bytes n) in
    let head  = String.head bits bytes in
    let tail  = String.tail bits bytes in
    (head, tail)

end

let compress (x : Hex.t) : Bit.t =
  compress_ x

let uncompress (x : Bit.t) : Hex.t =
  let `Hex result = ExtHx.of_string x in
  "0x"^result
