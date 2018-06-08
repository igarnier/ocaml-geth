open Batteries
    
(* Our minimal bitstring *)
type bitstring = string
type hexstring = string

let char_is_hex = function
  | '0' .. '9'
  | 'A' .. 'F'
  | 'a' .. 'f' -> true
  | _ -> false

let string_is_hex (x : string) =
  let rec loop i len x acc =
    if i = len then
      acc
    else
      loop (i+1) len x (acc && char_is_hex x.[i])
  in
  x.[0] = '0'
  && (x.[1] = 'x' || x.[1] = 'X')
  && (loop 2 (String.length x) x true)

let compress : hexstring -> bitstring =
  fun (x : hexstring) ->
    assert (string_is_hex x);
    let stripped = String.tail x 2 in
    Hex.to_string (`Hex stripped)

let uncompress : bitstring -> hexstring =
  fun (x : bitstring) ->
    let `Hex result = Hex.of_string x in
    "0x"^result

let hex_of_int i =
  Printf.sprintf "0x%x" i

let hex_of_char c =
  hex_of_int (Char.code c)

let hex_of_string string =
  if not (string_is_hex string) then
    failwith "hex_of_string: "^string^" not a hex string"
  else
    string

let hex_of_int64 (i : int64) =
  Printf.sprintf "0x%Lx" i

let hex_as_string x = x

let bits_of_char c =
  compress (hex_of_char c)

let bits_of_int64 i =
  compress (hex_of_int64 i)

let bits_of_string (s : string) =
  (s : bitstring)

let bit_length (x : bitstring) =
  (String.length x) * 8

let zero_padding ~(bits : bitstring) ~(zeroes : int) =
  if zeroes mod 8 != 0 then
    failwith "Bitstr.bitstr: error, can only pad modulo 8"
  else
    let zero_chars_num = zeroes / 8 in
    let padding = String.make zero_chars_num '\x00' in
    padding^bits

let one_padding ~(bits : bitstring) ~(ones : int) =
  if ones mod 8 != 0 then
    failwith "Bitstr.bitstr: error, can only pad modulo 8"
  else
    let ff_chars_num = ones / 8 in
    let padding = String.make ff_chars_num '\xff' in
    padding^bits

let zero_pad_to ~(bits : bitstring) ~(target_bits : int) =
  let len = bit_length bits in
  if len >= target_bits then
    bits
  else
    zero_padding ~bits ~zeroes:(target_bits - len)

let one_pad_to ~(bits : bitstring) ~(target_bits : int) =
  let len = bit_length bits in
  if len >= target_bits then
    bits
  else
    one_padding ~bits ~ones:(target_bits - len)

let concat (l : bitstring list) =
  String.concat "" l
