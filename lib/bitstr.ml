open Basic

type t = Bitstring.bitstring
type pad_direction = [`left | `right]

let of_char c =
  let i = Char.code c in
  let%bitstring x = {| i : 8 |} in
  x

let of_int64 i =
  let%bitstring x = {| i : 64 |} in
  x

let of_bigint numbits z =
  if Z.numbits z > numbits then
    failwith "of_bigint: bigint too big to fit on prescribed # of bits" ;
  let bs = Bitstring.create_bitstring numbits in
  for i = 0 to numbits - 1 do
    if Z.testbit z i then Bitstring.set bs (numbits - i - 1)
  done ;
  bs

let to_signed_bigint s =
  if Bitstring.is_set s 0 then
    (* let s = copy s in *)
    (* Bitstring.clear s 0; *)
    let str = Bitstring.string_of_bitstring s in
    (* Apply not to every char *)
    let str =
      String.map (fun c -> Char.chr (lnot (Char.code c) land 0xff)) str in
    let str = CCString.rev str in
    (* Z.of_bits str *)
    Z.(neg (add (of_bits str) one))
  else
    (* Positive number. Reving the string for endianness purposes. *)
    Z.of_bits (CCString.rev (Bitstring.string_of_bitstring s))

let to_unsigned_bigint s =
  Z.of_bits (CCString.rev (Bitstring.string_of_bitstring s))

let of_string = Bitstring.bitstring_of_string
let to_string = Bitstring.string_of_bitstring
let length s = Bits.int (Bitstring.bitstring_length s)

let zero_padding ~(dir : pad_direction) ~bits ~(zeroes : Bits.t) =
  let zeroes = Bits.to_int zeroes in
  if zeroes mod 8 <> 0 then
    failwith "Bitstr.Bit.zero_padding: error, can only pad modulo 8"
  else
    (* let zero_chars_num = zeroes / 8 in
     * let padding = String.make zero_chars_num '\x00' in *)
    let padding = Bitstring.zeroes_bitstring zeroes in
    match dir with
    | `left -> Bitstring.concat [padding; bits]
    | `right -> Bitstring.concat [bits; padding]

let one_padding ~(dir : pad_direction) ~bits ~(ones : Bits.t) =
  let ones = Bits.to_int ones in
  if ones mod 8 <> 0 then
    failwith "Bitstr.Bit.one_padding: error, can only pad modulo 8"
  else
    (* let one_chars_num = ones / 8 in
     * let padding = String.make one_chars_num '\xff' in *)
    let padding = Bitstring.ones_bitstring ones in
    match dir with
    | `left -> Bitstring.concat [padding; bits]
    | `right -> Bitstring.concat [bits; padding]

let zero_pad_to ~dir ~(bits : t) ~(target_bits : Bits.t) =
  let target_bits = Bits.to_int target_bits in
  let len = Bits.to_int (length bits) in
  if len >= target_bits then bits
  else zero_padding ~dir ~bits ~zeroes:(Bits.int (target_bits - len))

let one_pad_to ~dir ~(bits : t) ~(target_bits : Bits.t) =
  let target_bits = Bits.to_int target_bits in
  let len = Bits.to_int (length bits) in
  if len >= target_bits then bits
  else one_padding ~dir ~bits ~ones:(Bits.int (target_bits - len))

let concat = Bitstring.concat

let take s bits =
  let bits = Bits.to_int bits in
  (Bitstring.takebits bits s, Bitstring.dropbits bits s)

let take_int s bits = (Bitstring.takebits bits s, Bitstring.dropbits bits s)
let equal = Bitstring.equals
let show = to_string
let pp fmt bitstr = Format.pp_print_string fmt (show bitstr)

let to_0x x =
  let (`Hex x) = Hex.of_string (Bitstring.string_of_bitstring x) in
  "0x" ^ x

(* let of_char c =
 *   compress_ (Hex.of_char c)
 * 
 * let of_int64 i =
 *   compress_ (Hex.of_int64 i)
 * 
 * let of_string (s : string) =
 *   (s : t)
 * 
 * let to_string (s : t) =
 *   (s : string)
 * 
 * 
 * 
 * let length (x : t) =
 *   Bits.int ((String.length x) * 8)
 * 
 * let zero_padding ~(dir:pad_direction) ~(bits : t) ~(zeroes : Bits.t) =
 *   let zeroes = Bits.to_int zeroes in
 *   if zeroes mod 8 <> 0 then
 *     failwith "Bitstr.Bit.zero_padding: error, can only pad modulo 8"
 *   else
 *     let zero_chars_num = zeroes / 8 in
 *     let padding = String.make zero_chars_num '\x00' in
 *     match dir with
 *     | `left ->
 *       padding^bits
 *     | `right ->
 *       bits^padding
 * 
 * let one_padding ~(dir:pad_direction) ~(bits : t) ~(ones : Bits.t) =
 *   let ones = Bits.to_int ones in
 *   if ones mod 8 <> 0 then
 *     failwith "Bitstr.Bit.one_padding: error, can only pad modulo 8"
 *   else
 *     let ff_chars_num = ones / 8 in
 *     let padding = String.make ff_chars_num '\xff' in
 *     match dir with
 *     | `left ->
 *       padding^bits
 *     | `right ->
 *       bits^padding
 * 
 * let zero_pad_to ~dir ~(bits : t) ~(target_bits : Bits.t) =
 *   let target_bits = Bits.to_int target_bits in
 *   let len = Bits.to_int (length bits) in
 *   if len >= target_bits then
 *     bits
 *   else
 *     zero_padding ~dir ~bits ~zeroes:(Bits.int (target_bits - len))
 * 
 * let one_pad_to ~dir ~(bits : t) ~(target_bits : Bits.t) =
 *   let target_bits = Bits.to_int target_bits in
 *   let len = Bits.to_int (length bits) in
 *   if len >= target_bits then
 *     bits
 *   else
 *     one_padding ~dir ~bits ~ones:(Bits.int (target_bits - len))
 * 
 * let concat (l : t list) =
 *   String.concat "" l
 * 
 * let neg bits =
 *   String.map (fun c ->
 *       Char.chr (lnot (Char.code c))
 *     ) bits
 * 
 * let take (bits : t) (n : Bits.t) =
 *   if Bits.to_int n mod 8 <> 0 then
 *     failwith "Bit.take: can only take bits modulo 8";
 *   let bytes = Bytes.to_int (bits_to_bytes n) in
 *   let head  = String.head bits bytes in
 *   let tail  = String.tail bits bytes in
 *   (head, tail) *)
