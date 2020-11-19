open CCFun
open Basic

(* This is an external library whose name clashes with a module defined here. *)
module ExtHx = Hex

module Hex = struct
  type t = string [@@deriving eq]

  let char_is_hex = function
    | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
    | _ -> false

  let is_hex (x : string) =
    let rec loop i len x acc =
      if i >= len || not acc then acc
      else loop (i + 1) len x (char_is_hex x.[i]) in
    let len = String.length x in
    if len < 2 then failwith "is_hex: string too short"
    else
      let prefix = String.sub x 0 2 in
      match prefix with "0x" | "0X" -> loop 2 len x true | _ -> false

  let of_ubigint z =
    if Z.lt z Z.zero then failwith "of_ubigint: negative argument"
    else
      let hexstr = Z.format "%x" z in
      (* Geth expects its hex integers to have length mod 2 = 0 ... *)
      if String.length hexstr mod 2 = 0 then "0x" ^ hexstr else "0x0" ^ hexstr

  let of_uint = of_ubigint % Z.of_int

  (* let of_int64 = of_ubigint % Z.of_int64 *)

  (* let of_bigint z =
   *   if Z.lt z Z.zero then failwith "of_ubigint: negative argument"
   *   else
   *     let hexstr = Z.format "%x" z in
   *     (\* Geth expects its hex integers to have length mod 2 = 0 ... *\)
   *     if String.length hexstr mod 2 = 0 then "0x" ^ hexstr else "0x0" ^ hexstr *)

  let of_int i =
    (* use two's complement *)
    let hexstr = Printf.sprintf "%x" i in
    if String.length hexstr mod 2 = 0 then "0x" ^ hexstr
    else if i < 0 then "0xf" ^ hexstr
    else "0x0" ^ hexstr

  let of_char c = of_int (Char.code c)

  let of_string string =
    if not (is_hex string) then
      failwith "hex_of_string: " ^ string ^ " not a hex string"
    else string

  let of_int64 (i : int64) =
    let hexstr = Printf.sprintf "%Lx" i in
    if String.length hexstr mod 2 = 0 then "0x" ^ hexstr
    else if i < 0L then (* two's complement! *)
      "0xf" ^ hexstr
    else "0x0" ^ hexstr

  let of_bigint (z : Z.t) =
    let hexstr = Z.format "%x" z in
    if Z.geq z Z.zero then
      if String.length hexstr mod 2 = 0 then "0x" ^ hexstr else "0x0" ^ hexstr
    else if Z.fits_int64 z then of_int64 (Z.to_int64 z)
    else
      failwith
        "of_bigint: can't handle two's complement for really big bigints :( \
         TODO"

  let to_string x = x
  let length (x : t) = Bytes.int ((String.length x - 2) / 2)
  let show = of_string
  let pp fmt s = Format.pp_print_string fmt (of_string s)
end

module Bit = struct
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
end

let compress_ (x : Hex.t) : string =
  assert (Hex.is_hex x) ;
  let stripped =
    let len = String.length x in
    String.sub x (len - 2) 2 in
  try ExtHx.to_string (`Hex stripped)
  with Invalid_argument s -> raise (Invalid_argument (s ^ ": " ^ x))

let compress (x : Hex.t) : Bit.t = Bit.of_string (compress_ x)

let uncompress (x : Bit.t) : Hex.t =
  let (`Hex result) = ExtHx.of_string (Bit.to_string x) in
  "0x" ^ result
