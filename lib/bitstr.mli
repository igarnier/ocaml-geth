open Basic

type bitstring = private string
type hexstring = private string

val string_is_hex : string -> bool

val compress : hexstring -> bitstring
val uncompress : bitstring -> hexstring

val hex_of_int : int -> hexstring
val hex_of_char : char -> hexstring
val hex_of_int64 : int64 -> hexstring
val hex_of_string : string -> hexstring
val hex_as_string : hexstring -> string

val bits_of_int64 : int64 -> bitstring
val bits_of_string : string -> bitstring
val bits_as_string : bitstring -> string

type pad_direction = [ `left | `right ]
  
val bit_length : bitstring -> Bits.t
val zero_padding : dir:pad_direction -> bits:bitstring -> zeroes:Bits.t -> bitstring
val one_padding  : dir:pad_direction -> bits:bitstring -> ones:Bits.t -> bitstring
val zero_pad_to : dir:pad_direction -> bits:bitstring -> target_bits:Bits.t -> bitstring
val one_pad_to : dir:pad_direction -> bits:bitstring -> target_bits:Bits.t -> bitstring
val concat : bitstring list -> bitstring
