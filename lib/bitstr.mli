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
  
val bit_length : bitstring -> int
val zero_padding : bits:bitstring -> zeroes:int -> bitstring
val one_padding : bits:bitstring -> ones:int -> bitstring
val zero_pad_to : bits:bitstring -> target_bits:int -> bitstring
val one_pad_to : bits:bitstring -> target_bits:int -> bitstring
val concat : bitstring list -> bitstring
