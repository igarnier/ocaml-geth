open Basic

module Hex :
sig

  type t = private string

  val is_hex : string -> bool

  val of_int : int -> t
  val of_bigint : Z.t -> t  
  val of_char : char -> t
  val of_int64 : int64 -> t
  val of_string : string -> t
  val as_string : t -> string
  val length : t -> Bytes.t

end

module Bit :
sig

  type t = private string

  val of_int64 : int64 -> t
  val of_string : string -> t
  val as_string : t -> string

  type pad_direction = [ `left | `right ]
  
  val length : t -> Bits.t
  val zero_padding : dir:pad_direction -> bits:t -> zeroes:Bits.t -> t
  val one_padding  : dir:pad_direction -> bits:t -> ones:Bits.t -> t
  val zero_pad_to : dir:pad_direction -> bits:t -> target_bits:Bits.t -> t
  val one_pad_to : dir:pad_direction -> bits:t -> target_bits:Bits.t -> t
  val concat : t list -> t

  val take : t -> Bits.t -> t * t
  
end

val compress : Hex.t -> Bit.t
val uncompress : Bit.t -> Hex.t
