type t = Bitstring.bitstring [@@deriving eq]
type pad_direction = [`left | `right]

val pp : Format.formatter -> t -> unit
val of_char : char -> t
val of_int64 : int64 -> t
val of_string : string -> t
val of_bigint : int -> Z.t -> t
val to_signed_bigint : t -> Z.t
val to_unsigned_bigint : t -> Z.t
val to_string : t -> string
val length : t -> int
val zero_padding : dir:pad_direction -> bits:t -> zeroes:int -> t
val one_padding : dir:pad_direction -> bits:t -> ones:int -> t
val zero_pad_to : dir:pad_direction -> bits:t -> target_bits:int -> t
val one_pad_to : dir:pad_direction -> bits:t -> target_bits:int -> t
val concat : t list -> t

(* val neg : t -> t *)

val take : t -> int -> t * t
val take_int : t -> int -> t * t
val to_0x : t -> string
