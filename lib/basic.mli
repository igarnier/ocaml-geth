
(** In order to avoir mixing bits and bytes, we define private integer types for each. *)
module Bits :
sig
    type t = private int
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val (/) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( mod ) : t -> t -> t

    val int    : int -> t
    val to_int : t -> int
end

module Bytes :
sig
    type t = private int
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val (/) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( mod ) : t -> t -> t

    val int    : int -> t
    val to_int : t -> int
end

val bits_to_bytes : Bits.t -> Bytes.t
val bytes_to_bits : Bytes.t -> Bits.t
