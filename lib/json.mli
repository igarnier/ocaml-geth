open Yojson.Safe

type errmsg = {code: int; msg: string}

exception JsonError of errmsg

val hex : int -> t
val zhex : Z.t -> t
val maybe : (t -> 'a) -> t -> 'a option
val drop_assoc : t -> (string * t) list
val drop_string : t -> string
val drop_int : t -> int
val drop_int64 : t -> int64
val drop_bigint : t -> Z.t
val drop_list : t -> t list
val drop_bool : t -> bool
val drop_null : t -> unit
val drop_int_as_string : t -> int
val drop_int64_as_string : t -> int64
val drop_bigint_as_string : t -> Z.t
val drop_string_list : t -> string list

module Get : sig
  val result : t -> (t, errmsg) result
  val bool : t -> (bool, errmsg) result
  val null : t -> (unit, errmsg) result
  val int : t -> (int, errmsg) result
  val bigint : t -> (Z.t, errmsg) result
  val int_as_string : t -> (int, errmsg) result
  val bigint_as_string : t -> (Z.t, errmsg) result
  val string : t -> (string, errmsg) result
  val string_list : t -> (string list, errmsg) result
end

module GetExn : sig
  val result : t -> t
  val bool : t -> bool
  val null : t -> unit
  val int : t -> int
  val bigint : t -> Z.t
  val int_as_string : t -> int
  val bigint_as_string : t -> Z.t
  val string : t -> string
  val string_list : t -> string list
end

val from_string : string -> t
val to_string : t -> string
