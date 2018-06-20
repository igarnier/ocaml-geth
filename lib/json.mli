type errmsg = { code : int; msg : string }

type 'a result = ('a, errmsg) Batteries.Result.t

exception JsonError of errmsg

type json = Yojson.Safe.json

val hex : int -> json

val zhex : Z.t -> json

val maybe : (json -> 'a) -> json -> 'a option

val drop_assoc : json -> (string * json) list

val drop_string : json -> string

val drop_int : json -> int

val drop_int64 : json -> int64

val drop_bigint : json -> Z.t

val drop_list : json -> json list

val drop_bool : json -> bool

val drop_null : json -> unit

val drop_int_as_string : json -> int

val drop_int64_as_string : json -> int64

val drop_bigint_as_string : json -> Z.t

val drop_string_list : json -> string list

module Get :
sig
  val result : json -> (json, errmsg) Batteries.result
  val bool : json -> (bool, errmsg) Batteries.Result.t
  val null : json -> (unit, errmsg) Batteries.Result.t
  val int : json -> (int, errmsg) Batteries.Result.t
  val bigint : json -> (Z.t, errmsg) Batteries.Result.t
  val int_as_string : json -> (int, errmsg) Batteries.Result.t
  val bigint_as_string : json -> (Z.t, errmsg) Batteries.Result.t
  val string : json -> (string, errmsg) Batteries.Result.t
  val string_list : json -> (string list, errmsg) Batteries.Result.t
end

module GetExn :
sig
  val result : json -> json
  val bool : json -> bool
  val null : json -> unit
  val int : json -> int
  val bigint : json -> Z.t
  val int_as_string : json -> int
  val bigint_as_string : json -> Z.t
  val string : json -> string
  val string_list : json -> string list  
end

val from_string : string -> json
val to_string : json -> string
