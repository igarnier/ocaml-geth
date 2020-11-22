type t =
  (* atoms *)
  | UInt of int
  | Int of int
  | Address
  | Bool
  | Fixed of int * int
  | UFixed of int * int
  | NBytes of int
  | Bytes
  | String
  | Function
  (* composite *)
  | SArray of int * t
  | DArray of t
  | Tuple of t list
[@@deriving eq]

val of_string : string -> (t, string) result
val of_string_exn : string -> t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val is_dynamic : t -> bool

(** Smart contructors *)

val uint : int -> t
val int : int -> t
val string : t
val bytes : t
val address : t

(**/*)

module Parser : sig
  val numopt : int option Angstrom.t
  val t : t Angstrom.t
end
