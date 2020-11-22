open Types
open Json_encoding

type value = {desc: desc; typ: SolidityTypes.t}

and desc =
  | Int of int64
  | BigInt of Z.t
  | Bool of bool
  | String of string
  | Address of Address.t
  | Tuple of value list
  | Func of {selector: string; address: Address.t}

type event = {name: string; args: value list}
type named = {name: string; t: SolidityTypes.t; indexed: bool}

module Fun : sig
  type t =
    { kind: kind;
      name: string;
      inputs: named array;
      outputs: named array;
      mutability: mutability }

  and kind = Function | Constructor | Receive | Fallback

  and mutability = Pure | View | Nonpayable | Payable

  val is_constructor : t -> bool
end

module Evt : sig
  type t = {name: string; inputs: named array; anonymous: bool}
end

type t = Fun of Fun.t | Event of Evt.t

val func : t -> Fun.t option
val event : t -> Evt.t option
val encoding : t encoding
val int256_val : int64 -> value
val uint256_val : int64 -> value
val string_val : string -> value
val bytes_val : string -> value
val bool_val : bool -> value
val address_val : Address.t -> value
val tuple_val : value list -> value
val static_array_val : value list -> SolidityTypes.t -> value
val dynamic_array_val : value list -> SolidityTypes.t -> value
val method_id : Fun.t -> Bitstr.t
val type_of : value -> SolidityTypes.t

module Encode : sig
  val int64_as_uint256 : int64 -> Bitstr.t
  val int64_as_int256 : int64 -> Bitstr.t
  val address : Address.t -> Bitstr.t
  val bytes_static : string -> int -> Bitstr.t
  val bytes_dynamic : string -> Bitstr.t
  val encode : value -> Bitstr.t
end

module Decode : sig
  val decode : Bitstr.t -> SolidityTypes.t -> value
  val event_of_log : t list -> Log.t -> event
end

(**/*)

val keccak_4_bytes : string -> Bitstr.t
