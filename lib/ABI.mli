open Types
open Json_encoding

type value = {desc: desc; typ: SolidityTypes.t}

and desc =
  | Int of Z.t
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
val int : int -> Z.t -> value
val uint : int -> Z.t -> value
val uint256 : Z.t -> value
val string : string -> value
val bytes : string -> value
val bool : bool -> value
val address : Address.t -> value
val tuple : value list -> value
val static_array : value list -> SolidityTypes.t -> value
val dynamic_array : value list -> SolidityTypes.t -> value
val method_id : Fun.t -> Bitstring.t

module Encode : sig
  val encode : value -> Bitstring.t
end

module Decode : sig
  val decode : Bitstring.t -> SolidityTypes.t -> value
  val event_of_log : t list -> Log.t -> event
end

(**/*)

val to_0x : Bitstring.t -> string
val keccak_4_bytes : string -> Bitstring.t
