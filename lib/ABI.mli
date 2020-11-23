open Types
open Json_encoding
module SV = SolidityValue

type event = {name: string; args: SV.t list}
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
val event_of_log : Evt.t list -> Log.t -> event

(**/*)

val to_0x : Bitstring.t -> string
val keccak : string -> string
val keccak_4_bytes : string -> string
val event_id : Evt.t -> Bitstring.t
val method_id : Fun.t -> Bitstring.t
val create2 : addr:Address.t -> salt:string -> initCode:string -> string
