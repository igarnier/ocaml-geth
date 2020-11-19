open Types

module SolidityTypes : sig
  type atom =
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

  and t = Atom of atom | SArray of int * t | DArray of t | Tuple of t list

  val equal : t -> t -> bool
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
end

module ABI : sig
  type value = {desc: value_desc; typ: SolidityTypes.t}

  and value_desc =
    | Int of int64
    | BigInt of Z.t
    | Bool of bool
    | String of string
    | Address of Address.t
    | Tuple of value list
    | Func of {selector: string; address: Address.t}

  type event = {event_name: string; event_args: value list}
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
  end

  module Evt : sig
    type t = {name: string; inputs: named array; anonymous: bool}
  end

  type t = Fun of Fun.t | Event of Evt.t

  val is_constructor : Fun.t -> bool
  val encoding : t Json_encoding.encoding
  val int256_val : int64 -> value
  val uint256_val : int64 -> value
  val string_val : string -> value
  val bytes_val : string -> value
  val bool_val : bool -> value
  val address_val : Address.t -> value
  val tuple_val : value list -> value
  val static_array_val : value list -> SolidityTypes.t -> value
  val dynamic_array_val : value list -> SolidityTypes.t -> value
  val method_id : Fun.t -> Bitstr.Bit.t
  val type_of : value -> SolidityTypes.t

  module Encode : sig
    val int64_as_uint256 : int64 -> Bitstr.Bit.t
    val int64_as_int256 : int64 -> Bitstr.Bit.t
    val address : Address.t -> Bitstr.Bit.t
    val bytes_static : string -> int -> Bitstr.Bit.t
    val bytes_dynamic : string -> Bitstr.Bit.t
    val encode : value -> Bitstr.Bit.t
  end

  module Decode : sig
    val decode : Bitstr.Bit.t -> SolidityTypes.t -> value
    val decode_events : t list -> Tx.receipt -> event list
  end

  (**/*)

  val keccak_4_bytes : string -> Bitstr.Bit.t
end

type t = {contracts: (string * contract) list; version: string}

and contract = {abi: ABI.t list; bin: Bitstring.t}

val find_function : contract -> string -> ABI.Fun.t option
val simple : contract Json_encoding.encoding
val combined : t Json_encoding.encoding
