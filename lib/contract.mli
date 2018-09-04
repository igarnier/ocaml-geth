
module SolidityTypes :
sig
  type bitwidth = Basic.Bits.t
  type length = StaticLength of Basic.Bytes.t | DynamicLength
  type atomic =
      Tuint of { w : bitwidth }
    | Tint of { w : bitwidth }
    | Taddress
    | Tbool
    | Tfixed of { m : bitwidth; n : bitwidth }
    | Tufixed of { m : bitwidth; n : bitwidth }
    | Tbytes of { nbytes : length; }
    | Tstring
  and t =
      Tatomic of atomic
    | Tfunction
    | Tstatic_array of { numel : int; typ : t }
    | Tdynamic_array of { typ : t }
    | Ttuple of t list
  val is_dynamic : t -> bool
  val uint_t : int -> t
  val int_t : int -> t
  val string_t : t
  val bytes_t : t
  val address_t : t
end

module ABI :
sig

  type value =
    {
      desc : value_desc;
      typ  : SolidityTypes.t
    }

  and value_desc =
    | Int     of int64
    | BigInt  of Z.t
    | Bool    of bool
    | String  of string
    | Address of Types.Address.t
    | Tuple   of value list
    | Func    of { selector : string; address : Bitstr.Hex.t }

  type event =
    {
      event_name : string;
      event_args : value list
    }
  
  type abi =
    | Method of method_abi
    | Constructor of constructor_abi
    | Event of event_abi
  and constructor_abi = {
    c_inputs     : tuple_abi;
    c_payable    : bool;
    c_mutability : mutability;
  }
  and method_abi = {
    m_name       : string;
    m_constant   : bool;
    m_inputs     : tuple_abi;
    m_outputs    : tuple_abi;
    m_payable    : bool;
    m_mutability : mutability;
    m_type       : mtype;
  }
  and event_abi = {
    e_name      : string;
    e_inputs    : tuple_abi;
    e_anonymous : bool;
  }
  and tuple_abi = named_arg list
  and named_arg = { arg_name : string; arg_type : SolidityTypes.t; }
  and mtype = Function | Callback
  and mutability = Pure | View | Nonpayable | Payable

  val int256_val : int64 -> value
  val uint256_val : int64 -> value
  val string_val : string -> value
  val bytes_val : string -> value
  val bytes_val : string -> value
  val bool_val : bool -> value
  val address_val : Types.Address.t -> value
  val tuple_val : value list -> value
  val static_array_val : value list -> SolidityTypes.t -> value
  val dynamic_array_val : value list -> SolidityTypes.t -> value

  val method_id : method_abi -> Bitstr.Bit.t
  val type_of : value -> SolidityTypes.t

  module Encode :
  sig
    val int64_as_uint256 : int64 -> Bitstr.Bit.t
    val int64_as_int256 : int64 -> Bitstr.Bit.t
    val address : Types.Address.t -> Bitstr.Bit.t
    val bytes_static : string -> Basic.Bytes.t -> Bitstr.Bit.t
    val bytes_dynamic : string -> Bitstr.Bit.t
    val encode : value -> Bitstr.Bit.t
  end

  module Decode :
  sig
    val decode : Bitstr.Bit.t -> SolidityTypes.t -> value
    val decode_events : abi list -> Types.Tx.receipt -> event list
  end

  val from_json : Json.json -> abi list
end

module Compile :
sig
  type solidity_output = {
    version   : string;
    contracts : solidity_contract list;
  }
  and solidity_contract = {
    contract_name : string;
    bin : Bitstr.Bit.t;
    abi : ABI.abi list;
  }

  val to_json : filename:string -> solidity_output
  val get_constructor : solidity_contract -> ABI.constructor_abi
  val get_method : solidity_contract -> string -> ABI.method_abi option

  val deploy_rpc :
    uri:string ->
    account:Types.Address.t ->
    gas:Z.t ->
    contract:solidity_output ->
    arguments:ABI.value list ->
    value:Z.t option ->
    Types.Tx.receipt

  val call_method_tx :
    abi:ABI.method_abi ->
    arguments:ABI.value list ->
    src:Types.Address.t ->
    ctx:Types.Address.t -> gas:Z.t -> value:Z.t option -> Types.Tx.t

  val call_void_method_tx :
    mname:string ->
    src:Types.Address.t ->
    ctx:Types.Address.t -> gas:Z.t -> Types.Tx.t

  val execute_method :
    uri:string ->
    abi:ABI.method_abi ->
    arguments:ABI.value list ->
    src:Types.Address.t ->
    ctx:Types.Address.t -> gas:Z.t -> value:Z.t option -> Types.Tx.receipt

  val execute_method_lwt :
    uri:string ->
    abi:ABI.method_abi ->
    arguments:ABI.value list ->
    src:Types.Address.t ->
    ctx:Types.Address.t -> gas:Z.t -> value:Z.t option -> Types.Tx.receipt Lwt.t
  
  val call_method :
    uri:string ->
    abi:ABI.method_abi ->
    arguments:ABI.value list ->
    src:Types.Address.t ->
    ctx:Types.Address.t -> gas:Z.t -> value:Z.t option -> string
 

end  
