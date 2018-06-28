
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
    | Int of { v : int64; t : SolidityTypes.t; }
    | Bool of { v : bool; }
    | String of { v : string; t : SolidityTypes.t; }
    | Tuple of value list
    | Array of { vs : value list; static : static }

  and static =
    | Static
    | Dynamic

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
  val address_val : Types.address -> value    
  val tuple : value list -> value
  val type_of : value -> SolidityTypes.t
  val encode_value : value -> Bitstr.Bit.t
  val decode_events : abi list -> Types.Tx.receipt -> event list
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
    account:Types.address ->
    gas:Z.t ->
    contract:solidity_output ->
    arguments:ABI.value list -> Types.Tx.receipt
  val call_method_tx :
    abi:ABI.method_abi ->
    arguments:ABI.value list ->
    src:Types.address ->
    ctx:Types.address -> gas:Z.t -> Types.Tx.t
  val call_void_method_tx :
    mname:string ->
    src:Types.address ->
    ctx:Types.address -> gas:Z.t -> Types.Tx.t

  val execute_method :
    uri:string ->
    abi:ABI.method_abi ->
    arguments:ABI.value list ->
    src:Types.address ->
    ctx:Types.address -> gas:Z.t -> Types.Tx.receipt

  val call_method :
    uri:string ->
    abi:ABI.method_abi ->
    arguments:ABI.value list ->
    src:Types.address ->
    ctx:Types.address -> gas:Z.t -> string
 

end  
