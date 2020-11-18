open Geth
open Contract

type solidity_output = {version: string; contracts: solidity_contract list}

and solidity_contract =
  {contract_name: string; bin: Bitstr.Bit.t; abi: ABI.t list}

val to_json : filename:string -> solidity_output
val get_constructor : solidity_contract -> ABI.Fun.t
val get_method : solidity_contract -> string -> ABI.Fun.t option

val deploy_rpc :
  uri:string ->
  account:Types.Address.t ->
  contract:solidity_output ->
  arguments:ABI.value list ->
  ?gas:Z.t ->
  ?value:Z.t ->
  unit ->
  Types.Tx.receipt Lwt.t

val call_method_tx :
  uri:string ->
  abi:ABI.Fun.t ->
  arguments:ABI.value list ->
  src:Types.Address.t ->
  ctx:Types.Address.t ->
  ?gas:Z.t ->
  ?value:Z.t ->
  unit ->
  Types.Tx.t Lwt.t

val call_void_method_tx :
  mname:string ->
  src:Types.Address.t ->
  ctx:Types.Address.t ->
  ?gas:Z.t ->
  unit ->
  Types.Tx.t Lwt.t

val execute_method :
  uri:string ->
  abi:ABI.Fun.t ->
  arguments:ABI.value list ->
  src:Types.Address.t ->
  ctx:Types.Address.t ->
  ?gas:Z.t ->
  ?value:Z.t ->
  unit ->
  Types.Tx.receipt Lwt.t

val call_method :
  uri:string ->
  abi:ABI.Fun.t ->
  arguments:ABI.value list ->
  src:Types.Address.t ->
  ctx:Types.Address.t ->
  ?gas:Z.t ->
  ?value:Z.t ->
  unit ->
  string Lwt.t
