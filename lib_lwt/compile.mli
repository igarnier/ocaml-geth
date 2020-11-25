open Geth
open Contract

val to_json : filename:string -> t

val deploy_rpc :
  url:Uri.t ->
  account:Types.Address.t ->
  contract:t ->
  arguments:SolidityValue.t list ->
  ?gas:Z.t ->
  ?value:Z.t ->
  unit ->
  Types.Tx.receipt Lwt.t

val call_method_tx :
  url:Uri.t ->
  abi:ABI.Fun.t ->
  arguments:SolidityValue.t list ->
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
  url:Uri.t ->
  abi:ABI.Fun.t ->
  arguments:SolidityValue.t list ->
  src:Types.Address.t ->
  ctx:Types.Address.t ->
  ?gas:Z.t ->
  ?value:Z.t ->
  unit ->
  Types.Tx.receipt Lwt.t

val call_method :
  url:Uri.t ->
  abi:ABI.Fun.t ->
  arguments:SolidityValue.t list ->
  src:Types.Address.t ->
  ctx:Types.Address.t ->
  ?gas:Z.t ->
  ?value:Z.t ->
  unit ->
  string Lwt.t
