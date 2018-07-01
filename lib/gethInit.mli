module Genesis :
sig
  type config = {
    chain_id : int;
    homestead_block : Types.block_id;
    eip155_block : Types.block_id;
    eip158_block : Types.block_id;
  }
  type t = {
    config : config;
    alloc : (Types.address * Types.wei) list;
    coinbase : Types.address;
    difficulty : int;
    extra_data : string;
    gas_limit : int;
    nonce : int;
    mix_hash : Types.hash256;
    parent_hash : Types.hash256;
    timestamp : int;
  }
  val hex_of_int : int -> string
  val to_json : t -> Yojson.Basic.json
end

type geth_config = {
  genesis_block : Genesis.t;
  network_id : int;
  root_directory : string;
  data_subdir : string;
  source_subdir : string;
}

val ( // ) : string -> string -> string

module Geth :
sig
  val bootnode_genkey : string -> Shell.command
  val bootnode_nodekey : int -> string -> Shell.command
  type geth_option =
      Datadir of string
    | Bootnodes of string list
    | NetworkId of int
    | Verbosity of int
    | Nodiscover
    | Exec of string
    | IpcPath of string
    | Rpc of { rpcport : int; rpcaddr : string; rpcapis : string; }
    | Port of int
  type command = Init of string | Attach of string option
  val string_of_option : geth_option -> string
  val make : geth_option list -> command option -> Shell.command
end

type deploy_target = {
  ip_address : string;
  ssh_port : int;
  eth_port : int;
  login : string;
  password : string;
}
type network = deploy_target list
exception Deploy_error of deploy_target
val log_exec :
  ?read_stderr:bool ->
  ?read_timeout:int ->
  Ssh_client.Easy.shell_handle -> Shell.command -> unit
val log_exec_return :
  ?read_stderr:bool ->
  ?read_timeout:int ->
  Ssh_client.Easy.shell_handle -> Shell.command -> string
val log_exec_code :
  ?read_stderr:bool ->
  ?read_timeout:int ->
  Ssh_client.Easy.shell_handle -> Shell.command -> int
val write_genesis :
  Genesis.t -> string -> int -> Ssh_client.Types.ssh_session -> unit
val login_target :
  deploy_target -> (Ssh_client.Types.ssh_session -> 'a) -> 'a
val parse_enode : string -> string
val port_is_free : Ssh_client.Easy.shell_handle -> int -> bool
val add_peers : deploy_target -> string list -> unit
val revert_deploy : geth_config -> deploy_target -> unit
val prepare_target : geth_config -> deploy_target -> string
val configure_step : geth_config -> deploy_target list -> string list
val startup_step :
  geth_config ->
  deploy_target list -> (deploy_target * string) list -> unit
val deploy : geth_config -> deploy_target list -> unit
