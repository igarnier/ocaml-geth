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
    | DagDir of string
  type command = Init of string | Attach of string option
  val string_of_option : geth_option -> string
  val make : geth_option list -> command option -> Shell.command
end

type geth_config = {
  genesis_block  : Genesis.t;
  network_id     : int;
  root_directory : string;
  data_subdir    : string;
  source_subdir  : string;
  rpc_port       : int;
  p2p_port       : int;
}

type deploy_target = {
  ip_address : string;
  ssh_port   : int;
  login      : string;
  password   : string;
}

type network = deploy_target list

exception Deploy_error of deploy_target

val deploy : (geth_config * deploy_target) list -> unit
