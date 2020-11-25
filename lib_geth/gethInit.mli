open Geth

module Genesis : sig
  type config =
    {chain_id: int; homestead_block: int; eip155_block: int; eip158_block: int}

  type t =
    { config: config;
      alloc: (Types.Address.t * int) list;
      coinbase: Types.Address.t;
      difficulty: int;
      extra_data: string;
      gas_limit: int;
      nonce: int;
      mix_hash: Types.Hash256.t;
      parent_hash: Types.Hash256.t;
      timestamp: int }

  val hex_of_int : int -> string
  val to_json : t -> Yojson.Basic.t
end

module Geth : sig
  val bootnode_genkey : string -> Shell.command
  val bootnode_nodekey : int -> string -> Shell.command

  type geth_option =
    | Datadir of string
    | Bootnodes of string list
    | NetworkId of int
    | Verbosity of int
    | Nodiscover
    | Exec of string
    | IpcPath of string
    | Rpc of {rpcport: int; rpcaddr: string; rpcapis: string}
    | Port of int
    | DagDir of string

  type command = Init of string | Attach of string option

  val string_of_option : geth_option -> string
  val make : geth_option list -> command option -> Shell.command
end

type geth_config =
  { genesis_block: Genesis.t;
    network_id: int;
    root_directory: string;
    data_subdir: string;
    source_subdir: string;
    rpc_port: int;
    p2p_port: int }

type deploy_target =
  {ip_address: string; ssh_port: int; login: string; password: string}

type network = deploy_target list

exception Deploy_error of deploy_target

val deploy : (geth_config * deploy_target) list -> unit
