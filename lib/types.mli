(** An Ethereum address corresponding to a private key k_r is the
    rightmost truncation to 160 bit of a 256 bit Keccak hash
    of the corresponding ECDSA public key. Cf Yellow Paper. *)

module type BINARY = sig
  type t = private string [@@deriving ord, eq, show]

  val of_0x : string -> t
  val of_hex : Hex.t -> t
  val of_binary : ?pos:int -> string -> t
  val encoding : t Json_encoding.encoding
end

module Address : BINARY
module Hash256 : BINARY
module Hash512 : BINARY

module Log : sig
  type t =
    { blkNum: int;
      blkHash: Hash256.t;
      txIdx: int;
      txHash: Hash256.t;
      logIdx: int;
      address: Address.t;
      topics: Hash256.t array;
      data: string;
      removed: bool }
  [@@deriving show]

  val encoding : t Json_encoding.encoding
end

module Tx : sig
  type t =
    { src: Address.t;
      dst: Address.t option;
      gas: Z.t option;
      gas_price: Z.t option;
      value: Z.t option;
      data: string;
      nonce: int option }

  type accepted =
    { tx: t;
      block_hash: Hash256.t;
      block_number: int;
      tx_hash: Hash256.t;
      tx_index: int }

  type receipt =
    { block_hash: Hash256.t;
      block_number: int;
      contract_address: Address.t option;
      cumulative_gas_used: Z.t;
      gas_used: Z.t;
      src: Address.t;
      dst: Address.t option;
      logs: Log.t list;
      (* unused:
       * logs_bloom : string;
       * root : string; *)
      transaction_hash: Hash256.t;
      transaction_index: int }

  val accepted_from_json : Yojson.Safe.t -> accepted
  val to_json : t -> Yojson.Safe.t
  val receipt_from_json : Yojson.Safe.t -> receipt option
  val show_receipt : receipt -> string
end

module Block : sig
  type t =
    { num: int option;
      hash: Hash256.t option;
      mixHash: Hash256.t option;
      pHash: Hash256.t;
      nonce: int64 option;
      sha3Uncles: Hash256.t option;
      bloom: string;
      txRoot: Hash256.t;
      stateRoot: Hash256.t;
      receiptsRoot: Hash256.t;
      (* *)
      miner: Address.t;
      diff: Z.t;
      totalDiff: Z.t option;
      data: string;
      size: int;
      gasLimit: int64;
      gasUsed: int64;
      timestamp: int64;
      txs: Tx.accepted array;
      txHashes: Hash256.t array;
      uncles: Hash256.t array }
  [@@deriving show]

  val encoding : t Json_encoding.encoding
end

type port_info = {discovery: int; listener: int}

type protocol_info =
  | Eth of
      { difficulty: Z.t;
        genesis: Hash256.t option;
        head: Hash256.t;
        network: int option }

type node_info =
  { enode: string;
    id: Hash512.t;
    ip: string;
    listen_addr: string;
    name: string;
    ports: port_info;
    protocols: protocol_info }

type network_info = {local_address: string; remote_address: string}

type peer =
  { caps: string list;
    id: Hash512.t;
    name: string;
    network: network_info;
    protocols: protocol_info }

type peer_info = peer list

type block_info = {block_root: Hash256.t; block_accounts: ba_info list}

and ba_info =
  { ba_account: Address.t;
    ba_balance: Z.t;
    ba_code: Evm.bytecode;
    ba_code_hash: Hash256.t;
    ba_nonce: int;
    ba_root: Hash256.t;
    ba_storage: ba_storage }

and ba_storage = (Hash256.t * string) list

(* TODO: txpool *)
(* type txpool =
 *   {
 *     pending : txpool_info list;
 *     queued : txpool_info list
 *   }
 *
 * and txpool_info =
 *   {
 *     (\* tx_block_hash   : hash512;
 *      * tx_block_number : int option;
 *      * tx_src          : address;
 *      * dst             :  *\)
 *
 *         from: "0x0216d5032f356960cd3749c31ab34eeff21b3395",
 *         to: "0x7f69a91a3cf4be60020fb58b893b7cbb65376db8",
 *         gas: "0x5208",
 *         gasPrice: "0xba43b7400",
 *         value: "0x19a99f0cf456000"
 *         no data !
 *         nonce: "0x326",
 *
 *         blockHash: "0x0000000000000000000000000000000000000000000000000000000000000000",
 *         blockNumber: null,
 *
 *
 *         hash: "0xaf953a2d01f55cfe080c0c94150a60105e8ac3d51153058a1f03dd239dd08586",
 *         input: "0x",
 *         transactionIndex: null,
 *   } *)

(* val transaction_to_json : transaction -> Yojson.Safe.t *)
(* val receipt_from_json : Yojson.Safe.t -> transaction_receipt option *)
val node_info_from_json : Yojson.Safe.t -> node_info option
val peer_info_from_json : Yojson.Safe.t -> peer_info
val block_from_json : Yojson.Safe.t -> block_info
