open Geth
open Types

val switch_debug : unit -> unit

(* module Net :
 * sig
 * 
 * end *)

module Eth : sig
  type time = [`block of int | `latest | `earliest | `pending]

  val protocol_version : uri:string -> int Lwt.t
  val syncing : uri:string -> bool Lwt.t
  val coinbase : uri:string -> Address.t Lwt.t
  val mining : uri:string -> bool Lwt.t
  val hashrate : uri:string -> Z.t Lwt.t
  val gas_price : uri:string -> Z.t Lwt.t
  val accounts : uri:string -> Address.t list Lwt.t
  val block_number : uri:string -> int Lwt.t
  val get_balance : uri:string -> address:Address.t -> at_time:time -> Z.t Lwt.t

  val get_storage_at :
    uri:string ->
    address:Address.t ->
    position:Z.t ->
    at_time:time ->
    string Lwt.t

  val get_transaction_count :
    uri:string -> address:Address.t -> at_time:time -> int Lwt.t

  val get_transaction_count_by_hash :
    uri:string -> block_hash:Hash256.t -> int Lwt.t

  val get_transaction_count_by_number : uri:string -> at_time:time -> int Lwt.t
  val get_code : uri:string -> address:Address.t -> at_time:time -> string Lwt.t

  val get_block_by_hash :
    uri:string -> block_hash:Hash256.t -> Block.t option Lwt.t

  val get_block_by_number : uri:string -> at_time:time -> Block.t option Lwt.t
  val sign : uri:string -> address:Address.t -> message:string -> string Lwt.t
  val send_transaction : uri:string -> transaction:Tx.t -> Hash256.t Lwt.t
  val send_raw_transaction : uri:string -> data:string -> Hash256.t Lwt.t
  val call : uri:string -> transaction:Tx.t -> at_time:time -> string Lwt.t
  val estimate_gas : uri:string -> transaction:Tx.t -> Z.t Lwt.t

  val get_transaction_receipt :
    uri:string -> transaction_hash:Hash256.t -> Tx.receipt option Lwt.t

  val send_transaction_and_get_receipt :
    uri:string -> transaction:Tx.t -> Tx.receipt Lwt.t

  (* val send_contract_and_get_receipt_auto : uri:string -> src:Address.t -> data:Bitstr.Hex.t -> ?value:Z.t -> unit -> Tx.receipt *)
  val send_contract_and_get_receipt :
    uri:string ->
    src:Address.t ->
    data:Bitstr.Hex.t ->
    ?gas:Z.t ->
    ?value:Z.t ->
    unit ->
    Tx.receipt Lwt.t
end

(* module EthLwt :
 * sig
 *   val send_transaction_and_get_receipt : uri:string -> transaction:Tx.t -> Tx.receipt Lwt.t
 *   (\* val send_contract_and_get_receipt_auto : uri:string -> src:Address.t -> data:Bitstr.Hex.t -> ?value:Z.t -> unit -> Tx.receipt Lwt.t *\)
 *   val send_contract_and_get_receipt : uri:string -> src:Address.t -> data:Bitstr.Hex.t -> ?gas:Z.t -> ?value:Z.t -> unit -> Tx.receipt Lwt.t
 * end *)

module Personal : sig
  val send_transaction :
    uri:string ->
    src:Address.t ->
    dst:Address.t ->
    value:Z.t ->
    src_pwd:string ->
    Hash256.t Lwt.t

  val new_account : uri:string -> passphrase:string -> Address.t Lwt.t

  val unlock_account :
    uri:string ->
    account:Address.t ->
    passphrase:string ->
    unlock_duration:int ->
    bool Lwt.t
end

module Miner : sig
  val set_gas_price : uri:string -> gas_price:Z.t -> bool Lwt.t
  val start : uri:string -> thread_count:int -> unit Lwt.t
  val stop : uri:string -> unit Lwt.t
  val set_ether_base : uri:string -> address:Address.t -> bool Lwt.t
end

module Admin : sig
  val add_peer : uri:string -> peer_url:string -> bool Lwt.t
  val datadir : uri:string -> string Lwt.t
  val node_info : uri:string -> Types.node_info option Lwt.t
  val peers : uri:string -> Types.peer_info Lwt.t
end

module Debug : sig
  val dump_block : uri:string -> block_number:int -> Types.block_info Lwt.t
end
