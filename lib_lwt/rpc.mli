open Geth
open Types

val switch_debug : unit -> unit

(* module Net :
 * sig
 * 
 * end *)

module Eth : sig
  type time = [`block of int | `latest | `earliest | `pending]

  val protocol_version : Uri.t -> int Lwt.t
  val syncing : Uri.t -> bool Lwt.t
  val coinbase : Uri.t -> Address.t Lwt.t
  val mining : Uri.t -> bool Lwt.t
  val hashrate : Uri.t -> Z.t Lwt.t
  val gas_price : Uri.t -> Z.t Lwt.t
  val accounts : Uri.t -> Address.t list Lwt.t
  val block_number : Uri.t -> int Lwt.t
  val get_balance : Uri.t -> address:Address.t -> at_time:time -> Z.t Lwt.t

  val get_storage_at :
    Uri.t -> address:Address.t -> position:Z.t -> at_time:time -> string Lwt.t

  val get_transaction_count :
    Uri.t -> address:Address.t -> at_time:time -> int Lwt.t

  val get_transaction_count_by_hash : Uri.t -> block_hash:Hash256.t -> int Lwt.t
  val get_transaction_count_by_number : Uri.t -> at_time:time -> int Lwt.t
  val get_code : Uri.t -> address:Address.t -> at_time:time -> string Lwt.t
  val get_block_by_hash : Uri.t -> block_hash:Hash256.t -> Block.t option Lwt.t
  val get_block_by_number : Uri.t -> at_time:time -> Block.t option Lwt.t
  val sign : Uri.t -> address:Address.t -> message:string -> string Lwt.t
  val send_transaction : Uri.t -> transaction:Tx.t -> Hash256.t Lwt.t
  val send_raw_transaction : Uri.t -> data:string -> Hash256.t Lwt.t
  val call : Uri.t -> transaction:Tx.t -> at_time:time -> string Lwt.t
  val estimate_gas : Uri.t -> transaction:Tx.t -> Z.t Lwt.t

  val get_transaction_receipt :
    Uri.t -> transaction_hash:Hash256.t -> Tx.receipt option Lwt.t

  val send_transaction_and_get_receipt :
    Uri.t -> transaction:Tx.t -> Tx.receipt Lwt.t

  (* val send_contract_and_get_receipt_auto : Uri.t -> src:Address.t -> data:Bitstr.Hex.t -> ?value:Z.t -> unit -> Tx.receipt *)
  val send_contract_and_get_receipt :
    Uri.t ->
    src:Address.t ->
    data:string ->
    ?gas:Z.t ->
    ?value:Z.t ->
    unit ->
    Tx.receipt Lwt.t
end

(* module EthLwt :
 * sig
 *   val send_transaction_and_get_receipt : Uri.t -> transaction:Tx.t -> Tx.receipt Lwt.t
 *   (\* val send_contract_and_get_receipt_auto : Uri.t -> src:Address.t -> data:Bitstr.Hex.t -> ?value:Z.t -> unit -> Tx.receipt Lwt.t *\)
 *   val send_contract_and_get_receipt : Uri.t -> src:Address.t -> data:Bitstr.Hex.t -> ?gas:Z.t -> ?value:Z.t -> unit -> Tx.receipt Lwt.t
 * end *)

module Personal : sig
  val send_transaction :
    Uri.t ->
    src:Address.t ->
    dst:Address.t ->
    value:Z.t ->
    src_pwd:string ->
    Hash256.t Lwt.t

  val new_account : Uri.t -> passphrase:string -> Address.t Lwt.t

  val unlock_account :
    Uri.t ->
    account:Address.t ->
    passphrase:string ->
    unlock_duration:int ->
    bool Lwt.t
end

module Miner : sig
  val set_gas_price : Uri.t -> gas_price:Z.t -> bool Lwt.t
  val start : Uri.t -> thread_count:int -> unit Lwt.t
  val stop : Uri.t -> unit Lwt.t
  val set_ether_base : Uri.t -> address:Address.t -> bool Lwt.t
end

module Admin : sig
  val add_peer : Uri.t -> peer_url:string -> bool Lwt.t
  val datadir : Uri.t -> string Lwt.t
  val node_info : Uri.t -> Types.node_info option Lwt.t
  val peers : Uri.t -> Types.peer_info Lwt.t
end

module Debug : sig
  val dump_block : Uri.t -> block_number:int -> Types.block_info Lwt.t
end
