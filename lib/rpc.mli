open Types

val switch_debug : unit -> unit


module Net :
sig

end

module Eth :
sig

  type time =
    [`block of int | `latest | `earliest | `pending]

  val protocol_version : uri:string -> int
  val syncing : uri:string -> bool
  val coinbase : uri:string -> Address.t
  val mining : uri:string -> bool
  val hashrate : uri:string -> Z.t
  val gas_price : uri:string -> Z.t
  val accounts : uri:string -> Address.t list
  val block_number : uri:string -> int
  val get_balance : uri:string -> address:Address.t -> at_time:time -> Z.t
  val get_storage_at : uri:string -> address:Address.t -> position:Z.t -> at_time:time -> string
  val get_transaction_count : uri:string -> address:Address.t -> at_time:time -> int
  val get_transaction_count_by_hash : uri:string -> block_hash:Hash256.t -> int
  val get_transaction_count_by_number : uri:string -> at_time:time -> int
  val get_code : uri:string -> address:Address.t -> at_time:time -> string
  val get_block_by_hash : uri:string -> block_hash:Hash256.t -> Block.t option
  val get_block_by_number : uri:string -> at_time:time -> Block.t option
  val sign : uri:string -> address:Address.t -> message:string -> string
  val send_transaction : uri:string -> transaction:Tx.t -> Hash256.t
  val send_raw_transaction : uri:string -> data:string -> Hash256.t
  val call : uri:string -> transaction:Tx.t -> at_time:time -> string
  val estimate_gas : uri:string -> transaction:Tx.t -> at_time:time -> Z.t
  val get_transaction_receipt : uri:string -> transaction_hash:Hash256.t -> Tx.receipt option
  val send_transaction_and_get_receipt : uri:string -> transaction:Tx.t -> Tx.receipt
  val send_contract_and_get_receipt_auto : uri:string -> src:Address.t -> data:Bitstr.Hex.t -> ?value:Z.t -> unit -> Tx.receipt
  val send_contract_and_get_receipt : uri:string -> src:Address.t -> data:Bitstr.Hex.t -> gas:Z.t -> ?value:Z.t -> unit -> Tx.receipt
  
end

module EthLwt :
sig
  val send_transaction_and_get_receipt : uri:string -> transaction:Tx.t -> Tx.receipt Lwt.t
  val send_contract_and_get_receipt_auto : uri:string -> src:Address.t -> data:Bitstr.Hex.t -> ?value:Z.t -> unit -> Tx.receipt Lwt.t
  val send_contract_and_get_receipt : uri:string -> src:Address.t -> data:Bitstr.Hex.t -> gas:Z.t -> ?value:Z.t -> unit -> Tx.receipt Lwt.t
end

module Personal :
sig

  val send_transaction : uri:string -> src:Address.t -> dst:Address.t -> value:Z.t -> src_pwd:string -> Hash256.t
  val new_account : uri:string -> passphrase:string -> Address.t
  val unlock_account : uri:string -> account:Address.t -> passphrase:string -> unlock_duration:int -> unit
  
end

module Miner :
sig

  val set_gas_price : uri:string -> gas_price:Z.t -> bool
  val start : uri:string -> thread_count:int -> unit
  val stop : uri:string -> unit
  val set_ether_base : uri:string -> address:Address.t -> bool
  
end

module Admin :
sig

  val add_peer : uri:string -> peer_url:string -> bool
  val datadir  : uri:string -> string
  val node_info : uri:string -> Types.node_info option
  val peers : uri:string -> Types.peer_info

end

module Debug :
sig

  val dump_block : uri:string -> block_number:int -> Types.block_info
  
end
