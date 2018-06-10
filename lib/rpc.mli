
module Net :
sig

end

module Eth :
sig

  type time =
    [`block of int | `latest | `earliest | `pending]

  val protocol_version : uri:string -> int
  val syncing : uri:string -> bool
  val coinbase : uri:string -> Types.address
  val mining : uri:string -> bool
  val hashrate : uri:string -> int
  val gas_price : uri:string -> int
  val accounts : uri:string -> Types.address list
  val block_number : uri:string -> int
  val get_balance : uri:string -> address:Types.address -> at_time:time -> Z.t
  val get_storage_at : uri:string -> address:Types.address -> position:Z.t -> at_time:time -> string
  val get_transaction_count : uri:string -> address:Types.address -> at_time:time -> int
  val get_transaction_count_by_hash : uri:string -> block_hash:Types.hash256 -> int
  val get_transaction_count_by_number : uri:string -> at_time:time -> int
  val get_code : uri:string -> address:Types.address -> at_time:time -> string
  val sign : uri:string -> address:Types.address -> message:string -> string
  val send_transaction : uri:string -> transaction:Types.transaction -> Types.hash256
  val send_raw_transaction : uri:string -> data:string -> Types.hash256
  val call : uri:string -> transaction:Types.transaction -> at_time:time -> string
  val get_transaction_receipt : uri:string -> transaction_hash:Types.hash256 -> Types.transaction_receipt option
  val send_transaction_and_get_receipt : uri:string -> transaction:Types.transaction -> Types.transaction_receipt
  val send_contract_and_get_receipt : uri:string -> src:Types.address -> data:string -> gas:int -> Types.transaction_receipt
end

module Personal :
sig

  val send_transaction : uri:string -> src:Types.address -> dst:Types.address -> value:int -> src_pwd:string -> Types.hash256
  val new_account : uri:string -> passphrase:string -> Types.address
  val unlock_account : uri:string -> account:Types.address -> passphrase:string -> unlock_duration:int -> bool
  
end

module Miner :
sig

  val set_gas_price : uri:string -> gas_price:int -> bool
  val start : uri:string -> thread_count:int -> unit
  val stop : uri:string -> bool
  val set_ether_base : uri:string -> address:Types.address -> bool
  
end

module Admin :
sig

  val add_peer : uri:string -> peer_url:string -> bool
  val datadir  : uri:string -> string
  val node_info : uri:string -> Types.node_info option
  val peers : uri:string -> Types.peer_info

end
