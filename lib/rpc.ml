open Batteries
    
module Http_client = Nethttp_client

let rpc_call uri method_name (params : Json.json) =
  let open Yojson.Safe in
  let json : Json.json =
    `Assoc [ ("jsonrpc", `String "2.0");
             ("method", `String method_name);
             ("params", params);
             ("id", `Int 0); (* TODO: this sould be a UID *)
           ]
  in
  let data = Yojson.Safe.to_string json in
  Printf.printf "Rpc.call: raw request =\n%s\n" data;
  let req = new Http_client.post_raw uri data in
  req#set_req_header "Content-type" "application/json";
  let pipeline = new Http_client.pipeline in
  pipeline#add_with_callback req @@
  (fun call -> match call#response_status with
     | `Ok -> ()
     | `Bad_request ->
       print_endline call#response_body#value;
       let j = Json.from_string call#response_body#value in
       j |> Json.drop_assoc |> List.assoc "message"
         |> Json.drop_string |> print_endline;
     | _ ->
       print_endline call#response_status_text;
       print_endline call#response_body#value;
       (*print_endline "callback";*)
       ()
  );
  pipeline#run ();
  let (ans: string) = req#get_resp_body () in
  print_endline ans;
  Json.from_string ans


(* https://github.com/ethereum/wiki/wiki/JSON-RPC#json-rpc-api-reference *)

module Net =
struct

  let version ~uri =
    rpc_call uri "net_version" `Null
      
  let listening ~uri =
    rpc_call uri "net_listening" `Null

  let peer_count ~uri =
    rpc_call uri "net_peerCount" `Null    
  
end

module Eth =
struct

  type time =
    [`block of int | `latest | `earliest | `pending]


  let time_to_json (at_time : time) =
    match at_time with
    | `block i  ->
      `String (Json.hex_of_int i)
      (* let _ = failwith "TODO" in `Int i *)
    | `latest   -> `String "latest"
    | `earliest -> `String "earliest"
    | `pending  -> `String "pending"    
                       
  let protocol_version ~uri =
    rpc_call uri "eth_protocolVersion" `Null |> Json.int_as_string
  
  let syncing ~uri =
    rpc_call uri "eth_syncing" `Null |> Json.bool

  let coinbase ~uri =
    rpc_call uri "eth_coinbase" `Null |> Json.string |> Types.address_from_string

  let mining ~uri =
    rpc_call uri "eth_mining" `Null |> Json.bool

  let hashrate ~uri =
    rpc_call uri "eth_hashrate" `Null |> Json.bigint_as_string

  let gas_price ~uri =
    rpc_call uri "eth_gasPrice" `Null |> Json.bigint_as_string

  let accounts ~uri =
    rpc_call uri "eth_accounts" `Null
    |> Json.string_list
    |> List.map Types.address_from_string

  let block_number ~uri =
    rpc_call uri "eth_blockNumber" `Null |> Json.int_as_string

  let get_balance ~uri ~address ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [ `String (Types.address_to_string address); time ] in
    rpc_call uri "eth_getBalance" params |> Json.bigint_as_string

  let get_storage_at ~uri ~address ~position ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [ `String (Types.address_to_string address); `String (Z.to_string position); time ] in
    rpc_call uri "eth_getStorageAt" params |> Json.string

  let get_transaction_count ~uri ~address ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [ `String (Types.address_to_string address); time ] in
    rpc_call uri "eth_getTransactionCount" params |> Json.int

  let get_transaction_count_by_hash ~uri ~block_hash =
    let args = `List [`String (Types.hash256_to_string block_hash)] in
    rpc_call uri "eth_getTransactionCountByHash" args |> Json.int

  let get_transaction_count_by_number ~uri ~(at_time : time) =
    let args = `List [time_to_json at_time] in
    rpc_call uri "eth_getTransactionCountByNumber" args |> Json.int

  (* eth_getUncleCountByBlockHash, eth_getUncleCountByBlockNumber *)

  let get_code ~uri ~address ~(at_time : time) =
    let params = `List [ `String (Types.address_to_string address); time_to_json at_time ] in
    rpc_call uri "eth_getCode" params |> Json.string (* TODO: it would be nice to parse it back to bytecode *)

  let sign ~uri ~address ~message =
    rpc_call uri "eth_sign" (`List [`String (Types.address_to_string address); `String message]) |> Json.string

  let send_transaction ~uri ~transaction =
    let args = `List [Types.transaction_to_json transaction] in
    rpc_call uri "eth_sendTransaction" args |> Json.string |> Types.hash256_from_string

  let send_raw_transaction ~uri ~data =
    let args = `List [`String data] in
    rpc_call uri "eth_sendRawTransaction" args |> Json.string  |> Types.hash256_from_string

  let call ~uri ~transaction ~(at_time : time) =
    rpc_call uri "eth_call" (`List [ Types.transaction_to_json transaction; time_to_json at_time ]) |> Json.string

  (* estimateGas *)
  (* getBlockByHash/byNumber, etc *)
  (* getTransactionReceipt *)
  let get_transaction_receipt ~uri ~transaction_hash =
    rpc_call uri "eth_getTransactionReceipt" (`List [ `String (Types.hash256_to_string transaction_hash) ])
    |> Json.result
    |> Types.receipt_from_json

  let send_transaction_and_get_receipt ~uri ~transaction =
    let hash = send_transaction ~uri ~transaction in
    let rec loop () =
      match get_transaction_receipt ~uri ~transaction_hash:hash with
      | None ->
        Unix.sleep 1; loop ()
      | Some receipt ->
        receipt
    in
    loop ()

  let send_contract_and_get_receipt ~uri ~src ~data ~gas =
    let open Types in
    let tx =
      {
        src;
        dst = None;
        gas = Some gas;
        gas_price = None;
        value = None;
        data = Bitstr.hex_as_string data;
        nonce = None
      }
    in
    send_transaction_and_get_receipt ~uri ~transaction:tx
      
end
  
  
module Personal =
struct

  open Types

  let send_transaction ~uri ~src ~dst ~value ~src_pwd =
    let value = Json.hex_of_bigint value in
    let args  =
        `List [`Assoc [ ("from", `String (address_to_string src));
                        ("to", `String (address_to_string dst));
                        ("value", `String value) ];
               `String src_pwd
              ]
    in
    rpc_call uri "personal_sendTransaction" args
    |> Json.string
    |> hash256_from_string

  let new_account ~uri ~passphrase =
    let args  = `List [`String passphrase] in
    rpc_call uri "personal_newAccount" args
    |> Json.string
    |> address_from_string

  let unlock_account ~uri ~account ~passphrase ~unlock_duration =
    let args  =
      `List[`String (address_to_string account); `String passphrase; `Int unlock_duration]
    in
    rpc_call uri "personal_unlockAccount" args |> Json.bool
  
end

module Miner =
struct

  let set_gas_price ~uri ~gas_price =
    let args = `String (Json.hex_of_bigint gas_price) in
    rpc_call uri "miner_setGasPrice" (`List [args]) |> Json.bool

  let start ~uri ~thread_count =
    let args = `Int thread_count in
    rpc_call uri "miner_start" (`List [args]) |> Json.null

  let stop ~uri =
    rpc_call uri "miner_stop" `Null |> Json.bool

  let set_ether_base ~uri ~address =
    let args = `List [`String (Types.address_to_string address)] in
    rpc_call uri "miner_setEtherbase" args |> Json.bool

end

module Admin =
struct

  let add_peer ~uri ~peer_url =
    rpc_call uri "admin_addPeer" (`List [`String peer_url]) |> Json.bool

  let datadir ~uri =
    rpc_call uri "admin_datadir" `Null |> Json.string
      
  let node_info ~uri =
    rpc_call uri "admin_nodeInfo" `Null |> Json.result |> Types.node_info_from_json

  let peers ~uri =
    rpc_call uri "admin_peers" `Null |> Json.result |> Types.peer_info_from_json

end
