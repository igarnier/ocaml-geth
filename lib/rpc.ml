open Batteries
    
module Http_client = Nethttp_client

let rpc_call uri method_name (params : Yojson.Basic.json) =
  let open Yojson.Basic in
  let json =
    `Assoc [ ("jsonrpc", `String "2.0");
             ("method", `String method_name);
             ("params", params);
             ("id", `Int 0); (* TODO: this sould be a UID *)
           ]
  in
  let data = Yojson.Basic.to_string json in
  Printf.printf "Rpc.call: raw request =\n%s\n" data;
  let req = new Http_client.post_raw uri data in
  req#set_req_header "Content-type" "application/json";
  let pipeline = new Http_client.pipeline in
  pipeline#add_with_callback req @@
  (fun call -> match call#response_status with
     | `Ok -> ()
     | `Bad_request ->
       print_endline call#response_body#value;
       let j = Yojson.Basic.from_string call#response_body#value in
       j |> Tools.drop_assoc |> List.assoc "message"
         |> Tools.drop_string |> print_endline;
     | _ ->
       print_endline call#response_status_text;
       print_endline call#response_body#value;
       (*print_endline "callback";*)
       ()
  );
  pipeline#run ();
  let (ans: string) = req#get_resp_body () in
  print_endline ans;
  Yojson.Basic.from_string ans


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


  let protocol_version ~uri =
    rpc_call uri "eth_protocolVersion" `Null |> Tools.int_as_string
  
  let syncing ~uri =
    rpc_call uri "eth_syncing" `Null |> Tools.bool

  let coinbase ~uri =
    rpc_call uri "eth_coinbase" `Null |> Tools.string

  let mining ~uri =
    rpc_call uri "eth_mining" `Null |> Tools.bool

  let hashrate ~uri =
    rpc_call uri "eth_hashrate" `Null |> Tools.int_as_string

  let gas_price ~uri =
    rpc_call uri "eth_gasPrice" `Null |> Tools.int_as_string

  let accounts ~uri =
    rpc_call uri "eth_accounts" `Null |> Tools.string_list

  let block_number ~uri =
    rpc_call uri "eth_blockNumber" `Null |> Tools.int_as_string

  type time =
    [`block of int | `latest | `earliest | `pending]

  let time_to_json (at_time : time) =
    match at_time with
      | `block i  -> `Int i
      | `latest   -> `String "latest"
      | `earliest -> `String "earliest"
      | `pending  -> `String "pending"    

  let get_balance ~uri ~address ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [ `String address; time ] in
    rpc_call uri "eth_getBalance" params |> Tools.bigint_as_string

  let get_storage_at ~uri ~address ~position ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [ `String address; `String (Z.to_string position); time ] in
    rpc_call uri "eth_getStorageAt" params |> Tools.string

  let get_transaction_count ~uri ~address ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [ `String address; time ] in
    rpc_call uri "eth_getTransactionCount" params |> Tools.int

  let get_transaction_count_by_hash ~uri ~block_hash =
    let args = `List [`String block_hash] in
    rpc_call uri "eth_getTransactionCountByHash" args |> Tools.int

  let get_transaction_count_by_number ~uri ~(at_time : time) =
    let args = `List [time_to_json at_time] in
    rpc_call uri "eth_getTransactionCountByNumber" args |> Tools.int

  (* eth_getUncleCountByBlockHash, eth_getUncleCountByBlockNumber *)

  let get_code ~uri ~address ~(at_time : time) =
    let params = `List [ `String address; time_to_json at_time ] in
    rpc_call uri "eth_getCode" params |> Tools.string (* TODO: it would be nice to parse it back to bytecode *)

  let sign ~uri ~address ~message =
    rpc_call uri "eth_sign" (`List [`String address; `String message]) |> Tools.string

  let send_transaction ~uri ~transaction =
    let args = `List [Types.transaction_to_json transaction] in
    rpc_call uri "eth_sendTransaction" args |> Tools.string

  let send_raw_transaction ~uri ~data =
    let args = `List [`String data] in
    rpc_call uri "eth_sendRawTransaction" args |> Tools.string

  let call ~uri ~transaction ~(at_time : time) =
    rpc_call uri "eth_call" (`List [ Types.transaction_to_json transaction; time_to_json at_time ]) |> Tools.string

  (* estimateGas *)
  (* getBlockByHash/byNumber, etc *)
  (* getTransactionReceipt *)
  let get_transaction_receipt ~uri ~transaction_hash =
    rpc_call uri "eth_getTransactionReceipt" (`List [ `String transaction_hash ])
    |> Tools.result
    |> Types.receipt_from_json
      
end
  
  
module Personal =
struct
  
  let send_transaction ~uri ~src ~dst ~value ~src_pwd =
    let value = Tools.hex_of_int value in
    let args  =
        `List [`Assoc [ ("from", `String src);
                        ("to", `String dst);
                        ("value", `String value) ];
               `String src_pwd
              ]
    in
    rpc_call uri "personal_sendTransaction" args |> Tools.string

  let new_account ~uri ~passphrase =
    let args  = `List [`String passphrase] in
    rpc_call uri "personal_newAccount" args |> Tools.string

  let unlock_account ~uri ~account ~passphrase ~unlock_duration =
    let args  =
      `List[`String account; `String passphrase; `Int unlock_duration]
    in
    rpc_call uri "personal_unlockAccount" args |> Tools.bool
  
end

module Miner =
struct

  let set_gas_price ~uri ~gas_price =
    let args = `String (Tools.hex_of_int gas_price) in
    rpc_call uri "miner_setGasPrice" (`List [args])

  let start ~uri ~thread_count =
    let args = `Int thread_count in
    rpc_call uri "miner_start" (`List [args]) |> Tools.null

  let stop ~uri =
    rpc_call uri "miner_stop" `Null |> Tools.bool

  let set_ether_base ~uri ~address =
    let args = `List [`String address] in
    rpc_call uri "miner_setEtherbase" args |> Tools.bool
  
end

module Admin =
struct

  let add_peer ~uri ~peer_url =
    rpc_call uri "admin_addPeer" (`List [`String peer_url])

  let datadir ~uri =
    rpc_call uri "admin_datadir" `Null

  let node_info ~uri =
    rpc_call uri "admin_nodeInfo" `Null

  let peers ~uri =
    rpc_call uri "admin_peers" `Null

end
