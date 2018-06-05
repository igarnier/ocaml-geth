open Batteries
    
module Http_client = Nethttp_client

module Utils =
struct
  let drop_assoc  = function `Assoc xs -> xs | _ -> failwith "Bad argument"
  let drop_string = function `String s -> s | _ -> failwith "Bad argument"
  let drop_int    = function `Int n -> n | _ -> failwith "Bad argument"
  let drop_list   = function `List xs -> xs | _ -> failwith "Bad argument"
  let unwrap_res x = x |> drop_assoc |> List.assoc "data"
end

let call uri method_name params =
  let open Yojson.Basic in
  let json =
    `Assoc [ ("jsonrpc", `String "2.0");
             ("method", `String method_name);
             ("params", params);
             ("id", `Int 0); (* TODO: this sould be a UID *)
           ]
  in
  let data = Yojson.to_string json in
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
       j |> Utils.drop_assoc |> List.assoc "message"
         |> Utils.drop_string |> print_endline;
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
    call uri "net_version" `Null
      
  let listening ~uri =
    call uri "net_listening" `Null

  let peer_count ~uri =
    call uri "net_peerCount" `Null    
  
end

module Eth =
struct

  let protocol_version ~uri =
    call uri "eth_protocolVersion" `Null    
  
  let syncing ~uri =
    call uri "eth_syncing" `Null

  let coinbase ~uri =
    call uri "eth_coinbase" `Null

  let mining ~uri =
    call uri "eth_mining" `Null

  let hashrate ~uri =
    call uri "eth_hashrate" `Null

  let gas_price ~uri =
    call uri "eth_gasPrice" `Null

  let accounts ~uri =
    call uri "eth_accounts" `Null

  let block_number ~uri =
    call uri "eth_blockNumber" `Null

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
    call uri "eth_getBalance" params

  let get_storage_at ~uri ~address ~position ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [ `String address; `Int position; time ] in
    call uri "eth_getStorageAt" params

  let get_transaction_count ~uri ~address ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [ `String address; time ] in
    call uri "eth_getTransactionCount" params

  let get_transaction_count_by_hash ~uri ~block_hash =
    call uri "eth_getTransactionCountByHash" (`String block_hash)

  let get_transaction_count_by_number ~uri ~(at_time : time) =
    call uri "eth_getTransactionCountByNumber" (time_to_json at_time)

  (* eth_getUncleCountByBlockHash, eth_getUncleCountByBlockNumber *)

  let get_code ~uri ~address ~(at_time : time) =
    let params = `List [ `String address; time_to_json at_time ] in
    call uri "eth_getCode" params

  let sign ~uri ~address ~message =
    call uri "eth_sign" (`List [`String address; `String message])

  let send_transaction ~uri ~transaction =
    let open Types in
    let args =
      [
        ("from", `String transaction.src);
        ("to", `String transaction.dst);
      ] @
      (match transaction.gas with Some x -> [("gas", `Int x)] | _ -> []) @
      (match transaction.gas_price with Some x -> [("gasPrice", `Int x)] | _ -> []) @
      (match transaction.value with Some x -> [("value", `Int x)] | _ -> []) @
      [("data", `String transaction.data)] @
      (match transaction.nonce with Some x -> [("nonce", `Int x)] | _ -> [])
    in
    call uri "eth_sendTransaction" (`Assoc args)

  (* sendRawTransaction *)
  (* call *)
  (* estimateGas *)
  (* getBlockByHash/byNumber, etc *)
  (* getTransactionReceipt *)
  
end
  
  
module Personal =
struct
  
  let send_transaction ~uri ~src ~dst ~value ~src_pwd =
    let value = Printf.sprintf "0x%x" value in
    let args  =
        `List [`Assoc [ ("from", `String src);
                        ("to", `String dst);
                        ("value", `String value) ];
               `String src_pwd
              ]
    in
    call uri "personal_sendTransaction" args

  let new_account ~uri ~passphrase =
    let args  = `String passphrase in
    call uri "personal_newAccount" args

  let unlock_account ~uri ~account ~passphrase ~unlock_duration =
    let args  =
        `List[`String passphrase; `String passphrase; `Int unlock_duration]
    in
    call uri "personal_newAccount" args
  
end

module Miner =
struct

  let set_gas_price ~uri ~gas_price =
    let args = `Int gas_price in
    call uri "miner_setGasPrice" args

  let start ~uri ~thread_count =
    let args = `Int thread_count in
    call uri "miner_start" args    

  let stop ~uri =
    call uri "miner_stop" `Null

  let set_ether_base ~uri ~address =
    call uri "miner_setEtherBase" (`String address)    
  
end

module Admin =
struct

  let add_peer ~uri ~peer_url =
    call uri "admin_addPeer" (`String peer_url)

  let datadir ~uri =
    call uri "admin_datadir" `Null

  let node_info ~uri =
    call uri "admin_nodeInfo" `Null

  let peers ~uri =
    call uri "admin_peers" `Null

end
