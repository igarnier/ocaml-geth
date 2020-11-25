open CCFun
open Geth
open Types

let debug_flag = ref false
let switch_debug () = debug_flag := not !debug_flag

let print_debug s =
  if !debug_flag then Lwt_log.debug_f "Ocaml_geth.Rpc: %s" s
  else Lwt.return_unit

let rpc_call url method_name (params : Yojson.Safe.t) =
  let json : Yojson.Safe.t =
    `Assoc
      [ ("jsonrpc", `String "2.0"); ("method", `String method_name);
        ("params", params); ("id", `Int 0) (* TODO: this sould be a UID *) ]
  in
  let data = Yojson.Safe.to_string json in
  let headers = Cohttp.Header.of_list [("Content-type", "application/json")] in
  let body = Cohttp_lwt.Body.of_string data in
  print_debug (Printf.sprintf "Rpc.call: raw request =\n%s\n" data) ;%lwt
  let%lwt _resp, body = Cohttp_lwt_unix.Client.post url ~headers ~body in
  match body with
  | `Empty -> Lwt.fail_with "Ocaml_geth.Rpc.rpc_call: error, empty reply"
  | `Strings _ls -> Lwt.fail_with "Ocaml_geth.Rpc.rpc_call: error, string list"
  | `String _s -> Lwt.fail_with "Ocaml_geth.Rpc.rpc_call: error, string"
  | `Stream stream -> (
      let%lwt ls = Lwt_stream.to_list stream in
      match ls with
      | [] -> Lwt.fail_with "Ocaml_geth.Rpc.rpc_call: error, empty reply"
      | [reply] ->
          print_debug
            (Printf.sprintf "Ocaml_geth.Rpc.call: raw reply =\n%s\n" reply) ;%lwt
          Lwt.return (Yojson.Safe.from_string reply)
      | _ ->
          Lwt.fail_with
            "Ocaml_geth.Rpc.rpc_call: error, cannot interpret multi-part reply"
      )

(* let req = new Http_client.post_raw url data in
 * req#set_req_header "Content-type" "application/json";
 * let pipeline = new Http_client.pipeline in
 * let errormsg = ref None in
 * pipeline#add_with_callback req @@
 * (fun call -> match call#response_status with
 *    | `Ok -> ()
 *    | `Bad_request ->
 *      let response_body =
 *        Json.from_string call#response_body#value
 *        |> Json.drop_assoc
 *        |> List.assoc "message"
 *        |> Json.drop_string
 *      in
 *      errormsg := Some ("Bad_request:" ^ response_body)
 *    | _ ->
 *      let msg =
 *        "Response status text: \n" ^
 *        (call#response_status_text) ^ "\n" ^
 *        "Response vody: \n" ^
 *        (call#response_body#value)
 *      in
 *      errormsg := Some msg
 * );
 * pipeline#run ();
 * match !errormsg with
 * | None ->
 *   let (ans: string) = req#get_resp_body () in
 *   print_debug ans;%lwt
 *   Lwt.return (Json.from_string ans)
 * | Some error ->
 *   Lwt.fail_with error *)

(* let rpc_call url method_name (params : Json.json) =
 *   let open Yojson.Safe in
 *   let json : Json.json =
 *     `Assoc [ ("jsonrpc", `String "2.0");
 *              ("method", `String method_name);
 *              ("params", params);
 *              ("id", `Int 0); (\* TODO: this sould be a UID *\)
 *            ]
 *   in
 *   let data = Yojson.Safe.to_string json in
 *   print_debug (Printf.sprintf "Rpc.call: raw request =\n%s\n" data);%lwt
 *   let req = new Http_client.post_raw url data in
 *   req#set_req_header "Content-type" "application/json";
 *   let pipeline = new Http_client.pipeline in
 *   let errormsg = ref None in
 *   pipeline#add_with_callback req @@
 *   (fun call -> match call#response_status with
 *      | `Ok -> ()
 *      | `Bad_request ->
 *        let response_body =
 *          Json.from_string call#response_body#value
 *          |> Json.drop_assoc
 *          |> List.assoc "message"
 *          |> Json.drop_string
 *        in
 *        errormsg := Some ("Bad_request:" ^ response_body)
 *      | _ ->
 *        let msg =
 *          "Response status text: \n" ^
 *          (call#response_status_text) ^ "\n" ^
 *          "Response vody: \n" ^
 *          (call#response_body#value)
 *        in
 *        errormsg := Some msg
 *   );
 *   pipeline#run ();
 *   match !errormsg with
 *   | None ->
 *     let (ans: string) = req#get_resp_body () in
 *     print_debug ans;%lwt
 *     Lwt.return (Json.from_string ans)
 *   | Some error ->
 *     Lwt.fail_with error *)

(* https://github.com/ethereum/wiki/wiki/JSON-RPC#json-rpc-api-reference *)

module Get = Json.GetExn

(* module Net = struct
 *   let version ~url = rpc_call url "net_version" `Null
 *   let listening ~url = rpc_call url "net_listening" `Null
 *   let peer_count ~url = rpc_call url "net_peerCount" `Null
 * end *)

let ( |>> ) promise pure = Lwt.bind promise (fun x -> Lwt.return (pure x))

module Eth = struct
  type time = [`block of int | `latest | `earliest | `pending]

  let time_to_json (at_time : time) =
    match at_time with
    | `block i ->
        let s = Printf.sprintf "0x%x" i in
        `String s
    | `latest -> `String "latest"
    | `earliest -> `String "earliest"
    | `pending -> `String "pending"

  let protocol_version url =
    rpc_call url "eth_protocolVersion" `Null |>> Get.int_as_string

  let syncing url = rpc_call url "eth_syncing" `Null |>> Get.bool

  let coinbase url =
    rpc_call url "eth_coinbase" `Null |>> Address.of_0x % Get.string

  let mining url = rpc_call url "eth_mining" `Null |>> Get.bool
  let hashrate url = rpc_call url "eth_hashrate" `Null |>> Get.bigint_as_string
  let gas_price url = rpc_call url "eth_gasPrice" `Null |>> Get.bigint_as_string

  let accounts url =
    rpc_call url "eth_accounts" `Null
    |>> Get.string_list |>> List.map Address.of_0x

  let block_number url =
    rpc_call url "eth_blockNumber" `Null |>> Get.int_as_string

  let get_balance url ~address ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [`String (Address.show address); time] in
    rpc_call url "eth_getBalance" params |>> Get.bigint_as_string

  let get_storage_at url ~address ~position ~(at_time : time) =
    let time = time_to_json at_time in
    let params =
      `List
        [`String (Address.show address); `String (Z.to_string position); time]
    in
    rpc_call url "eth_getStorageAt" params |>> Get.string

  let get_transaction_count url ~address ~(at_time : time) =
    let time = time_to_json at_time in
    let params = `List [`String (Address.show address); time] in
    rpc_call url "eth_getTransactionCount" params |>> Get.int

  let get_transaction_count_by_hash url ~block_hash =
    let args = `List [`String (Hash256.show block_hash)] in
    rpc_call url "eth_getTransactionCountByHash" args |>> Get.int

  let get_transaction_count_by_number url ~(at_time : time) =
    let args = `List [time_to_json at_time] in
    rpc_call url "eth_getTransactionCountByNumber" args |>> Get.int

  (* eth_getUncleCountByBlockHash, eth_getUncleCountByBlockNumber *)

  let get_code url ~address ~(at_time : time) =
    let params = `List [`String (Address.show address); time_to_json at_time] in
    rpc_call url "eth_getCode" params |>> Get.string

  (* TODO: it would be nice to parse it back to bytecode *)

  let get_block_by_hash url ~block_hash =
    let params = `List [`String (Hash256.show block_hash)] in
    rpc_call url "eth_getBlockByHash" params |>> Get.result |>> assert false

  let get_block_by_number url ~at_time =
    let params = `List [time_to_json at_time; `Bool true] in
    rpc_call url "eth_getBlockByNumber" params |>> Get.result |>> assert false

  let sign url ~address ~message =
    rpc_call url "eth_sign"
      (`List [`String (Address.show address); `String message])
    |>> Get.string

  let send_transaction url ~transaction =
    let args = `List [Tx.to_json transaction] in
    rpc_call url "eth_sendTransaction" args |>> Get.string |>> Hash256.of_0x

  let send_raw_transaction url ~data =
    let args = `List [`String data] in
    rpc_call url "eth_sendRawTransaction" args |>> Get.string |>> Hash256.of_0x

  let call url ~transaction ~(at_time : time) =
    rpc_call url "eth_call"
      (`List [Tx.to_json transaction; time_to_json at_time])
    |>> Get.string

  let estimate_gas url ~transaction =
    try%lwt
      rpc_call url "eth_estimateGas" (`List [Tx.to_json transaction])
      |>> Get.bigint_as_string
    with exn ->
      let msg = "estimate_gas: error" in
      Lwt.fail_with @@ msg ^ "/" ^ Printexc.to_string exn

  (* getBlockByHash/byNumber, etc *)
  (* getTransactionReceipt *)
  let get_transaction_receipt url ~transaction_hash =
    rpc_call url "eth_getTransactionReceipt"
      (`List [`String (Hash256.show transaction_hash)])
    |>> Get.result |>> Tx.receipt_from_json

  let send_transaction_and_get_receipt url ~transaction =
    let%lwt hash = send_transaction url ~transaction in
    let rec loop () =
      match%lwt get_transaction_receipt url ~transaction_hash:hash with
      | None ->
          Lwt_log.debug "send_transaction_and_get_receipt: waiting ..." ;%lwt
          Lwt_unix.sleep 1.0 ;%lwt
          loop ()
      | Some receipt -> Lwt.return receipt
      | exception exn ->
          Lwt_log.debug
            "send_transaction_and_get_receipt: error in get_transaction_receipt\n" ;%lwt
          Lwt.fail exn in
    loop ()

  (* let send_contract_and_get_receipt_auto url ~src ~data ?value () =
   *   let open Tx in
   *   let tx =
   *     {
   *       src;
   *       dst   = None;
   *       gas   = None;
   *       gas_price = None;
   *       value;
   *       data  = Bitstr.Hex.to_string data;
   *       nonce = None
   *     }
   *   in
   *   let gas = estimate_gas url ~transaction:tx in
   *   let tx  = { tx with gas = Some gas } in
   *   send_transaction_and_get_receipt url ~transaction:tx *)

  let send_contract_and_get_receipt url ~src ~data ?gas ?value () =
    let open Tx in
    let tx =
      {src; dst= None; gas= None; gas_price= None; value; data; nonce= None}
    in
    let%lwt tx =
      match gas with
      | None ->
          let%lwt gas = estimate_gas url ~transaction:tx in
          Lwt.return {tx with gas= Some gas}
      | Some _ -> Lwt.return {tx with gas} in
    send_transaction_and_get_receipt url ~transaction:tx
end

(* module EthLwt =
 * struct
 *
 *   let send_transaction_and_get_receipt url ~transaction =
 *     let%lwt hash = Eth.send_transaction url ~transaction in
 *     let rec loop () =
 *       match%lwt Eth.get_transaction_receipt url ~transaction_hash:hash with
 *       | None ->
 *         Lwt_unix.sleep 2.0;%lwt
 *         loop ()
 *       | Some receipt ->
 *         Lwt.return receipt
 *       | exception err ->
 *         Lwt_io.eprintf "send_transaction_and_get_receipt: error in get_transaction_receipt\n";%lwt
 *         Lwt.fail err
 *     in
 *     loop ()
 *
 *   (\* let send_contract_and_get_receipt_auto url ~src ~data ?value () =
 *    *   let open Tx in
 *    *   let tx =
 *    *     {
 *    *       src;
 *    *       dst   = None;
 *    *       gas   = None;
 *    *       gas_price = None;
 *    *       value;
 *    *       data  = Bitstr.Hex.to_string data;
 *    *       nonce = None
 *    *     }
 *    *   in
 *    *   let gas = Eth.estimate_gas url ~transaction:tx in
 *    *   let tx  = { tx with gas = Some gas } in
 *    *   send_transaction_and_get_receipt url ~transaction:tx *\)
 *
 *   let send_contract_and_get_receipt url ~src ~data ?gas ?value () =
 *     let open Tx in
 *     let tx =
 *       {
 *         src;
 *         dst   = None;
 *         gas   = None;
 *         gas_price = None;
 *         value;
 *         data  = Bitstr.Hex.to_string data;
 *         nonce = None
 *       }
 *     in
 *     let tx =
 *       match gas with
 *       | None ->
 *         let%lwt gas = Eth.estimate_gas url ~transaction:tx in
 *         Lwt.return { tx with gas = Some gas }
 *       | Some _ ->
 *         Lwt.return { tx with gas }
 *     in
 *     send_transaction_and_get_receipt url ~transaction:tx
 *
 * end *)

module Personal = struct
  let send_transaction url ~src ~dst ~value ~src_pwd =
    (* let value = Bitstr.(hex_to_string (hex_of_bigint value)) in *)
    let args =
      `List
        [ `Assoc
            [ ("from", `String (Address.show src));
              ("to", `String (Address.show dst)); ("value", Json.zhex value) ];
          `String src_pwd ] in
    rpc_call url "personal_sendTransaction" args
    |>> Get.string |>> Hash256.of_0x

  let new_account url ~passphrase =
    let args = `List [`String passphrase] in
    rpc_call url "personal_newAccount" args |>> Get.string |>> Address.of_0x

  let unlock_account url ~account ~passphrase ~unlock_duration =
    let args =
      `List
        [ `String (Address.show account); `String passphrase;
          `Int unlock_duration ] in
    rpc_call url "personal_unlockAccount" args |>> Get.bool
end

module Miner = struct
  let set_gas_price url ~gas_price =
    rpc_call url "miner_setGasPrice" (`List [Json.zhex gas_price]) |>> Get.bool

  let start url ~thread_count =
    let args = `Int thread_count in
    rpc_call url "miner_start" (`List [args]) |>> Get.null

  let stop url = rpc_call url "miner_stop" `Null |>> ignore

  let set_ether_base url ~address =
    let args = `List [`String (Address.show address)] in
    rpc_call url "miner_setEtherbase" args |>> Get.bool
end

module Admin = struct
  let add_peer url ~peer_url =
    rpc_call url "admin_addPeer" (`List [`String peer_url]) |>> Get.bool

  let datadir url = rpc_call url "admin_datadir" `Null |>> Get.string

  let node_info url =
    rpc_call url "admin_nodeInfo" `Null
    |>> Get.result |>> Types.node_info_from_json

  let peers url =
    rpc_call url "admin_peers" `Null
    |>> Get.result |>> Types.peer_info_from_json
end

module Debug = struct
  let dump_block url ~block_number =
    let bnum = Printf.sprintf "0x%x" block_number in
    rpc_call url "debug_dumpBlock" (`List [`String bnum])
    |>> Get.result |>> Types.block_from_json
end
