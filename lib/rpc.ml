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
  ans
  
  
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
    

end
