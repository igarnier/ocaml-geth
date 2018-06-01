module Http_client = Nethttp_client

module YoUtil = struct
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
       j |> YoUtil.drop_assoc |> List.assoc "message"
         |> YoUtil.drop_string |> print_endline;
     | _ ->
       print_endline call#response_status_text;
       print_endline call#response_body#value;
       (*print_endline "callback";*)
       ()
  );
  pipeline#run ();
  let (ans: string) = req#get_resp_body () in
  print_endline ans
  
  
