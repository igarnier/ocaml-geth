open Geth
open Contract

type solidity_output = {version: string; contracts: solidity_contract list}

and solidity_contract =
  {contract_name: string; bin: Bitstr.Bit.t; abi: ABI.abi list}

let exec_and_get_stdout command args =
  let output, input = Unix.pipe () in
  let this_pid = Unix.fork () in
  if this_pid < 0 then failwith "exec_and_get_stdout: error while forking"
  else if this_pid = 0 then (
    Unix.dup2 input Unix.stdout ;
    Unix.close output ;
    Unix.close input ;
    Unix.execvp command args )
  else (
    Unix.close input ;
    let _, status = Unix.wait () in
    match status with
    | WEXITED 0 ->
        let res = CCIO.read_all (Unix.in_channel_of_descr output) in
        Unix.close output ; res
    | WEXITED n ->
        let m =
          Printf.sprintf
            "exec_and_get_stdout: abnormal termination of child process (code \
             %d)"
            n in
        failwith m
    | WSIGNALED n ->
        let m =
          Printf.sprintf
            "exec_and_get_stdout: abnormal termination of child process \
             (signal %d)"
            n in
        failwith m
    | WSTOPPED n ->
        let m =
          Printf.sprintf
            "exec_and_get_stdout: abnormal termination of child process \
             (stopped %d)"
            n in
        failwith m )

let to_json ~filename =
  let raw_jsn =
    exec_and_get_stdout "solc"
      [|"solc"; "--optimize"; "--combined-json"; "abi,bin,interface"; filename|]
  in
  let result = Json.from_string raw_jsn in
  try
    let fields = Json.drop_assoc result in
    let version = assoc "version" fields |> Json.drop_string in
    let contracts = assoc "contracts" fields |> Json.drop_assoc in
    let contracts =
      List.map
        (fun (contract_name, contract_contents) ->
          let contents = Json.drop_assoc contract_contents in
          let bin = assoc "bin" contents |> Json.drop_string in
          let bin = Bitstr.(compress (Hex.of_string ("0x" ^ bin))) in
          let abi =
            assoc "abi" contents |> Json.drop_string |> Json.from_string
            |> ABI.from_json in
          {contract_name; bin; abi})
        contracts in
    {version; contracts}
  with Not_found ->
    Printf.printf "to_json: error while parsing json.\n" ;
    print_string raw_jsn ;
    exit 1

let get_constructor ctx =
  let constr_abi =
    List.fold_left
      (fun acc abi -> match abi with ABI.Constructor cs -> Some cs | _ -> acc)
      None ctx.abi in
  match constr_abi with
  | None -> failwith "get_constructor: constructor not found"
  | Some cs -> cs

let get_method ctx mname =
  List.fold_left
    (fun acc abi ->
      match abi with
      | ABI.Method ms -> if ms.ABI.m_name = mname then Some ms else acc
      | _ -> acc)
    None ctx.abi

let deploy_rpc ~(uri : string) ~(account : Types.Address.t)
    ~(contract : solidity_output) ~(arguments : ABI.value list) ?gas ?value () =
  let prepare_constructor ctx =
    let constr_abi = get_constructor ctx in
    let inputs = constr_abi.ABI.c_inputs in
    let encoded =
      match arguments with
      | [] -> Bitstring.empty_bitstring
      | _ ->
          List.iter2
            (fun v t ->
              if not (ABI.type_of v = t.ABI.arg_type) then
                let typeof_v = SolidityTypes.print (ABI.type_of v) in
                let arg_typ = SolidityTypes.print t.ABI.arg_type in
                failwith
                  ( "deploy_rpc: constructor argument types do not match \
                     constructor declaration: " ^ typeof_v ^ " vs " ^ arg_typ ))
            arguments inputs ;
          ABI.(Encode.encode (ABI.tuple_val arguments)) in
    Bitstr.(uncompress (Bit.concat [ctx.bin; encoded])) in
  let rec loop ctxs =
    match ctxs with
    | [] -> failwith "deploy_rpc: no contracts were deployable"
    | ctx :: tl -> (
      match Bitstr.Bit.as_string ctx.bin with
      | "" -> loop tl
      | _ ->
          let data = prepare_constructor ctx in
          Rpc.Eth.send_contract_and_get_receipt ~uri ~src:account ~data ?gas
            ?value () ) in
  loop contract.contracts

let call_method_tx ~(uri : string) ~(abi : ABI.method_abi)
    ~(arguments : ABI.value list) ~(src : Types.Address.t)
    ~(ctx : Types.Address.t) ?gas ?value () =
  let mname = abi.m_name in
  let inputs = abi.ABI.m_inputs in
  let siglen = List.length inputs in
  let arglen = List.length arguments in
  if siglen <> arglen then (
    let m =
      Printf.sprintf
        "call_method: # of arguments mismatch for method %s: %d expected vs %d \
         actual\n"
        mname siglen arglen in
    Lwt_log.debug_f "%s" m ;%lwt Lwt.fail_with m )
  else
    let method_id = ABI.method_id abi in
    Lwt_log.debug_f "calling method %s with code %s\n%!" mname
      (Bitstr.Hex.as_string (Bitstr.uncompress method_id)) ;%lwt
    let encoded = ABI.(Encode.encode (ABI.tuple_val arguments)) in
    let bitstring = Bitstr.Bit.concat [method_id; encoded] in
    let data = Bitstr.(Hex.as_string (uncompress bitstring)) in
    let raw_transaction =
      { Types.Tx.src;
        dst= Some ctx;
        gas= None;
        gas_price= None;
        value;
        data;
        nonce= None } in
    match gas with
    | Some _ -> Lwt.return {raw_transaction with gas}
    | None ->
        let%lwt gas = Rpc.Eth.estimate_gas ~uri ~transaction:raw_transaction in
        Lwt.return {raw_transaction with gas= Some gas}

let call_void_method_tx ~mname ~(src : Types.Address.t) ~(ctx : Types.Address.t)
    ?gas () =
  let method_id = ABI.keccak_4_bytes mname in
  let data = Bitstr.(Hex.as_string (uncompress method_id)) in
  let tx =
    { Types.Tx.src;
      dst= Some ctx;
      gas;
      gas_price= None;
      value= None;
      data;
      nonce= None } in
  Lwt.return tx

let execute_method ~(uri : string) ~(abi : ABI.method_abi)
    ~(arguments : ABI.value list) ~(src : Types.Address.t)
    ~(ctx : Types.Address.t) ?gas ?value () =
  let%lwt tx = call_method_tx ~uri ~abi ~arguments ~src ~ctx ?gas ?value () in
  Rpc.Eth.send_transaction_and_get_receipt ~uri ~transaction:tx

let call_method ~(uri : string) ~(abi : ABI.method_abi)
    ~(arguments : ABI.value list) ~(src : Types.Address.t)
    ~(ctx : Types.Address.t) ?gas ?value () =
  let%lwt tx = call_method_tx ~uri ~abi ~arguments ~src ~ctx ?gas ?value () in
  Rpc.Eth.call ~uri ~transaction:tx ~at_time:`latest
