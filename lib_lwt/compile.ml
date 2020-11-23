module Rpc_lwt = Rpc
open Geth
open Contract

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

module X = Json_encoding.Make (Json_repr.Yojson)

let to_json ~filename =
  let raw_jsn =
    exec_and_get_stdout "solc"
      [|"solc"; "--optimize"; "--combined-json"; "abi,bin,interface"; filename|]
  in
  let result = Yojson.Safe.from_string raw_jsn in
  try X.destruct Contract.combined result
  with Json_encoding.Cannot_destruct (_path, exn) ->
    Format.kasprintf failwith "%a"
      (Json_encoding.print_error ?print_unknown:None)
      exn

let deploy_rpc ~(uri : string) ~(account : Types.Address.t) ~contract ~arguments
    ?gas ?value () =
  let prepare_constructor ({abi; bin} : contract) =
    let constr =
      List.find_map
        (function
          | ABI.Fun x when ABI.Fun.is_constructor x -> Some x | _ -> None)
        abi
      |> Option.get in
    let encoded =
      match arguments with
      | [] -> Bitstring.empty_bitstring
      | _ ->
          let open SolidityTypes in
          List.iter2
            (fun (v : SolidityValue.t) t ->
              if not (equal v.t t.ABI.t) then
                let arg_typ = t.ABI.t in
                Format.kasprintf failwith
                  "deploy_rpc: constructor argument types do not match \
                   constructor declaration: %a vs %a"
                  pp v.t pp arg_typ)
            arguments
            (Array.to_list constr.inputs) ;
          SolidityValue.(encode (tuple arguments)) in
    ABI.to_0x Bitstring.(concat [bin; encoded]) in
  let rec loop c =
    match c with
    | [] -> failwith "deploy_rpc: no contracts were deployable"
    | (_name, k) :: tl -> (
      match Bitstring.string_of_bitstring k.bin with
      | "" -> loop tl
      | _ ->
          let data = prepare_constructor k in
          Rpc_lwt.Eth.send_contract_and_get_receipt ~uri ~src:account ~data ?gas
            ?value () ) in
  loop contract.contracts

let call_method_tx ~(uri : string) ~(abi : ABI.Fun.t)
    ~(arguments : SolidityValue.t list) ~(src : Types.Address.t)
    ~(ctx : Types.Address.t) ?gas ?value () =
  let mname = abi.name in
  let inputs = abi.inputs in
  let siglen = Array.length inputs in
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
      (ABI.to_0x method_id) ;%lwt
    let encoded = SolidityValue.(encode (tuple arguments)) in
    let bitstring = Bitstring.concat [method_id; encoded] in
    let data = ABI.to_0x bitstring in
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
        let%lwt gas =
          Rpc_lwt.Eth.estimate_gas ~uri ~transaction:raw_transaction in
        Lwt.return {raw_transaction with gas= Some gas}

let call_void_method_tx ~mname ~(src : Types.Address.t) ~(ctx : Types.Address.t)
    ?gas () =
  let method_id = ABI.keccak_4_bytes mname |> Bitstring.bitstring_of_string in
  let data = ABI.to_0x method_id in
  let tx =
    { Types.Tx.src;
      dst= Some ctx;
      gas;
      gas_price= None;
      value= None;
      data;
      nonce= None } in
  Lwt.return tx

let execute_method ~(uri : string) ~(abi : ABI.Fun.t)
    ~(arguments : SolidityValue.t list) ~(src : Types.Address.t)
    ~(ctx : Types.Address.t) ?gas ?value () =
  let%lwt tx = call_method_tx ~uri ~abi ~arguments ~src ~ctx ?gas ?value () in
  Rpc_lwt.Eth.send_transaction_and_get_receipt ~uri ~transaction:tx

let call_method ~(uri : string) ~(abi : ABI.Fun.t)
    ~(arguments : SolidityValue.t list) ~(src : Types.Address.t)
    ~(ctx : Types.Address.t) ?gas ?value () =
  let%lwt tx = call_method_tx ~uri ~abi ~arguments ~src ~ctx ?gas ?value () in
  Rpc_lwt.Eth.call ~uri ~transaction:tx ~at_time:`latest
