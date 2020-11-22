open Alcotest
open Geth
open Geth_lwt

(* open Mlparity
 * 
 * module Test_Deploy (X : sig end) = struct
 *   let genesis =
 *     GethInit.Genesis.
 *       { config=
 *           {chain_id= 0; homestead_block= 0; eip155_block= 0; eip158_block= 0};
 *         alloc= [];
 *         coinbase= "0x0000000000000000000000000000000000000000";
 *         difficulty= 1;
 *         extra_data= "";
 *         gas_limit= 0x2fefd8;
 *         nonce= 0x107;
 *         mix_hash=
 *           "0x0000000000000000000000000000000000000000000000000000000000000000";
 *         parent_hash=
 *           "0x0000000000000000000000000000000000000000000000000000000000000000";
 *         timestamp= 0 }
 * 
 *   let conf =
 *     let open GethInit in
 *     { genesis_block= genesis;
 *       network_id= 8798798;
 *       root_directory= "priveth";
 *       data_subdir= "data";
 *       source_subdir= "source" }
 * 
 *   let password =
 *     Printf.printf "password: %!" ;
 *     Ssh_client.Easy.read_secret ()
 * 
 *   let ilias_on_xps13 =
 *     {GethInit.ip_address= "127.0.0.1"; ssh_port= 22; login= "ilias"; password}
 * 
 *   let _ =
 *     let enode = GethInit.start_no_discover conf ilias_on_xps13 in
 *     Printf.printf "enode: %s\n" enode
 * end *)

module Test_Asm (X : sig end) = struct
  let code =
    let open Asm in
    [ { name= Some "block0";
        instrs=
          [ Push {width= 1}; Lit (Evm.literal_of_int 0); Push {width= 1};
            Lit (Evm.literal_of_int 1); Add; Jumpi {block= "block0"} ] } ]

  let result = Asm.to_bytecode code
  let string_result = Evm.dump result
  let _ = Printf.printf "%s\n%!" string_result

  let example =
    let open Evm in
    [ Instr PUSH1; Literal (Evm.literal_of_int 5); Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr ADD; Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr ADD; Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr ADD; Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr ADD; Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr ADD; Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr ADD; Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr PUSH1;
      Literal (Evm.literal_of_int 5); Instr ADD ]

  let _ = Printf.printf "%s\n%!" Evm.(dump (deploy example))
end

module A = Test_Asm ()

let compile fn () = ignore (Compile.to_json ~filename:fn)
let contracts = ["helloworld.sol"]
let compile = List.map (fun s -> (s, `Quick, compile s)) contracts

let jsons =
  [ "IERC20.json"; "IUniswapV2Callee.json"; "IUniswapV2ERC20.json";
    "IUniswapV2Factory.json"; "IUniswapV2Pair.json" ]

let logs =
  [ {|{"removed":false,"logIndex":"0x23","transactionIndex":"0x19","transactionHash":"0xef1b5f072b1ad94164395eb20098af29017d3a5b536122f584f786bf4f6b0547","blockHash":"0x80ca3b76ebf523f09186f28d13286710656dfca1f7acd3df81cf31c22f843766","blockNumber":"0xac4863","address":"0x1f9840a85d5aF5bf1D1762F925BDADdC4201F984","data":"0x000000000000000000000000000000000000000000000001d790872df723f800","topics":["0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef","0x0000000000000000000000001ad3252d94343997a32fd23e45ea5b0bad8a6b7f","0x000000000000000000000000376976bfe2002a4bff84ab11beac2a62a0642a90"]}|}
  ]

module X = Json_encoding.Make (Json_repr.Yojson)

let ofJson e json () =
  try ignore (X.destruct e json)
  with Json_encoding.Cannot_destruct (_path, exn) ->
    Format.kasprintf failwith "%a"
      (Json_encoding.print_error ?print_unknown:None)
      exn

let contract =
  List.map
    (fun s -> (s, `Quick, ofJson Contract.simple (Yojson.Safe.from_file s)))
    jsons

let log =
  List.map
    (fun s ->
      (s, `Quick, ofJson Types.Log.encoding (Yojson.Safe.from_string s)))
    logs

let parse_tests =
  [ "int"; "uint"; "int32"; "uint32"; "address"; "bool"; "fixed"; "fixed12x12";
    "ufixed12x12"; "string"; "function"; "int[]"; "(int,int)" ]

let roundtrip s =
  let roundtrip = Contract.SolidityTypes.(to_string (of_string_exn s)) in
  check string s s roundtrip

let parse = [("basic", `Quick, fun () -> List.iter roundtrip parse_tests)]

let () =
  Alcotest.run "geth"
    [ ("parser", parse); ("compile", compile); ("contract", contract);
      ("log", log) ]
