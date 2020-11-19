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

let parse_tests =
  [ "int"; "uint"; "int32"; "uint32"; "address"; "bool"; "fixed"; "fixed12x12";
    "ufixed12x12"; "string"; "function"; "int[]"; "(int,int)" ]

let roundtrip s =
  let roundtrip = Contract.SolidityTypes.(to_string (of_string_exn s)) in
  check string s s roundtrip

let parse = [("basic", `Quick, fun () -> List.iter roundtrip parse_tests)]
let () = Alcotest.run "geth" [("parser", parse); ("compile", compile)]
