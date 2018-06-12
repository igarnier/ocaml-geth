open Batteries

open Ocaml_geth
open Basic (* for Bits and Bytes *)
open Contract

(* --------------------------------------------------------------------- *)
(* Some helpful functions *)
(* --------------------------------------------------------------------- *)

(* Read password from stdin in a stealthy manner *)
let read_secret () =
  let open Unix in
  let term_init = tcgetattr stdin in
  let term_no_echo = { term_init with c_echo = false } in
  tcsetattr stdin TCSADRAIN term_no_echo;
  let password =
    try read_line ()
    with _ ->
      (tcsetattr stdin TCSAFLUSH term_init;
       failwith "read_secret: readline failed")
  in 
  tcsetattr stdin TCSAFLUSH term_init;
  password

let input_password (account : Types.address) =
  Printf.printf "password for account %s: %!" (account :> string);
  let res = read_secret () in
  print_newline ();
  res

(* --------------------------------------------------------------------- *)
(* Deploying a smart contract. We functorize the code over some global
   parameters (like the creating account). *)
(* --------------------------------------------------------------------- *)

(* Testing ABI argument encoding *)
(* open SolidityTypes
 * 
 * let _ =
 *   let string_t = Tatomic Tstring in
 *   let uint_t   = Tatomic (Tuint { w = Bits.int 32 }) in
 *   let res = ABI.encode_value (ABI.Tuple [ ABI.Int { v = 123L; t = uint_t };
 *                                           ABI.Int { v = 456L; t = uint_t };
 *                                           ABI.String { v = "thequickbrownfoxjumpsoverthelazydog"; t = string_t };
 *                                           ABI.String { v = "shesellsseashellsontheseashore"; t = string_t }
 *                                         ])
 *   in
 *   Printf.printf "%s\n" (Bitstr.(hex_as_string (uncompress res))) *)


module Storage
    (X : sig

       val account : Types.address
       val uri     : string

     end) =
struct  

  let _ =
    let passphrase = input_password X.account in
    Rpc.Personal.unlock_account ~account:X.account ~uri:"http://localhost:8545" ~passphrase ~unlock_duration:3600

  (* Compile solidity file using solc with the right options, parse the
     result back. *)
  let solidity_output = Compile.to_json ~filename:"storage.sol"

  (* Extract the contract: bin is the bytecode, abi specifies how to call
     methods from the contract. *)
  let deploy_receipt =
    Compile.deploy_rpc
      ~uri:X.uri
      ~account:X.account
      ~gas:(Z.of_int 9999999)
      ~contract:solidity_output
      ~arguments:ABI.([ Int { v = 0x123456L; t = SolidityTypes.uint_t 256 };
                        String { v = "This is a test"; t = SolidityTypes.string_t }
                      ])

  
  (* Get the contract address on chain *)
  let storage_ctx_address =
    match deploy_receipt.Types.contract_address with
    | None ->
      failwith "could not get contract address from deploy receipt"
    | Some addr -> addr

  (* --------------------------------------------------------------------- *)
  (* Calling a method from a solidity smart contract *)
  (* --------------------------------------------------------------------- *)    

  (* Let's say we want to call "set" with argument data = 42. We need to
     encode this into a transaction and send it to the contract address,
     with enough gas for the method to complete its execution.  *)

  (* The encoding takes the form enc(method_signature) enc(arguments) where
     juxtaposition means concatenation of bitstrings and
     1) enc(method_signature), called the "function selector", consists of 
     the 4 first bytes of the keccak256 hash of the string representation 
     of the signature of the method, which is here "set(uint256)", in 
     hexadecimal notation.
     2) enc(arguments) is specified in the solidity ABI. For unstructured
     arguments (such as ints) it is just the hexadecimal representation.

     The keccack256 hash of "set(uint256)" is 
     "60fe47b16ed402aae66ca03d2bfc51478ee897c26a1158669c7058d5f24898f4"
     hence enc("set(uint256)") = "60fe47b1",
     while the argument 42 is encoded in hexadecimal as 2A, which we must 
     0-pad to 256 bits. The encoding of calling set with argument 42 is thus
     "0x60fe47b1000000....00002A". This string will constitute the data
     section of the transaction corresponding to the call.
  *)

  let find_method mname =
    List.fold_left (fun acc ctx ->
        let res = Compile.get_method ctx mname in
        match res with
        | None -> acc
        | Some _ -> res
      ) None solidity_output.contracts

  let set =
    let set_abi =
      match find_method "set"  with
      | None -> failwith "set method not found in solidity output"
      | Some abi -> abi
    in
    fun i ->
      let tx =
        Compile.call_method_tx
          ~abi:set_abi
          ~arguments:[ABI.Int { v = i; t = SolidityTypes.uint_t 256 }]
          ~src:X.account
          ~ctx:storage_ctx_address
          ~gas:(Z.of_int 99999)
      in
      Rpc.Eth.send_transaction_and_get_receipt ~uri:X.uri ~transaction:tx

  let get =
    let get_abi =
      match find_method "get"  with
      | None -> failwith "get method not found in solidity output"
      | Some abi -> abi
    in
    fun () ->
      let tx =
        Compile.call_method_tx
          ~abi:get_abi
          ~arguments:[]
          ~src:X.account
          ~ctx:storage_ctx_address
          ~gas:(Z.of_int 99999)
      in
      Rpc.Eth.call ~uri:X.uri ~transaction:tx ~at_time:`latest

  (* let _ = 
   *   if
   *     Rpc.Personal.unlock_account
   *       ~uri:X.uri
   *       ~account:X.account
   *       ~passphrase:(input_password X.account)
   *       ~unlock_duration:300
   *   then
   *     ()
   *   else
   *     failwith "Could not unlock account" *)

end
