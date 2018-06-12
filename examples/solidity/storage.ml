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

  (* Compile solidity file using solc with the right options, parse the
     result back. *)
  let storage_contract =
    match Compile.to_json ~filename:"storage.sol" with
    | { Compile.contracts = [storage] } -> storage
    | _ ->
      failwith "More than one contract in file"

  (* Extract the contract: bin is the bytecode, abi specifies how to call
     methods from the contract. 
  *)
  let Compile.({ bin; abi }) = storage_contract

  (* Storing a contract on-chan requires prefixing it with some specific "deploy code".
     Think of an auto-installer. Contracts produced by solc are automatically prefixed
     by such "deploy code", so we don't need to do anything for that.
  *)

  (* In order to deploy a contract, we need to send a specific transaction containing
     the deployable_bin. We have to send the contract from a specific source account 
     that is going to pay for the gas used during deployement.
  *)
  let deploy_tx ~(src : Types.address) ~(gas : int) ~(data : string) =
    let open Types in
    {
      src;
      dst = None;
      gas = Some (Z.of_int gas);
      gas_price = None;
      value = None;
      data;
      nonce = None
    }

  (* open X *)

  let _ =
    if
      Rpc.Personal.unlock_account
        ~uri:X.uri
        ~account:X.account
        ~passphrase:(input_password X.account)
        ~unlock_duration:300
    then
      ()
    else
      failwith "Could not unlock account"

  (* Send the tx and get receipt: long version *)
  let deploy_receipt =
    let transaction_hash =
      let tx = deploy_tx ~src:X.account ~gas:999999 ~data:bin in
      Rpc.Eth.send_transaction
        ~uri:X.uri
        ~transaction:tx
    in
    (* Get transaction receipt. This might take a while. *)
    let rec wait () =
      match
        Rpc.Eth.get_transaction_receipt ~uri:X.uri ~transaction_hash
      with
      | None ->
        Unix.sleep 1;
        wait ()
      | Some receipt -> receipt
    in
    wait ()

  (* This is all already neatly packed in:

  let deploy_receipt =
    Rpc.Eth.send_contract_and_get_receipt ~uri:X.uri ~src:X.account ~data:bin ~gas:999999

  *)

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

  let set_42_tx =
    let uint32_t = SolidityTypes.(Tatomic (Tuint { w = Bits.int 32 })) in
    let ABI.Method set_abi =
      List.find (function | (ABI.Method { ABI.m_name }) -> m_name = "set"
                          | _ -> false
        ) abi
    in
    Compile.call_method_tx
      ~abi:set_abi
      ~args:[ABI.Int { v = 42L; t = uint32_t }]
      ~src:X.account
      ~ctx:storage_ctx_address
      ~gas:(Z.of_int 100000)

  (* Send the transaction *)

  let _ = 
    if
      Rpc.Personal.unlock_account
        ~uri:X.uri
        ~account:X.account
        ~passphrase:(input_password X.account)
        ~unlock_duration:300
    then
      ()
    else
      failwith "Could not unlock account"

  let set_42_receipt =
    Rpc.Eth.send_transaction_and_get_receipt ~uri:X.uri ~transaction:set_42_tx

  (* How to check that 42 has effectively been set? We need to call the method
     get. There is a big difference here however: as get does not modify the
     state, we don't need to issue an actual transaction on the chain. Using
     Rpc.Eth.call, we can inspect the chain and get the result directly.
  *)

  let get_tx =
    let ABI.Method get_abi =
      List.find (function | (ABI.Method { ABI.m_name }) -> m_name = "get"
                          | _ -> false
        ) abi
    in
    Compile.call_method_tx
      ~abi:get_abi
      ~args:[]
      ~src:X.account
      ~ctx:storage_ctx_address
      ~gas:(Z.of_int 10000)

  let get_42_result =
    Rpc.Eth.call ~uri:X.uri ~transaction:set_42_tx ~at_time:`latest

(* end *)

end
