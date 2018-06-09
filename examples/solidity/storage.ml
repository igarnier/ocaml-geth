open Ocaml_geth
open Contract

(* --------------------------------------------------------------------- *)
(* Deploying a smart contract *)
(* --------------------------------------------------------------------- *)

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
   Think of an auto-installer. 
   To do so, we parse the bin file back into EVM opcodes, prefix the opcodes adequately
   and dump back the result as a string.
*)
let deployable_bin =
  Evm.parse_hexstring ("0x"^bin)
  |> Evm.deploy
  |> Evm.dump

(* In order to deploy a contract, we need to send a specific transaction containing
   the deployable_bin. 
   We have to send the contract from a specific source account that is going to pay
   for the gas used during deployement.
*)
let deploy_tx ~(src : Types.address) ~(gas : int) ~(data : string) =
  let open Types in
  {
    src;
    dst = None;
    gas = Some gas;
    gas_price = None;
    value = None;
    data;
    nonce = None
  }

(* Let us say that there is a dummy source account with passphrase "dumdum",
   and with enough funds to pay for our experiments. In order to send the
   deploy transaction, we have to unlock its wallet. We assume there's a node
   running on this machine, with RPC services activated on the port 8545.
*)

let dummy_account : Types.address = failwith "" (* put the account address here *)

let uri = "http://localhost:8545"

let _ =
  if
    Rpc.Personal.unlock_account
      ~uri
      ~account:dummy_account
      ~passphrase:"dummy"
      ~unlock_duration:300
  then
    ()
  else
    failwith "Could not unlock account"

(* Send the tx *)
let transaction_hash =
  let tx = deploy_tx ~src:dummy_account ~gas:100000 ~data:deployable_bin in
  Rpc.Eth.send_transaction
    ~uri
    ~transaction:tx

(* Get transaction receipt. This might take a while. *)
let deploy_receipt =
  let rec wait () =
    match
      Rpc.Eth.get_transaction_receipt ~uri ~transaction_hash
    with
    | None ->
      Unix.sleep 1;
      wait ()
    | Some receipt -> receipt
  in
  wait ()

(* There is a convenience function that does the send and waits for the receipt:
   Rpc.Eth.send_transaction_and_get_receipt
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
  let set_abi = List.find (fun { ABI.m_name } -> m_name = "set") abi in
  Compile.call_method_tx
    ~abi:set_abi
    ~args:[ABI.Int 42L]
    ~src:dummy_account
    ~ctx:storage_ctx_address
    ~gas:10000

(* Send the transaction *)

let _ = 
  if
    Rpc.Personal.unlock_account
      ~uri
      ~account:dummy_account
      ~passphrase:"dummy"
      ~unlock_duration:300
  then
    ()
  else
    failwith "Could not unlock account"

let set_42_receipt =
  Rpc.Eth.send_transaction_and_get_receipt ~uri ~transaction:set_42_tx

(* How to check that 42 has effectively been set? We need to call the method
   get. There is a big difference here however: as get does not modify the
   state, we don't need to issue an actual transaction on the chain. Using
   Rpc.Eth.call, we can inspect the chain and get the result directly.
 *)

let get_tx =
  let get_abi = List.find (fun { ABI.m_name } -> m_name = "get") abi in
  Compile.call_method_tx
    ~abi:get_abi
    ~args:[]
    ~src:dummy_account
    ~ctx:storage_ctx_address
    ~gas:10000
