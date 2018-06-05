open Mlparity
open Rpc

(* The RPC ports will typically be blocked from outside (they really should, for
   obvious security reasons). *)
let uri = "http://localhost:8545"

(* create a new account *)
let passphrase = "test"
let account = Personal.new_account ~uri ~passphrase

(* this account will receive the rewards for mining blocks *)
let _ = assert (Miner.set_ether_base ~uri ~address:account)

(* start mining *)
let _ = Miner.start ~uri ~thread_count:1

(* wait a bit ... *)

let _ = Unix.sleep 10

(* how rich are we *)
let _ =
  let balance = Eth.get_balance ~uri ~address:account ~at_time:`latest in
  Printf.printf "%s: %s wei\n" account (Z.to_string balance)

(* Create a new contract *)
let create_contract () =
  let program =
    (* An ASM program is a list of named blocks (the names are useful for jumping).
       Here, the program adds 4 to 5 and returns. *)
    let open Asm in
    [
      { name   = None;
        instrs =
          [ Push { width = 1 };
            Lit (Evm.literal_of_int 4);
            Push { width = 1 };
            Lit (Evm.literal_of_int 5);
            Add;
            (* save to address 0 in the local mem *)
            Push { width = 1 };
            Lit (Evm.literal_of_int 0);
            Mstore;
            (* specify that the result is between addesses 0 and 0+32 of the local mem *)
            Push { width = 1 };
            Lit (Evm.literal_of_int 32); (* Mstore stores words of length 32 bytes. *)
            Push { width = 1 };
            Lit (Evm.literal_of_int 0);
            Return ]
      }
    ]
  in
  (* Evm.deploy packs the code so that it can be saved to the blockchain.
     Otherwise, it will only be executed once. *)
  let bytecode = Evm.dump (Evm.deploy (Asm.to_bytecode program)) in
  Printf.printf "bytecode: %s" bytecode;
  let deploy_transaction =
    let open Types in
    {
      src = account;
      dst = None;
      gas = Some 100000;
      gas_price = None; (* there is a sensible default *)
      value = None;
      data = bytecode;
      nonce = None
    }
  in
  (* unlock account prior to deploying contract *)
  assert (Personal.unlock_account ~uri ~account ~passphrase ~unlock_duration:300);
  let transaction_hash = Eth.send_transaction ~uri ~transaction:deploy_transaction in
  Printf.printf "deploy tx hash: %s\n" transaction_hash;
  (* wait until tx is processed *)
  let receipt =
    let rec loop () =
      match Eth.get_transaction_receipt ~uri ~transaction_hash with
      | None         ->
        Unix.sleep 1;
        loop ()
      | Some receipt -> receipt
    in
    loop ()
  in
  let contract_addr =
    match receipt.contract_address with
    | None      -> failwith "Error while accessing contract address"
    | Some addr -> addr
  in
  Printf.printf "smart contract addr: %s\n" contract_addr;
  (* Check that contract code is correctly uploaded *)
  let code = Eth.get_code ~uri ~address:contract_addr ~at_time:`latest in
  Printf.printf "smart contract code: %s\n" code;
  contract_addr

(* call contract *)
let call contract_addr =
  let call_transaction =
    let open Types in
    {
      src = account;
      dst = Some (contract_addr);
      gas = Some 100000;
      gas_price = None; (* there is a sensible default *)
      value = None;
      data = ""; (* our contract needs no input *)
      nonce = None
    }
  in
  let result = Eth.call ~uri ~transaction:call_transaction ~at_time:`latest in
  Printf.printf "result: %s\n" result

let _ =
  call (create_contract ())
