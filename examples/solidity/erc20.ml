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

type mode =
  | Create of { initial_supply : int64; token_name : string; token_symbol : string }
  | Connect of { address : Types.address }


module Erc20
    (X : sig

       val account : Types.address
       val uri     : string

       val mode : mode

     end) =
struct  

  let _ =
    let passphrase = input_password X.account in
    Rpc.Personal.unlock_account ~account:X.account ~uri:"http://localhost:8545" ~passphrase ~unlock_duration:3600

  (* Compile solidity file using solc with the right options, parse the
     result back. This includes the binary code of the contract and its ABI. *)
  let solidity_output = Compile.to_json ~filename:"erc20.sol"

  let ctx_address =
    match X.mode with
    | Create { initial_supply; token_name; token_symbol } ->
      (* Get the contract address on chain *)  
      let deploy_receipt =
        Compile.deploy_rpc
          ~uri:X.uri
          ~account:X.account
          ~gas:(Z.of_int 999999)
          ~contract:solidity_output
          ~arguments:ABI.([ uint256_val initial_supply;
                            string_val token_name;
                            string_val token_symbol ])
      in
      (match deploy_receipt.Types.Tx.contract_address with
       | None ->
         failwith "could not get contract address from deploy receipt"
       | Some addr -> addr)
    | Connect { address } -> address

  let find_method mname =
    List.fold_left (fun acc ctx ->
        let res = Compile.get_method ctx mname in
        match res with
        | None -> acc
        | Some _ -> res
      ) None solidity_output.contracts

  (**
   * Transfer tokens
   *
   * Send `value` tokens to `dst` from your account
   *
   * @param _to The address of the recipient
   * @param _value the amount to send
  *)  
  let transfer : Types.address -> int64 -> Types.Tx.receipt =
    let transfer_abi =
      match find_method "transfer"  with
      | None -> failwith "transfer method not found in solidity output"
      | Some abi -> abi
    in
    fun dst value ->
      Compile.execute_method
        ~uri:X.uri
        ~abi:transfer_abi
        ~src:X.account
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)
        ~arguments:ABI.([ address_val dst;
                          uint256_val value
                        ])

  (**
   * Transfer tokens from other address
   *
   * Send `value` tokens to `dst` on behalf of `src`
   *
   * @param src The address of the sender
   * @param dst The address of the recipient
   * @param value the amount to send
  *)
  let transfer_from : Types.address -> Types.address -> int64 -> Types.Tx.receipt =
    let transfer_from_abi =
      match find_method "transferFrom"  with
      | None -> failwith "transferFrom method not found in solidity output"
      | Some abi -> abi
    in
    fun src dst value ->
      Compile.execute_method
        ~uri:X.uri
        ~abi:transfer_from_abi
        ~src:X.account
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)
        ~arguments:ABI.([ address_val src;
                          address_val dst;
                          uint256_val value
                        ])

  (**
   * Set allowance for other address
   *
   * Allows `spender` to spend no more than `value` tokens on your behalf
   *
   * @param spender The address authorized to spend
   * @param value the max amount they can spend
  *)
  let approve : Types.address -> int64 -> Types.Tx.receipt =
    let approve_abi =
      match find_method "approve"  with
      | None -> failwith "approve method not found in solidity output"
      | Some abi -> abi
    in
    fun spender value ->
      Compile.execute_method
        ~uri:X.uri
        ~abi:approve_abi
        ~src:X.account
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)
        ~arguments:ABI.([ address_val spender;
                          uint256_val value
                        ])

  (**
   * Set allowance for other address and notify
   *
   * Allows `spender` to spend no more than `value` tokens on your behalf, and then ping the contract about it
   *
   * @param spender The address authorized to spend
   * @param value the max amount they can spend
   * @param extra_data some extra information to send to the approved contract
  *)
  let approve_and_call : Types.address -> int64 -> string -> Types.Tx.receipt =
    let approve_and_call_abi =
      match find_method "approveAndCall"  with
      | None -> failwith "approveAndCall method not found in solidity output"
      | Some abi -> abi
    in
    fun spender value extra_data ->
      Compile.execute_method
        ~uri:X.uri
        ~abi:approve_and_call_abi
        ~src:X.account
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)
        ~arguments:ABI.([ address_val spender;
                          uint256_val value;
                          bytes_val extra_data
                        ])

  (** Get balance. The account [src] must be authentitcated before calling this
      function. *)
  let get_balance : Types.address -> string =
    let get_balance_abi =
      match find_method "getBalance"  with
      | None -> failwith "getBalance method not found in solidity output"
      | Some abi -> abi
    in
    fun src ->
      Compile.call_method
        ~uri:X.uri
        ~abi:get_balance_abi
        ~arguments:[]
        ~src
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)

  (**
   * Destroy tokens
   *
   * Remove `value` tokens from the system irreversibly
   *
   * @param value the amount of money to burn
  *)
  let burn : int64 -> Types.Tx.receipt =
    let burn_abi =
      match find_method "burn" with
      | None -> failwith "burn method not found in solidity output"
      | Some abi -> abi
    in
    fun value ->
      Compile.execute_method
        ~uri:X.uri
        ~abi:burn_abi
        ~src:X.account
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)
        ~arguments:ABI.([ uint256_val value ])

  (**
   * Destroy tokens from other account
   *
   * Remove `value` tokens from the system irreversibly on behalf of `from`.
   *
   * @param from the address of the sender
   * @param value the amount of money to burn
  *)
  let burn_from : Types.address -> int64 -> Types.Tx.receipt =
    let burn_from_abi =
      match find_method "burnFrom"  with
      | None -> failwith "burnFrom method not found in solidity output"
      | Some abi -> abi
    in
    fun from value ->
      Compile.execute_method
        ~uri:X.uri
        ~abi:burn_from_abi
        ~src:X.account
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)
        ~arguments:ABI.([ address_val from; uint256_val value ])


end
