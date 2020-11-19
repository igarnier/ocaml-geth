open Lwt.Infix
open Geth
open Geth_lwt
open Contract
open Compile

(* --------------------------------------------------------------------- *)
(* Some helpful functions *)
(* --------------------------------------------------------------------- *)

(* Read password from stdin in a stealthy manner *)
let read_secret () =
  let open Unix in
  let term_init = tcgetattr stdin in
  let term_no_echo = {term_init with c_echo= false} in
  tcsetattr stdin TCSADRAIN term_no_echo ;
  let password =
    try read_line ()
    with _ ->
      tcsetattr stdin TCSAFLUSH term_init ;
      failwith "read_secret: readline failed" in
  tcsetattr stdin TCSAFLUSH term_init ;
  password

let input_password (account : Types.Address.t) =
  Printf.printf "password for account %s: %!" (account :> string) ;
  let res = read_secret () in
  print_newline () ; res

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

module Storage (X : sig
  val account : Types.Address.t
  val uri : string
end) =
struct
  let _ =
    let passphrase = input_password X.account in
    Rpc.Personal.unlock_account ~account:X.account ~uri:"http://localhost:8545"
      ~passphrase ~unlock_duration:3600

  (* Compile solidity file using solc with the right options, parse the
     result back. This includes the binary code of the contract and its ABI. *)
  let solidity_output = Compile.to_json ~filename:"storage.sol"

  (* Get the contract address on chain *)
  let deploy_receipt () =
    deploy_rpc ~uri:X.uri ~account:X.account ~gas:(Z.of_int 175000)
      ~contract:solidity_output
      ~arguments:ABI.[uint256_val 0x123456L; string_val "This is a test"]
      ()

  let storage_ctx_address () =
    deploy_receipt ()
    >>= fun x ->
    match x.Types.Tx.contract_address with
    | None -> Lwt.fail_with "could not get contract address from deploy receipt"
    | Some addr -> Lwt.return addr

  (* --------------------------------------------------------------------- *)
  (* Calling a method from a solidity smart contract *)
  (* --------------------------------------------------------------------- *)

  let contract =
    match solidity_output.contracts with
    | [] | _ :: _ :: _ -> failwith "Storage: more than one contract"
    | [(_, ctx)] -> ctx

  let set =
    let set_abi =
      match Contract.find_function contract "set" with
      | None -> failwith "set method not found in solidity output"
      | Some abi -> abi in
    fun i ->
      storage_ctx_address ()
      >>= fun ctx ->
      execute_method ~uri:X.uri ~abi:set_abi ~arguments:[ABI.uint256_val i]
        ~src:X.account ~ctx ~gas:(Z.of_int 99999) ()
      >|= fun receipt -> ABI.Decode.decode_events contract.abi receipt

  let get =
    let get_abi =
      match Contract.find_function contract "get" with
      | None -> failwith "get method not found in solidity output"
      | Some abi -> abi in
    fun () ->
      storage_ctx_address ()
      >>= fun ctx ->
      call_method ~uri:X.uri ~abi:get_abi ~arguments:[] ~src:X.account ~ctx
        ~gas:(Z.of_int 99999) ()

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

module X = struct
  let account =
    Types.Address.from_string "0x0cb903d0139c1322a52f70038332efd363f94ea8"

  let uri = "http://localhost:8545"
end

module S = Storage (X)

let receipt = S.set 0x666L

let main () =
  S.storage_ctx_address ()
  >>= fun x ->
  Printf.printf "%s\n" (Types.Address.show x) ;
  S.get ()
  >>= fun res ->
  Printf.printf "result: %s\n" res ;
  Lwt.return_unit

let () = Lwt_main.run (main ())
