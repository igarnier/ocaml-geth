open Batteries

(* https://solidity.readthedocs.io/en/develop/abi-spec.html *)


module SolidityTypes =
struct

  type bitwidth = int (* mod 8 = 0, 0 < bitwidth <= 256*)

  type atomic =
    | Tuint of { w : bitwidth }
    | Tint of { w : bitwidth }
    | Taddress
    | Tbool
    (* signed fixed-point decimal number of M bits, 8 <= M <= 256, M % 8 ==0, and 0 < N <= 80, which denotes the value v as v / (10 ** N). *)      
    | Tfixed of { m : bitwidth; n : bitwidth }
    | Tufixed of { m : bitwidth; n : bitwidth }
    | Tbytes of { nbytes : int } (* 0 < #nbytes <= 32 *)

  and t =
    | Tatomic of atomic
    | Tfunction
    | Tstatic_array of { numel : int; typ : t }
    (* | Tdynamic_array of { typ : t } *)


end

module ABI =
struct

  type value =
    | Int    of int64
    | Bool   of bool
    | String of string

  type method_abi =
    {
      m_name       : string;     (* name of the method *)
      m_constant   : bool;       (* true if function is either Pure or View. Redundant??? *)
      m_inputs     : tuple_abi;
      m_outputs    : tuple_abi;
      m_payable    : bool;       (* true iff a function accepts ether *)
      m_mutability : mutability; (* purity annotation *)
      m_type       : mtype       (* Kind of method. Defaults to Function if omitted *)
    }

  and tuple_abi = named_arg list

  and named_arg = { arg_name : string;
                    arg_type : SolidityTypes.t }

  and mtype =
    | Function
    | Constructor
    | Callback

  and mutability =
    | Pure         (* specified to not read blockchain state *)
    | View         (* specified to not modify the blockchain state *)
    | Nonpayable
    | Payable

  (* -------------------------------------------------------------------------------- *)
  (* Encoding of types *)

  open Printf

  let rec encoding_of_type t =
    let open SolidityTypes in
    match t with
    | Tatomic atomic ->
      (match atomic with
       | Tuint { w } -> sprintf "uint%d" w
       | Tint { w }  -> sprintf "int%d" w
       | Taddress    -> "address"
       | Tbool       -> "bool"
       | Tfixed { m; n }   -> sprintf "fixed%dx%d" m n
       | Tufixed { m; n }  -> sprintf "ufixed%dx%d" m n
       | Tbytes { nbytes } -> sprintf "bytes%d" nbytes
      )
    | Tfunction         -> "bytes24"
    | Tstatic_array { numel; typ } ->
      sprintf "%s[%d]" (encoding_of_type typ) numel

  let string_of_signature { m_name; m_inputs } =
    let types =
      List.map (fun { arg_type } -> arg_type) m_inputs
    in
    let encodings =
      List.map encoding_of_type types
    in
    let elts = String.concat "," encodings in
    m_name^"("^elts^")"

  let method_id method_abi =
    let hash = Cryptokit.Hash.keccak 256 in
    let resl = Cryptokit.hash_string hash (string_of_signature method_abi) in
    let head = String.head resl 2 in
    Bitstr.bits_of_string head

  (* -------------------------------------------------------------------------------- *)

  (* Encoding of values *)
  let encode_int (i : int64) (t : SolidityTypes.atomic) =
    match t with
    | Tuint { w } ->
      if i < 0L then
        failwith "Contract.ABI.encode: cannot encode negative integer as unsigned int"
      else
        Bitstr.(zero_pad_to ~bits:(bits_of_int64 i) ~target_bits:256)
    | Tint { w } ->
      if i < 0L then
        Bitstr.(one_pad_to ~bits:(bits_of_int64 i) ~target_bits:256)
      else
        Bitstr.(zero_pad_to ~bits:(bits_of_int64 i) ~target_bits:256)
    | _ ->
      failwith "encode_int: incompatible Solidity type"
    
  let encode_value (v : value) (t : SolidityTypes.t) =
    let open SolidityTypes in
    match v with
    | Int i ->
      (match t with
       | Tatomic atomic ->
           encode_int i atomic
       | _ ->
         failwith "encode_value: type mismatch (bool)"
      )
    | Bool b ->
      (match t with
       | Tatomic Tbool ->
         encode_int 1L (Tuint { w = 32 })
       | _ ->
         failwith "encode_value: type mismatch (bool)")
    | String s ->
      (match t with
       | Tatomic Taddress -> (* 160 bits *)
         let encoded = Bitstr.compress (Bitstr.hex_of_string s) in
         Bitstr.zero_pad_to ~bits:encoded ~target_bits:256
       | _ ->
         failwith "encode_value: type mismatch (string)")

  (* -------------------------------------------------------------------------------- *)
  (* Deserialization of ABIs from solc --json output *)

  let method_type_of_json mtype =
    match mtype with
    | `String mtype ->
      (match mtype with
       | "function" -> Function
       | "constructor" -> Constructor
       | "callback" -> Callback
       | _ ->
         failwith ("method_type_of_json: incorrect method type "^mtype)       
      )
    | _ ->
      let dump = Json.to_string mtype in
      failwith ("type_of_json: can't decode "^dump)

  let mutability_of_string str =
    match str with
    | "pure" ->
      Pure
    | "view" ->
      View
    | "nonpayable" ->
      Nonpayable
    | "payable" ->
      Payable
    | _ ->
      failwith ("mutability_of_string: incorrect mutability type "^str)

  let type_of_json (json_type : Json.json) =
    match json_type with
    | `String s ->
      (match s with
       | "uint256" -> SolidityTypes.(Tatomic (Tuint { w = 256 }))
       | "int256 " -> SolidityTypes.(Tatomic (Tint { w = 256 }))
       | _ ->
         failwith ("type_of_json: can't decode "^s)
      )
    | _ ->
      let dump = Json.to_string json_type in
      failwith ("type_of_json: can't decode "^dump)

  let signature_of_json json =
    let json_args = Json.drop_list json in
    ListLabels.map json_args ~f:(fun argument ->
        let fields = Json.drop_assoc argument in
        let arg_name = List.assoc "name" fields |> Json.drop_string in
        let arg_type = List.assoc "type" fields |> type_of_json in
        { arg_name; arg_type }
      )

  let from_json json =
    ListLabels.map (Json.drop_list json) ~f:(fun method_abi ->
        let fields = Json.drop_assoc method_abi in
        let m_name     = List.assoc "name" fields |> Json.drop_string in
        let m_constant = List.assoc "constant" fields |> Json.drop_bool in
        let m_inputs   = List.assoc "inputs" fields |> signature_of_json in
        let m_outputs  = List.assoc "outputs" fields |> signature_of_json in
        let m_payable  = List.assoc "payable" fields |> Json.drop_bool in
        let m_mutability = List.assoc "stateMutability" fields |> Json.drop_string |> mutability_of_string in
        let m_type = List.assoc "type" fields |> method_type_of_json in
        {
          m_name; m_constant; m_inputs; m_outputs; m_payable; m_mutability; m_type
        }
      )

end

module Compile =
struct

  type solidity_output =
    {
      version   : string;
      contracts : solidity_contract list
    }

  and solidity_contract =
    {
      contract_name : string;
      bin           : string;
      abi           : ABI.method_abi list;
    }

  let exec_and_get_stdout command args =
    let output, input = Unix.pipe () in  
    let this_pid      = Unix.fork () in
    if this_pid < 0 then
      failwith "exec_and_get_stdout: error while forking"
    else if this_pid = 0 then begin
      Unix.dup2 input Unix.stdout;
      Unix.close output;
      Unix.close input;
      Unix.execvp command args
    end else begin
      Unix.close input;
      let _, status = Unix.wait () in
      match status with
      | WEXITED 0 ->
        let res = IO.read_all (Unix.in_channel_of_descr output) in
        Unix.close output;
        res
      | WEXITED n ->
        let m =
          Printf.sprintf "exec_and_get_stdout: abnormal termination of child process (code %d)" n
        in
        failwith m
      | WSIGNALED n ->
        let m =
          Printf.sprintf "exec_and_get_stdout: abnormal termination of child process (signal %d)" n
        in
        failwith m
      | WSTOPPED n ->
        let m =
          Printf.sprintf "exec_and_get_stdout: abnormal termination of child process (stopped %d)" n
        in
        failwith m          
    end

  let to_json ~filename =
    let result  = Json.from_string (exec_and_get_stdout "solc" [| "solc"; "--optimize"; "--combined-json"; "abi,bin,interface"; filename |]) in
    let fields  = Json.drop_assoc result in
    let version = List.assoc "version" fields |> Json.drop_string in
    let contracts = List.assoc "contracts" fields |> Json.drop_assoc in
    let contracts =
      List.map (fun (contract_name, contract_contents) ->
          let contents = Json.drop_assoc contract_contents in
          let bin = List.assoc "bin" contents |> Json.drop_string in
          let abi = List.assoc "abi" contents |> Json.drop_string |> Json.from_string |> ABI.from_json in
          { contract_name; bin; abi }
        ) contracts
    in
    { version; contracts }

  let deploy_rpc : uri:string -> account:Types.address -> passphrase:string -> gas:int -> contract:solidity_output -> Types.hash256 =
    fun ~uri ~account ~passphrase ~gas ~contract ->
      match contract.contracts with
      | [ ctx ] ->
        let bytecode =
          "0x"^ctx.bin
          |> Evm.parse_hexstring
          |> Evm.deploy
          |> Evm.dump
        in
        let open Types in
        let transaction =
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
        if Rpc.Personal.unlock_account ~uri ~account ~passphrase ~unlock_duration:300 then
          Rpc.Eth.send_transaction ~uri ~transaction
        else
          failwith "deploy_rpc: could not unlock account"
      | _ ->
        failwith "deploy_rpc: more than one contract in solidity_output"

  let call_method_tx ~(abi : ABI.method_abi) ~(args : ABI.value list) ~(src : Types.address) ~(ctx : Types.address) ~(gas : int) =
      let mname = abi.m_name in
      let inputs = abi.ABI.m_inputs in
      let siglen = List.length inputs in
      let arglen = List.length args in
      if siglen = arglen then
        let method_id = ABI.method_id abi in
        let args      =
          List.map2 (fun value { ABI.arg_type } ->
              ABI.encode_value value arg_type
            ) args inputs
        in
        let bitstring = Bitstr.concat (method_id :: args) in
        let data = Bitstr.(hex_as_string (uncompress bitstring)) in
        {
          Types.src; dst = Some ctx;  gas = Some gas; gas_price = None; value = None; data; nonce = None
        }
      else
        let m = Printf.sprintf
            "call_method: # of arguments mismatch for method %s: %d expected vs %d actual\n" mname siglen arglen
        in
        failwith m
         
end
