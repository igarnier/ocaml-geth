open Batteries

(* https://solidity.readthedocs.io/en/develop/abi-spec.html *)


module SolidityTypes =
struct

  type bitwidth = int (* mod 8 = 0, 0 < bitwidth <= 256*)

  type t =
    | Tuint of { w : bitwidth }
    | Tint of { w : bitwidth }
    | Taddress
    | Tbool
    (* signed fixed-point decimal number of M bits, 8 <= M <= 256, M % 8 ==0, and 0 < N <= 80, which denotes the value v as v / (10 ** N). *)      
    | Tfixed of { m : bitwidth; n : bitwidth }
    | Tufixed of { m : bitwidth; n : bitwidth }
    | Tbytes of { nbytes : int } (* 0 < #nbytes <= 32 *)
    | Tfunction
    | Tstatic_array of { numel : int; typ : t }
    (* | Tdynamic_array of { typ : t } *)

end

module ABI =
struct

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

  let rec encoding_of_type =
    let open SolidityTypes in
    function
    | Tuint { w } -> sprintf "uint%d" w
    | Tint { w }  -> sprintf "int%d" w
    | Taddress    -> "address"
    | Tbool       -> "bool"
    | Tfixed { m; n }   -> sprintf "fixed%dx%d" m n
    | Tufixed { m; n }  -> sprintf "ufixed%dx%d" m n
    | Tbytes { nbytes } -> sprintf "bytes%d" nbytes
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
    Cryptokit.hash_string hash (string_of_signature method_abi)

  (* -------------------------------------------------------------------------------- *)
  open Bitstring
      
  let int64_to_bitstring (i : int64) =
    let%bitstring result =
    {|
      i : 64
    |}
    in
    result

  let zero_bitstring nbytes =
    Bitstring.zeroes_bitstring (nbytes * 8)

  (* Encoding of values *)
  let encode_int (i : int64) (t : SolidityTypes.t) =
    match t with
    | Taddress | Tfunction | Tstatic_array _ ->
      failwith "Contract.ABI.encode: wrong type for integer value"
    | Tuint { w } ->
      if i < 0L then
        failwith "Contract.ABI.encode: cannot encode negative integer as unsigned int"
      else
        Bitstring.concat [zero_bitstring 24; int64_to_bitstring i]
    | Tint { w } ->
      Bitstring.concat [zero_bitstring 24; int64_to_bitstring i]
    | _ ->
      failwith "encode_int: incompatible Solidity type"

  (* -------------------------------------------------------------------------------- *)
  (* Deserialization of ABIs from solc --json output *)

  let method_type_from_json mtype =
    match mtype with
    | `String mtype ->
      (match mtype with
       | "function" -> Function
       | "constructor" -> Constructor
       | "callback" -> Callback
       | _ ->
         failwith ("method_type_from_json: incorrect method type "^mtype)       
      )
    | _ ->
      let dump = Json.to_string mtype in
      failwith ("type_from_json: can't decode "^dump)

  let mutability_from_string str =
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
      failwith ("mutability_from_string: incorrect mutability type "^str)

  let type_from_json (json_type : Json.json) =
    match json_type with
    | `String s ->
      (match s with
       | "uint256" -> SolidityTypes.Tuint { w = 256 }
       | "int256 " -> SolidityTypes.Tint { w = 256 }
       | _ ->
         failwith ("type_from_json: can't decode "^s)
      )
    | _ ->
      let dump = Json.to_string json_type in
      failwith ("type_from_json: can't decode "^dump)

  let signature_from_json json =
    let json_args = Json.drop_list json in
    ListLabels.map json_args ~f:(fun argument ->
        let fields = Json.drop_assoc argument in
        let arg_name = List.assoc "name" fields |> Json.drop_string in
        let arg_type = List.assoc "type" fields |> type_from_json in
        { arg_name; arg_type }
      )

  let from_json json =
    ListLabels.map (Json.drop_list json) ~f:(fun method_abi ->
        let fields = Json.drop_assoc method_abi in
        let m_name     = List.assoc "name" fields |> Json.drop_string in
        let m_constant = List.assoc "constant" fields |> Json.drop_bool in
        let m_inputs   = List.assoc "inputs" fields |> signature_from_json in
        let m_outputs  = List.assoc "outputs" fields |> signature_from_json in
        let m_payable  = List.assoc "payable" fields |> Json.drop_bool in
        let m_mutability = List.assoc "stateMutability" fields |> Json.drop_string |> mutability_from_string in
        let m_type = List.assoc "type" fields |> method_type_from_json in
        {
          m_name; m_constant; m_inputs; m_outputs; m_payable; m_mutability; m_type
        }
      )


end

module SolidityContract =
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


  let compile_solidity ~filename =
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

end


type value =
  | Int of int
  | Bool of bool
  | Addr of Types.address


(* Function description: [method_abi]. *)

(*
"Constructor and Callback function never have name or outputs. 
 Callback function doesn’t have inputs either.
 Sending non-zero ether to non-payable function will throw. Don’t do it."
*)




