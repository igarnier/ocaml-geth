open Batteries
    
(* https://solidity.readthedocs.io/en/develop/abi-spec.html *)

module Types =
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

(* Function description: [method_abi]. *)

(*
Constructor and fallback function never have name or outputs. Fallback function doesn’t have inputs either.

Sending non-zero ether to non-payable function will throw. Don’t do it.
*)

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
                  arg_type : Types.t }

and mtype =
  | Function
  | Constructor
  | Callback

and mutability =
  | Pure         (* specified to not read blockchain state *)
  | View         (* specified to not modify the blockchain state *)
  | Nonpayable
  | Payable


open Printf

let rec encoding_of_type =
  let open Types in
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
  
