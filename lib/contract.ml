open Batteries
open Basic

(* https://solidity.readthedocs.io/en/develop/abi-spec.html *)

let assoc key fields =
  try List.assoc key fields with
  | Not_found ->
    (Printf.printf "assoc: key %s not found\n%!" key;
     Printf.printf "available fields:\n%!";
     List.iter (fun (field, json) ->
         Printf.printf "%s : %s\n%!" field (Json.to_string json)
       ) fields;
     raise Not_found)



module SolidityTypes =
struct

  type bitwidth = Bits.t (* mod 8 = 0, 0 < bitwidth <= 256*)

  type length =
    | StaticLength of Bytes.t
    | DynamicLength

  type atomic =
    | Tuint of { w : bitwidth }
    | Tint of { w : bitwidth }
    | Taddress
    | Tbool
    (* signed fixed-point decimal number of M bits, 8 <= M <= 256, M % 8 ==0, and 0 < N <= 80, which denotes the value v as v / (10 ** N). *)      
    | Tfixed of { m : bitwidth; n : bitwidth }
    | Tufixed of { m : bitwidth; n : bitwidth }
    | Tbytes of { nbytes : length } (* 0 < #nbytes <= 32 *)
    | Tstring      

  and t =
    | Tatomic of atomic
    | Tfunction
    | Tstatic_array of { numel : int; typ : t }
    | Ttuple of t list
    (* | Tdynamic_array of { typ : t } *)

  let is_dynamic t =
    match t with
    | Tatomic (Tbytes { nbytes = DynamicLength })
    | Tatomic Tstring
    | Tstatic_array _
    | Ttuple _
 (* | Tdynamic_array _ *)  -> true
    | _ -> false

  let uint_t w =
    Tatomic (Tuint { w = Bits.int w })

  let int_t w =
    Tatomic (Tint { w = Bits.int w })

  let string_t = Tatomic Tstring
  
  let bytes_t = Tatomic (Tbytes { nbytes = DynamicLength })

  let address_t = Tatomic Taddress
 

end

module ABI =
struct

  type value =
    | Int    of { v : int64; t : SolidityTypes.t }
    | Bool   of { v : bool }
    | String of { v : string; t : SolidityTypes.t }
    | Tuple  of value list

  type abi =
    | Method of method_abi
    | Constructor of constructor_abi
    | Event of event_abi

  and constructor_abi =
    {
      c_inputs     : tuple_abi;
      c_payable    : bool;
      c_mutability : mutability;
    }

  and method_abi =
    {
      m_name       : string;     (* name of the method *)
      m_constant   : bool;       (* true if function is either Pure or View. Redundant??? *)
      m_inputs     : tuple_abi;
      m_outputs    : tuple_abi;
      m_payable    : bool;       (* true iff a function accepts ether *)
      m_mutability : mutability; (* purity annotation *)
      m_type       : mtype       (* Kind of method. Defaults to Function if omitted *)
    }

  and event_abi =
    {
      e_name      : string;
      e_inputs    : tuple_abi;
      e_anonymous : bool;
    }    
    

  and tuple_abi = named_arg list

  and named_arg = { arg_name : string;
                    arg_type : SolidityTypes.t;
                    (* potential additional field for Event: indexed:bool*)
                  }

  and mtype =
    | Function
    | Callback

  and mutability =
    | Pure         (* specified to not read blockchain state *)
    | View         (* specified to not modify the blockchain state *)
    | Nonpayable
    | Payable

  (* -------------------------------------------------------------------------------- *)
  (* Convenience functions to create ABI values *)

  let int256_val (v : int64) =
    Int { v; t = SolidityTypes.int_t 256 }

  let uint256_val (v : int64) =
    Int { v; t = SolidityTypes.uint_t 256 }

  let string_val (v : string) =
    String { v; t = SolidityTypes.string_t }

  let bytes_val (v : string) =
    String { v; t = SolidityTypes.bytes_t }

  let address_val (v : Types.address) =
    String { v = Types.address_to_string v;
             t = SolidityTypes.address_t }

  let tuple vals =
    Tuple vals
  
  (* -------------------------------------------------------------------------------- *)

  let rec type_of value =
    match value with
    | Int { t } -> t
    | Bool _ -> SolidityTypes.(Tatomic Tbool)
    | String { t } -> t
    | Tuple vs ->
      SolidityTypes.Ttuple (List.map type_of vs)

  (* -------------------------------------------------------------------------------- *)
  (* Encoding of types *)

  open Printf

  let rec encoding_of_type t =
    let open SolidityTypes in
    match t with
    | Tatomic atomic ->
      (match atomic with
       | Tuint { w } -> sprintf "uint%d" (Bits.to_int w)
       | Tint { w }  -> sprintf "int%d" (Bits.to_int w)
       | Taddress    -> "address"
       | Tbool       -> "bool"
       | Tfixed { m; n }   -> sprintf "fixed%dx%d" (Bits.to_int m) (Bits.to_int n)
       | Tufixed { m; n }  -> sprintf "ufixed%dx%d" (Bits.to_int m) (Bits.to_int n)
       | Tbytes { nbytes = StaticLength n } -> sprintf "bytes%d" (Bytes.to_int n)
       | Tbytes { nbytes = DynamicLength } -> sprintf "bytes"
       | Tstring -> "string"
      )
    | Tfunction -> "bytes24"
    | Tstatic_array { numel; typ } ->
      sprintf "%s[%d]" (encoding_of_type typ) numel
    | Ttuple types ->
      let tys = List.map encoding_of_type types in
      "("^(String.concat "," tys)^")"
      

  let string_of_signature { m_name; m_inputs } =
    let types =
      List.map (fun { arg_type } -> arg_type) m_inputs
    in
    let encodings =
      List.map encoding_of_type types
    in
    let elts = String.concat "," encodings in
    m_name^"("^elts^")"

  let keccak_4_bytes str =
    let hash = Cryptokit.Hash.keccak 256 in
    let resl = Cryptokit.hash_string hash str in
    let head = String.head resl 4 in
    Bitstr.bits_of_string head

  let method_id method_abi =
    keccak_4_bytes (string_of_signature method_abi)

  (* -------------------------------------------------------------------------------- *)

  (* in bytes *)
  let header_size value =
    let ill_typed () =
      failwith "header_size: ill_typed value"
    in
    let open SolidityTypes in
    match value with
    | Int { t } ->
      (match t with
       | Tatomic Tuint { w }
       | Tatomic Tint { w } ->
         (* data is padded to 32 bytes *)
         bits_to_bytes (Bits.int 256)           
       | _ -> ill_typed ())
    | Bool _ ->
      Bytes.int 32
    | String { t } ->
      (match t with
       | Tatomic Taddress
       | Tatomic (Tbytes { nbytes = StaticLength _ }) ->
         (* Everything is padded to 32 bytes anyway. *)
         bits_to_bytes (Bits.int 256)
       | Tatomic (Tbytes { nbytes = DynamicLength })
       | Tatomic Tstring ->
         (* In the dynamic case, the offset is also padded to 32 bytes. *)
         bits_to_bytes (Bits.int 256)
       | _ ->
         ill_typed ())
    | Tuple values ->
      bits_to_bytes (Bits.int 256)

  (* in bytes *)
  (* let tail_size value =
   *   match value with
   *   | Int _ 
   *   | Bool _ -> Bytes.int 0
   *   | String { v } ->
   *     (\* length (coded on 32 bytes) + data *\)
   *     Bytes.int (32 + (String.length v))
   *   | Tuple values ->
   *     failwith "tail_size: can't handle tuple yet" *)
  
  let zero_pad_string_to_mod32 s =
    let len = String.length s in
    let result =
      if len = 0 then
        String.make 32 '\000'
      else
        let rem = len mod 32 in
        if rem = 0 then
          s
        else
          let pad = 32 - rem in
          s^(String.make pad '\000')
    in
    Bitstr.bits_of_string result
    

  (* Encoding of values *)
  let encode_int (i : int64) (t : SolidityTypes.atomic) =
    match t with
    | Tuint { w } ->
      if i < 0L then
        failwith "encode_int: cannot encode negative integer as unsigned int"
      else
        Printf.printf "encoding uint %Ld as %s\n" i (Bitstr.(hex_as_string (uncompress (bits_of_int64 i))));
        Bitstr.(zero_pad_to ~dir:`left ~bits:(bits_of_int64 i) ~target_bits:(Bits.int 256))
    | Tint { w } ->
      if i < 0L then
        Bitstr.(one_pad_to ~dir:`left ~bits:(bits_of_int64 i) ~target_bits:(Bits.int 256))
      else
        Bitstr.(zero_pad_to ~dir:`left ~bits:(bits_of_int64 i) ~target_bits:(Bits.int 256))
    | _ ->
      failwith "encode_int: incompatible Solidity type"

  let encode_string (s : string) ( t : SolidityTypes.atomic) =
    match t with
     | Taddress -> (* 160 bits *)
       let encoded = Bitstr.compress (Bitstr.hex_of_string s) in
       Bitstr.zero_pad_to ~dir:`left ~bits:encoded ~target_bits:(Bits.int 256)
     | Tbytes { nbytes = (StaticLength n) } ->
       if (Bytes.to_int n) <= 0 || (Bytes.to_int n) > 32 then
         failwith "encode_string: Bytes type has wrong length";
       let len = String.length s in
       if len <> (Bytes.to_int n) then
         failwith "encode_string: string value length mistmatch with type length";
       zero_pad_string_to_mod32 s
     | Tstring
     (* We're supposed to utf8-encode [s] and then treat it as bytes. 
        TODO: do the encoding! *)
     | Tbytes { nbytes = DynamicLength } ->
       let len = String.length s in
       let len = encode_int (Int64.of_int len) (Tuint { w = Bits.int 256 }) in
       Bitstr.concat [len; zero_pad_string_to_mod32 s]
     | _ ->
       failwith "encode_string: type mismatch (string)"

  let rec encode_value (value : value) =
    let open SolidityTypes in
    match value with
    | Int { v; t } ->
      (match t with
       | Tatomic atomic ->
           encode_int v atomic
       | _ ->
         failwith "encode_value: type mismatch (bool)"
      )
    | Bool { v } ->
      let res =
        if v then
          encode_int 1L (Tuint { w = Bits.int 256 })
        else
          encode_int 0L (Tuint { w = Bits.int 256 })
      in
      Printf.printf "encoding bool %b: %s\n" v Bitstr.(hex_as_string (uncompress res));
      res
    | String { v; t } ->
      (match t with
       | Tatomic atomic ->
         encode_string v atomic
       | _ ->
         failwith "encode_value: type mismatch (string)")
    | Tuple values ->
      (* compute size of header *)
      let headsz : Bytes.t =
        List.fold_left Bytes.(fun acc v -> acc + header_size v) (Bytes.int 0) values
      in
      (* convert tail values to bitstrings (possibly empty if not dynamic) *)
      let tails   = List.map encode_tails values in
      (* for each value, compute where its dynamic data is stored as an offset,
         taking into account header size. *)
      let _, offsets =
        List.fold_left (fun (offset, acc) bitstr ->
            let _ = Printf.printf "offset for %s: %d bytes\n" Bitstr.(hex_as_string (uncompress bitstr)) (Bytes.to_int offset) in
            let byte_len = bits_to_bytes (Bitstr.bit_length bitstr) in
            let next_offset = Bytes.(offset + byte_len) in
            (next_offset, offset :: acc)
          ) (headsz, []) tails
      in
      let offsets = List.rev offsets in
      let heads = List.map2 encode_heads values offsets in
      Bitstr.concat (heads @ tails)
      
  and encode_heads (value : value) (offset : Bytes.t) =
    let open SolidityTypes in
    if not (is_dynamic (type_of value)) then
      encode_value value
    else match value with
      | String { v; t } ->
        encode_int
          (Int64.of_int (Bytes.to_int offset))
          (SolidityTypes.Tuint { w = Bits.int 256 })
      | Tuple _ ->
        failwith "encode_heads: tuple not handled yet"
      | _ ->
        failwith "encode_heads: bug found"

  and encode_tails (value : value) =
    let open SolidityTypes in
    if not (is_dynamic (type_of value)) then
      Bitstr.bits_of_string ""
    else match value with
      | String _ ->
        encode_value value
      | _ ->
        failwith "encode_tails: bug found"
      

  (* let encode_list (vs : value list) (ts : SolidityTypes.t list) =
   *   let rec loop vs ts acc =
   *     match vs, ts with
   *     | [], [] -> acc
   *     | v :: vs', t :: ts' ->
   *       let enc = encode_value v t in
   *       let acc = failwith "" in
   *       loop vs' ts' acc
   *     | _ ->
   *       failwith "encode_list: value and type list have different lengths"
   *   in
   *   loop vs ts (Bitstr.bits_of_string "") *)

  (* -------------------------------------------------------------------------------- *)
  (* Deserialization of ABIs from solc --json output *)

  let method_type_of_json mtype =
    match mtype with
    | `String mtype ->
      (match mtype with
       | "function" -> `Method Function
       | "callback" -> `Method Callback
       | "constructor" -> `Constructor                         
       | "event" -> `Event
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
      let open SolidityTypes in
      (match s with
       | "uint8"   -> Tatomic (Tuint { w = Bits.int 8 })
       | "uint16"   -> Tatomic (Tuint { w = Bits.int 16 })
       | "uint32"   -> Tatomic (Tuint { w = Bits.int 32 })
       | "uint64"   -> Tatomic (Tuint { w = Bits.int 64 })
       | "uint128"   -> Tatomic (Tuint { w = Bits.int 128 })
       | "uint256" -> Tatomic (Tuint { w = Bits.int 256 })
       | "int256 " -> Tatomic (Tint { w = Bits.int 256 })
       | "bool"    -> Tatomic Tbool
       | "string"  -> Tatomic Tstring
       | "bytes"   -> Tatomic (Tbytes { nbytes = DynamicLength })
       | "address" -> Tatomic Taddress
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
        let arg_name = assoc "name" fields |> Json.drop_string in
        let arg_type = assoc "type" fields |> type_of_json in
        { arg_name; arg_type }
      )

  let from_json json =
    ListLabels.map (Json.drop_list json) ~f:(fun method_abi ->
        let fields = Json.drop_assoc method_abi in
        let m_type = assoc "type" fields |> method_type_of_json in
        match m_type with
        | `Constructor ->
          let c_inputs   = assoc "inputs" fields |> signature_of_json in
          let c_payable  = assoc "payable" fields |> Json.drop_bool in
          let c_mutability = assoc "stateMutability" fields |> Json.drop_string |> mutability_of_string in
          Constructor {
            c_inputs; c_payable; c_mutability
          }
         
        | `Method m_type ->
          let m_name     = assoc "name" fields |> Json.drop_string in
          let m_constant = assoc "constant" fields |> Json.drop_bool in
          let m_inputs   = assoc "inputs" fields |> signature_of_json in
          let m_outputs  = assoc "outputs" fields |> signature_of_json in
          let m_payable  = assoc "payable" fields |> Json.drop_bool in
          let m_mutability = assoc "stateMutability" fields |> Json.drop_string |> mutability_of_string in

          Method {
            m_name; m_constant; m_inputs; m_outputs; m_payable; m_mutability; m_type
          }
        | `Event ->
          let e_name   = assoc "name" fields |> Json.drop_string in
          let e_inputs = assoc "inputs" fields |> signature_of_json in
          let e_anonymous = assoc "anonymous" fields |> Json.drop_bool in
          Event {
            e_name; e_inputs; e_anonymous
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
      bin           : Bitstr.bitstring;
      abi           : ABI.abi list;
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
    let raw_jsn = exec_and_get_stdout "solc" [| "solc"; "--optimize"; "--combined-json"; "abi,bin,interface"; filename |] in
    let result  = Json.from_string raw_jsn in
    try
      let fields  = Json.drop_assoc result in
      let version   = assoc "version" fields |> Json.drop_string in
      let contracts = assoc "contracts" fields |> Json.drop_assoc in
      let contracts =
        List.map (fun (contract_name, contract_contents) ->
            let contents = Json.drop_assoc contract_contents in
            let bin = assoc "bin" contents |> Json.drop_string in
            let bin = Bitstr.(compress (hex_of_string ("0x"^bin))) in
            let abi = assoc "abi" contents |> Json.drop_string |> Json.from_string |> ABI.from_json in
            { contract_name; bin; abi }
          ) contracts
      in
      { version; contracts }
    with
    | Not_found ->
      Printf.printf "to_json: error while parsing json.\n";
      print_string raw_jsn;
      exit 1

  let get_constructor ctx =
    let constr_abi =
      List.fold_left (fun acc abi ->
          match abi with
          | ABI.Constructor cs -> Some cs
          | _ -> acc
        ) None ctx.abi
    in
    match constr_abi with
    | None ->
      failwith "get_constructor: constructor not found"
    | Some cs -> cs

  let get_method ctx mname =
    List.fold_left (fun acc abi ->
        match abi with
        | ABI.Method ms ->
          if ms.ABI.m_name = mname then
            Some ms
          else
            acc
        | _ -> acc
      ) None ctx.abi
  
  let deploy_rpc
      ~(uri : string)
      ~(account : Types.address)
      ~(gas : Z.t)
      ~(contract : solidity_output)
      ~(arguments : ABI.value list) =
    let prepare_constructor ctx =
      let constr_abi = get_constructor ctx in
      let inputs     = constr_abi.ABI.c_inputs in
      let typechecks =
        List.for_all2 (fun v t -> ABI.type_of v = t.ABI.arg_type) arguments inputs
      in
      if not typechecks then
        failwith "deploy_rpc: constructor argument types do not match constructor declaration"
      else
        let encoded = ABI.(encode_value (Tuple arguments)) in
        Bitstr.(uncompress (concat [ctx.bin; encoded]))
    in
    let deploy data =
      Rpc.Eth.send_contract_and_get_receipt ~uri ~src:account ~data ~gas
    in
    let rec loop ctxs =
      match ctxs with
      | [] ->
        failwith "deploy_rpc: no contracts were deployable"
      | ctx :: tl ->
        match Bitstr.bits_as_string ctx.bin with
        | "" -> loop tl
        | _  ->
          (let data = prepare_constructor ctx in
           deploy data)
    in
    loop contract.contracts

  let call_method_tx
      ~(abi : ABI.method_abi)
      ~(arguments : ABI.value list)
      ~(src : Types.address)
      ~(ctx : Types.address)
      ~(gas : Z.t) =
      let mname = abi.m_name in
      let inputs = abi.ABI.m_inputs in
      let siglen = List.length inputs in
      let arglen = List.length arguments in
      if siglen <> arglen then
        let m = Printf.sprintf
            "call_method: # of arguments mismatch for method %s: %d expected vs %d actual\n" mname siglen arglen
        in
        failwith m
      else
        let method_id = ABI.method_id abi in
        let encoded = ABI.(encode_value (Tuple arguments)) in        
        let bitstring = Bitstr.concat [method_id; encoded] in
        let data = Bitstr.(hex_as_string (uncompress bitstring)) in
        {
          Types.src; dst = Some ctx;  gas = Some gas; gas_price = None; value = None; data; nonce = None
        }

  let call_void_method_tx ~mname ~(src : Types.address) ~(ctx : Types.address) ~(gas : Z.t) =
      let method_id = ABI.keccak_4_bytes mname in
      let data = Bitstr.(hex_as_string (uncompress method_id)) in
      {
        Types.src; dst = Some ctx;  gas = Some gas; gas_price = None; value = None; data; nonce = None
      }

  
end
