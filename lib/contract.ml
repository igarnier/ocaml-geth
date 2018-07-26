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
    | Tdynamic_array of { typ : t }
    | Ttuple of t list

  let rec is_dynamic t =
    match t with
    | Tatomic (Tbytes { nbytes = DynamicLength })
    | Tatomic Tstring
    | Tdynamic_array _  -> true
    | Ttuple typs ->
      List.exists is_dynamic typs
    | Tstatic_array { typ } when is_dynamic typ -> true
    | _ -> false

  let rec print t =
    let open Printf in
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
      sprintf "%s[%d]" (print typ) numel
    | Tdynamic_array { typ } ->
      sprintf "%s[]" (print typ)
    | Ttuple types ->
      let tys = List.map print types in
      "("^(String.concat "," tys)^")"

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
    {
      desc : value_desc;
      typ  : SolidityTypes.t
    }

  and value_desc =
    | Int     of int64
    | BigInt  of Z.t
    | Bool    of bool
    | String  of string
    | Address of Types.address
    | Tuple   of value list
    | Func    of { selector : string; address : Bitstr.Hex.t }

  type event =
    {
      event_name : string;
      event_args : value list
    }

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

  and named_arg = 
    { 
      arg_name : string;
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

  let type_of { typ } = typ

  let int256_val (v : int64) =
    { 
      desc = Int v; 
      typ  = SolidityTypes.int_t 256
    }

  let uint256_val (v : int64) =
    { 
      desc = Int v; 
      typ  = SolidityTypes.uint_t 256
    }

  let string_val (v : string) =
    { 
      desc = String v; 
      typ  = SolidityTypes.string_t
    }

  let bytes_val (v : string) =
    { 
      desc = String v; 
      typ  = SolidityTypes.bytes_t
    }

  let bool_val (v : bool) =
    { 
      desc = Bool v;
      typ  = SolidityTypes.address_t
    }

  let address_val (v : Types.address) =
    { 
      desc = Address v;
      typ  = SolidityTypes.address_t
    }

  let tuple_val vals =
    { 
      desc = Tuple vals; 
      typ = SolidityTypes.Ttuple (List.map type_of vals) 
    }

  let static_array_val vals typ =
    {
      desc = Tuple vals;
      typ  = SolidityTypes.Tstatic_array { numel = List.length vals;
                                           typ }
    }

  let dynamic_array_val vals typ =
    {
      desc = Tuple vals;
      typ  = SolidityTypes.Tdynamic_array { typ }
    }

  (* -------------------------------------------------------------------------------- *)
  (* Computing function selectors *)

  open Printf

  let string_of_signature m_name m_inputs =
    let types =
      List.map (fun { arg_type } -> arg_type) m_inputs
    in
    let encodings =
      List.map SolidityTypes.print types
    in
    let elts = String.concat "," encodings in
    m_name^"("^elts^")"

  let keccak str =
    let hash = Cryptokit.Hash.keccak 256 in
    let resl = Cryptokit.hash_string hash str in
    Bitstr.Bit.of_string resl

  let keccak_4_bytes str =
    let hash = Cryptokit.Hash.keccak 256 in
    let resl = Cryptokit.hash_string hash str in
    let head = String.head resl 4 in
    Bitstr.Bit.of_string head

  let method_id method_abi =
    keccak_4_bytes (string_of_signature method_abi.m_name method_abi.m_inputs)

  let event_id event_abi =
    keccak (string_of_signature event_abi.e_name event_abi.e_inputs)

  (* -------------------------------------------------------------------------------- *)

  let header_size_of_type typ =
    let open SolidityTypes in
    match typ with
    | Tstatic_array { numel } when not (SolidityTypes.is_dynamic typ) ->
      32 * numel
    | Ttuple typs when not (SolidityTypes.is_dynamic typ) ->
      32 * (List.length typs)
    | _ ->
      32

  let header_size typs =
    let sum =
      List.fold_left (fun acc typ -> acc + header_size_of_type typ) 0 typs
    in
    Bytes.int sum

  module Encode =
  struct

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
      Bitstr.Bit.of_string result

    (* Encoding of values *)
    let int64_as_uint256 (i : int64) =
      if i < 0L then
        failwith "encode_int: cannot encode negative integer as unsigned int"
      else
        Bitstr.Bit.of_bigint 256 (Z.of_int64 i)

    let int64_as_int256 (i : int64) =
      Bitstr.Bit.of_bigint 256 (Z.of_int64 i)

    let address (s : Types.address) =
      let encoded = Bitstr.compress s in
      Bitstr.Bit.zero_pad_to ~dir:`left ~bits:encoded ~target_bits:(Bits.int 256)

    let bytes_static s n =
      if (Bytes.to_int n) <= 0 || (Bytes.to_int n) > 32 then
        failwith "bytes_static: Bytes type has wrong length";
      let len = String.length s in
      if len <> (Bytes.to_int n) then
        failwith "bytes_static: string value length mistmatch with target length";
      zero_pad_string_to_mod32 s

    let bytes_dynamic s =
      let len  = String.length s in
      let elen = int64_as_uint256 (Int64.of_int len) in
      (* Printf.printf "debug: encoding string of length %d: encoding of length=%s, string = %s\n"
       *   len
       *   Bitstr.(Hex.as_string (uncompress elen))
       *   Bitstr.(Hex.as_string (uncompress (zero_pad_string_to_mod32 s)))
       * ; *)
      Bitstr.Bit.concat [elen; zero_pad_string_to_mod32 s]

    let rec encode (value : value) =
      let { desc; typ } = value in
      let open SolidityTypes in
      match value.desc, typ with
      | Int v, Tatomic Tuint _ ->
        int64_as_uint256 v
      | Int v, Tatomic Tint _ ->
        int64_as_int256 v
      | Bool v, Tatomic Tbool ->
        let i = if v then 1L else 0L in
        int64_as_uint256 i
      | Address v, Tatomic Taddress ->
        address v
      | String v, Tatomic (Tbytes { nbytes = StaticLength n }) ->
        bytes_static v n
      | String v, Tatomic Tstring
      | String v, Tatomic (Tbytes { nbytes = DynamicLength }) ->
        bytes_dynamic v
      | Tuple values, Ttuple typs -> (* The types are implicitly contained in the values. *)
        (* compute size of header *)
        let headsz = header_size typs in
        (* convert tail values to bitstrings (possibly empty if not dynamic) *)
        let tails  = List.map encode_tails values in
        (* for each value, compute where its dynamic data is stored as an offset,
           taking into account header size. *)
        let _, offsets =
          List.fold_left (fun (offset, acc) bitstr ->
              let byte_len = bits_to_bytes (Bitstr.Bit.length bitstr) in
              let next_offset = Bytes.(offset + byte_len) in
              (next_offset, offset :: acc)
            ) (headsz, []) tails
        in
        let offsets = List.rev offsets in
        let heads = List.map2 encode_heads values offsets in
        Bitstr.Bit.concat (heads @ tails)
      | _ ->
        (* TODO: static/dynamic arrays *)
        failwith "encode: error"

    and encode_heads (value : value) (offset : Bytes.t) =
      if not (SolidityTypes.is_dynamic (type_of value)) then
        encode value
      else 
        int64_as_uint256 (Int64.of_int (Bytes.to_int offset))

    and encode_tails (value : value) =
      if not (SolidityTypes.is_dynamic (type_of value)) then
        Bitstr.Bit.of_string ""
      else
        encode value

  end

  module Decode =
  struct

    let rec decode b t =
      (* Printf.eprintf "decoding %s with data %s\n" (SolidityTypes.print t) (Bitstr.Hex.as_string (Bitstr.uncompress b)); *)
      let open SolidityTypes in
      match t with
      | Tatomic at ->
        decode_atomic b at
      | Tfunction ->
        decode_function b
      | Tstatic_array { numel; typ } ->
        decode_static_array b numel typ
      | Tdynamic_array { typ } ->
        decode_dynamic_array b typ
      | Ttuple typs ->
        tuple_val (decode_tuple b typs)
          
    and decode_atomic b at =
      (* Printf.eprintf "decoding atomic %s\n" (SolidityTypes.print (SolidityTypes.Tatomic at));       *)
      match at with
      | Tuint { w } ->
        decode_uint b w
      | Tint { w } ->
        decode_int b w
      | Taddress ->
        address_val (decode_address b)
      | Tbool ->
        let z = Bitstr.Bit.to_unsigned_bigint b in
        bool_val (Z.gt z Z.zero)
      | Tfixed _
      | Tufixed _ ->
        failwith "decode_atomic: fixed point numbers not handled yet"
      | Tbytes { nbytes = StaticLength n } ->
        let    n     = bytes_to_bits n in
        let bytes, _ = Bitstr.Bit.take b n in
        bytes_val (Bitstr.Bit.as_string bytes)
      | Tbytes { nbytes = DynamicLength } ->
        let len, rem = Bitstr.Bit.take b (Bits.int 256) in
        let len = Z.to_int (Bitstr.Bit.to_unsigned_bigint len) in
        let bytes, _ = Bitstr.Bit.take rem (bytes_to_bits (Bytes.int len)) in
        bytes_val (Bitstr.Bit.as_string bytes)
      | Tstring ->
        let len, rem = Bitstr.Bit.take b (Bits.int 256) in
        let len = Z.to_int (Bitstr.Bit.to_unsigned_bigint len) in
        let bytes, _ = Bitstr.Bit.take rem (bytes_to_bits (Bytes.int len)) in
        string_val (Bitstr.Bit.as_string bytes)

    and decode_address b =
      let addr, _ = Bitstr.Bit.take b (Bits.int 160) in
      Bitstr.uncompress addr

    and decode_function b =
      let content, _ = Bitstr.Bit.take b (Bits.int (160 + 32)) in
      let address, selector = Bitstr.Bit.take content (Bits.int 160) in
      let address  = decode_address address in
      let selector = Bitstr.Bit.as_string selector in
      { desc = Func { selector; address };
        typ  = SolidityTypes.Tfunction }

    and decode_static_array b numel t =
      static_array_val (decode_tuple b (List.make numel t)) t
        
    and decode_dynamic_array b t =
      let numel, content = Bitstr.Bit.take b (Bits.int 256) in
      let numel = Z.to_int (Bitstr.Bit.to_unsigned_bigint numel) in
      dynamic_array_val (decode_tuple b (List.make numel t)) t

    and decode_tuple b typs =
      (* Printf.eprintf "decoding tuple %s with data = %s\n"  *)
      (*   (SolidityTypes.print (SolidityTypes.Ttuple typs))
       *   (Bitstr.Hex.as_string (Bitstr.uncompress b))
       * ; *)
      let _, values =
        List.fold_left (fun (header_chunk, values) ty ->
            let chunk, rem = Bitstr.Bit.take header_chunk (Bits.int 256) in
            if SolidityTypes.is_dynamic ty then
              let offset  = Bitstr.Bit.to_unsigned_bigint chunk in
              let offset  = Z.to_int offset in
              (* offsets are computed starting from the beginning of [b] *)
              let _, tail = Bitstr.Bit.take b (bytes_to_bits (Bytes.int offset )) in
              let value   = decode tail ty in
              (rem, value :: values)
            else
              let value = decode chunk ty in
              (rem, value :: values)
        ) (b, []) typs
      in
      List.rev values

    and decode_uint (b : Bitstr.Bit.t) w =
      (* Printf.eprintf "decode_uint: %s\n" (Bitstr.Hex.as_string (Bitstr.uncompress b)); *)
      let z = Bitstr.Bit.to_unsigned_bigint b in
      if Z.fits_int64 z then
        { desc = Int (Z.to_int64 z);
          typ  = SolidityTypes.uint_t (Bits.to_int w) }
      else
        { desc = BigInt z;
          typ  = SolidityTypes.uint_t (Bits.to_int w) }          
          
    and decode_int b w =
      (* Printf.eprintf "decode_int: %s\n" (Bitstr.Hex.as_string (Bitstr.uncompress b)); *)
      let z = Bitstr.Bit.to_signed_bigint b in
      if Z.fits_int64 z then
        { desc = Int (Z.to_int64 z);
          typ  = SolidityTypes.uint_t (Bits.to_int w) }
      else
        { desc = BigInt z;
          typ  = SolidityTypes.uint_t (Bits.to_int w) }
            
    let decode_events abis receipt =
      let open Types.Tx in      
      let codes = List.fold_left (fun acc abi ->
          match abi with
          | Event event_abi ->
            let id = Bitstr.uncompress (event_id event_abi) in
            (id, event_abi) :: acc
          | _ -> acc
        ) [] abis
      in
      List.fold_left (fun acc log ->
          let topics = log.log_topics in
          let data   = log.log_data in
          (* Check whether /at most one/ topic corresponds to an event *)
          let relevant = List.filter (fun hash ->
              List.mem_assoc hash codes
            ) topics
          in
          let event =
            match relevant with
            | [] | _ :: _ :: _ ->
              failwith "0 or > 1 matching event for topic, aborting"
            | [ hash ] ->
              List.assoc hash codes
          in
          let types  = List.map (fun { arg_type } -> arg_type) event.e_inputs in
          let fields =
            match decode (Bitstr.compress (Bitstr.Hex.of_string data)) (Ttuple types) with
            | { desc = Tuple values } -> values
            | exception _ ->
              failwith "decode_events: error while decoding"
            | _ ->
              failwith "decode_events: bug found"
          in          
          {
            event_name = event.e_name;
            event_args = fields
          } :: acc
        ) [] receipt.logs

  end

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
      bin           : Bitstr.Bit.t;
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
            let bin = Bitstr.(compress (Hex.of_string ("0x"^bin))) in
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
      ~(uri:string)
      ~(account:Types.address)
      ~(gas:Z.t)
      ~(contract:solidity_output)
      ~(arguments:ABI.value list)
      ~(value:Z.t option) =
    let prepare_constructor ctx =
      let constr_abi = get_constructor ctx in
      let inputs     = constr_abi.ABI.c_inputs in
      let encoded = 
        match arguments with
        | [] ->
          Bitstring.empty_bitstring
        | _  ->
          (List.iter2 (fun v t -> 
               if not (ABI.type_of v = t.ABI.arg_type) then
                 (let typeof_v = SolidityTypes.print (ABI.type_of v) in
                  let arg_typ  = SolidityTypes.print t.ABI.arg_type in
                  failwith ("deploy_rpc: constructor argument types do not match constructor declaration: "^typeof_v^" vs "^arg_typ)
                 )
             ) arguments inputs;
           ABI.(Encode.encode (ABI.tuple_val arguments))
          )
      in
      Bitstr.(uncompress (Bit.concat [ctx.bin; encoded]))
    in
    let deploy data =
      Rpc.Eth.send_contract_and_get_receipt ~uri ~src:account ~data ~gas ()
    in
    let rec loop ctxs =
      match ctxs with
      | [] ->
        failwith "deploy_rpc: no contracts were deployable"
      | ctx :: tl ->
        match Bitstr.Bit.as_string ctx.bin with
        | "" -> loop tl
        | _  ->
          (let data = prepare_constructor ctx in
           deploy data)
    in
    loop contract.contracts

  let call_method_tx
      ~(abi:ABI.method_abi)
      ~(arguments:ABI.value list)
      ~(src:Types.address)
      ~(ctx:Types.address)
      ~(gas:Z.t)
      ~(value:Z.t option)
    =
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
        Printf.printf "calling method %s with code %s\n%!" mname (Bitstr.Hex.as_string (Bitstr.uncompress method_id));
        let encoded = ABI.(Encode.encode (ABI.tuple_val arguments)) in        
        let bitstring = Bitstr.Bit.concat [method_id; encoded] in
        let data = Bitstr.(Hex.as_string (uncompress bitstring)) in
        {
          Types.Tx.src; 
          dst = Some ctx; 
          gas = Some gas; 
          gas_price = None; 
          value; 
          data; 
          nonce = None
        }

  let call_void_method_tx ~mname ~(src:Types.address) ~(ctx:Types.address) ~(gas:Z.t) =
      let method_id = ABI.keccak_4_bytes mname in
      let data = Bitstr.(Hex.as_string (uncompress method_id)) in
      {
        Types.Tx.src; dst = Some ctx;  gas = Some gas; gas_price = None; value = None; data; nonce = None
      }
      
  let execute_method
      ~(uri:string)
      ~(abi:ABI.method_abi)
      ~(arguments:ABI.value list)
      ~(src:Types.address)
      ~(ctx:Types.address)
      ~(gas:Z.t)
      ~(value:Z.t option) =
    let tx = call_method_tx ~abi ~arguments ~src ~ctx ~gas ~value in
    Rpc.Eth.send_transaction_and_get_receipt ~uri ~transaction:tx

  let call_method
      ~(uri:string)
      ~(abi:ABI.method_abi)
      ~(arguments:ABI.value list)
      ~(src:Types.address)
      ~(ctx:Types.address)
      ~(gas:Z.t)
      ~(value:Z.t option) =
    let tx = call_method_tx ~abi ~arguments ~src ~ctx ~gas ~value in        
    Rpc.Eth.call ~uri ~transaction:tx ~at_time:`latest

  let execute_method_lwt
      ~(uri:string)
      ~(abi:ABI.method_abi)
      ~(arguments:ABI.value list)
      ~(src:Types.address)
      ~(ctx:Types.address)
      ~(gas:Z.t)
      ~(value:Z.t option) =
    let tx = call_method_tx ~abi ~arguments ~src ~ctx ~gas ~value in
    Rpc.EthLwt.send_transaction_and_get_receipt ~uri ~transaction:tx

end
