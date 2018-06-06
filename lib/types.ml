open Batteries

type address = string
type hash256 = string
type hash512 = string
  
    
let char_is_hex = function
  | '0' .. '9'
  | 'A' .. 'F'
  | 'a' .. 'f' -> true
  | _ -> false

let string_is_hex x =
  let rec loop i len x acc =
    if i = len then
      acc
    else
      loop (i+1) len x (acc && char_is_hex x.[i])
  in
  x.[0] = '0'
  && (x.[1] = 'x' || x.[1] = 'X')
  && (loop 2 (String.length x) x true)

let address_to_string x = x

let address_from_string x =
  if String.length x != 42 || not (string_is_hex x) then
    failwith "address_from_string: input must be 20 bytes (40 hex chars) 0x-prefixed"
  else
    x

let hash256_to_string x = x
  
let hash256_from_string x =
  if String.length x != 66 || not (string_is_hex x) then
    failwith "hash256_from_string: input must be 32 bytes (64 hex chars) 0x-prefixed"
  else
    x

let hash512_to_string x = x
  
let hash512_from_string x =
  if String.length x != 130 || not (string_is_hex x) then
    failwith "hash512_from_string: input must be 64 bytes (128 hex chars) 0x-prefixed"
  else
    x


type wei       = int (* Z.t ? *)
type block_id  = int (* Z.t ? *)

type transaction =
  {
    src : address;
    dst : address option;
    gas : int option;
    gas_price : int option;
    value : int option;
    data : string;
    nonce : int option
  }

type transaction_receipt =
  {
    block_hash          : hash256;
    block_number        : int;
    contract_address    : address option;
    cumulative_gas_used : int;
    src                 : address;
    dst                 : address option;
    gas_used            : int;
    logs                : string list;
    (* unused:
     * logs_bloom : string;
     * root : string; *)
    transaction_hash    : hash256;
    transaction_index   : int
  }


type port_info =
  {
    discovery : int;
    listener  : int
  }

type protocol_info =
  Eth of {
    difficulty : Z.t;
    genesis    : hash256 option;
    head       : hash256;
    network    : int
  }

type node_info =
  {
    enode       : string;
    id          : hash512;
    ip          : string;
    listen_addr : string;
    name        : string;
    ports       : port_info;
    protocols   : protocol_info
  }

type network_info =
  {
    local_address  : string;
    remote_address : string;
  }

type peer =
  {
    caps      : string list;
    id        : hash512;
    name      : string;
    network   : network_info;
    protocols : protocol_info;
  }

type peer_info = peer list


let hex i =
  `String (Tools.hex_of_int i)

let transaction_to_json : transaction -> Yojson.Basic.json =
  fun t ->
    let args =
      [ ("from", `String (address_to_string t.src)) ]
      @ (match t.dst with Some x -> [("to", `String (address_to_string x))] | _ -> [])
      @ (match t.gas with Some x -> [("gas", hex x)] | _ -> [])
      @ (match t.gas_price with Some x -> [("gasPrice", hex x)] | _ -> [])
      @ (match t.value with Some x -> [("value", hex x)] | _ -> [])
      @ [("data", `String t.data)]
      @ (match t.nonce with Some x -> [("nonce", `Int x)] | _ -> [])
    in
    (`Assoc args)

let assoc key fields =
  try List.assoc key fields with
  | Not_found ->
    failwith (Printf.sprintf "assoc: key %s not found" key)

let receipt_from_json : Yojson.Basic.json -> transaction_receipt option =
  fun j ->
    match j with
    | `Null -> None
    | `Assoc fields ->
      begin
        let block_hash   = assoc "blockHash" fields |> Tools.drop_string in
        let block_number = assoc "blockNumber" fields |> Tools.drop_int_as_string in
        let contract_address =
          match assoc "contractAddress" fields with
          | `String addr -> Some addr
          | `Null        -> None
          | _ ->
            failwith "Types.receipt_from_json: unexpected result"
        in
        let cumulative_gas_used = assoc "cumulativeGasUsed" fields |> Tools.drop_int_as_string in
        let gas_used = assoc "gasUsed" fields |> Tools.drop_int_as_string in    
        let src = assoc "from" fields |> Tools.drop_string |> address_from_string in
        let dst =
          match assoc "to" fields with
          | `String addr -> Some (address_from_string addr)
          | `Null        -> None
          | _ ->
            failwith "Types.receipt_from_json: unexpected result"
        in
        let logs = assoc "logs" fields |> Tools.drop_string_list in
        let transaction_hash = assoc "transactionHash" fields |> Tools.drop_string in
        let transaction_index = assoc "transactionIndex" fields |> Tools.drop_int_as_string in
        Some {
          block_hash;
          block_number;
          contract_address;
          cumulative_gas_used;
          gas_used;      
          src;
          dst;
          logs;
          transaction_hash;
          transaction_index
        }
      end
    | _ ->
      let s = Yojson.Basic.to_string j in
      failwith ("Types.receipt_from_json: unexpected json: "^s)

let port_info_from_json : Yojson.Basic.json -> port_info option =
  fun j ->
    match j with
    | `Assoc fields ->
      let discovery = assoc "discovery" fields |> Tools.drop_int in
      let listener  = assoc "listener" fields |> Tools.drop_int in
      Some { discovery; listener }
    | _ ->
      None

let protocol_info_from_json : Yojson.Basic.json -> protocol_info option =
  fun j ->
    let proto = Tools.drop_assoc j |> assoc "eth" in
    match proto with
    | `Assoc fields ->
      (* TODO: use Json.Safe instead of Basic ... *)
      let difficulty = assoc "difficulty" fields |> Tools.drop_int |> Z.of_int in
      let genesis =
        try Some (assoc "genesis" fields
                  |> Tools.drop_string
                  |> hash256_from_string)
        with Not_found -> None
      in
      let head = assoc "head" fields
                 |> Tools.drop_string
                 |> hash256_from_string
      in
      let network = assoc "head" fields |> Tools.drop_int in
      Some (Eth { difficulty; genesis; head; network })
    | _ ->
      None

let node_info_from_json : Yojson.Basic.json -> node_info option =
  fun j ->
    match j with
    | `Assoc fields ->
      let enode = assoc "enode" fields |> Tools.drop_string in
      let id = assoc "id" fields
               |> Tools.drop_string
               |> hash512_from_string
      in
      let ip = assoc "ip" fields |> Tools.drop_string in
      let listen_addr = assoc "listenAddr" fields |> Tools.drop_string in
      let name = assoc "name" fields |> Tools.drop_string in
      let ports =
        match assoc "ports" fields |> port_info_from_json with
        | None -> failwith "node_info_from_json: can't parse port_info"
        | Some port_info -> port_info
      in
      let protocols =
        match assoc "protocols" fields |> protocol_info_from_json with
        | None ->  failwith "node_info_from_json: can't parse protocol_info"
        | Some protocol_info -> protocol_info
      in
      Some {
        enode; id; ip; listen_addr; name; ports; protocols 
      }
    | _ ->
      None

let network_info_from_json : Yojson.Basic.json -> network_info option =
  fun j ->
    match j with
    | `Assoc fields ->
      let local_address  = assoc "localAddress" fields |> Tools.drop_string in
      let remote_address = assoc "remoteAddress" fields |> Tools.drop_string in
      Some { local_address; remote_address }
    | _ ->
      None

let peer_from_json : Yojson.Basic.json -> peer option =
  fun j ->
    match j with
    | `Assoc fields ->
      let caps = assoc "caps" fields |> Tools.drop_string_list in
      let id   = assoc "id" fields
               |> Tools.drop_string
               |> hash512_from_string
      in
      let name = assoc "name" fields |> Tools.drop_string in
      let network =
        match assoc "network" fields |> network_info_from_json with
        | None -> failwith "peer_from_json: can't parse network back"
        | Some network_info -> network_info
      in                      
      let protocols =
        match assoc "protocols" fields |> protocol_info_from_json with
        | None -> failwith "peer_from_json: can't parse protocol back"
        | Some protocol -> protocol
      in
      Some { caps; id; name; network; protocols }
    | _ ->
      None

let peer_info_from_json : Yojson.Basic.json -> peer_info =
  fun j ->
    let elts = Tools.drop_list j in
    List.map (fun x ->
        match peer_from_json x with
        | None     -> failwith "peer_info_from_json: can't parse peer back"
        | Some res -> res
      ) elts
