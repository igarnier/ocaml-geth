open Batteries

type address = string
type hash256 = string
type hash512 = string

let address_to_string x = x

let address_from_string x =
  if String.length x != 42 || not (Bitstr.string_is_hex x) then
    failwith "address_from_string: input must be 20 bytes (40 hex chars) 0x-prefixed"
  else
    x

let hash256_to_string x = x
  
let hash256_from_string x =
  if String.length x != 66 || not (Bitstr.string_is_hex x) then
    failwith "hash256_from_string: input must be 32 bytes (64 hex chars) 0x-prefixed"
  else
    x

let hash512_to_string x = x
  
let hash512_from_string x =
  let len = String.length x in
  if len != 130 || not (Bitstr.string_is_hex x) then
    let open Printf in
    let msg = sprintf "hash512_from_string: input must be 64 bytes (128 hex chars) 0x-prefixed.\
 Got %s, length %d instead." x len in
    failwith msg
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
    network    : int option
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
  `String (Json.hex_of_int i)

let transaction_to_json : transaction -> Json.json =
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

let receipt_from_json : Json.json -> transaction_receipt option =
  fun j ->
    match j with
    | `Null -> None
    | `Assoc fields ->
      begin
        let block_hash   = assoc "blockHash" fields |> Json.drop_string in
        let block_number = assoc "blockNumber" fields |> Json.drop_int_as_string in
        let contract_address =
          match assoc "contractAddress" fields with
          | `String addr -> Some addr
          | `Null        -> None
          | _ ->
            failwith "Types.receipt_from_json: unexpected result"
        in
        let cumulative_gas_used = assoc "cumulativeGasUsed" fields |> Json.drop_int_as_string in
        let gas_used = assoc "gasUsed" fields |> Json.drop_int_as_string in    
        let src = assoc "from" fields |> Json.drop_string |> address_from_string in
        let dst =
          match assoc "to" fields with
          | `String addr -> Some (address_from_string addr)
          | `Null        -> None
          | _ ->
            failwith "Types.receipt_from_json: unexpected result"
        in
        let logs = assoc "logs" fields |> Json.drop_string_list in
        let transaction_hash = assoc "transactionHash" fields |> Json.drop_string in
        let transaction_index = assoc "transactionIndex" fields |> Json.drop_int_as_string in
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
      let s = Yojson.Safe.to_string j in
      failwith ("Types.receipt_from_json: unexpected json: "^s)

let port_info_from_json : Json.json -> port_info option =
  fun j ->
    match j with
    | `Assoc fields ->
      let discovery = assoc "discovery" fields |> Json.drop_int in
      let listener  = assoc "listener" fields |> Json.drop_int in
      Some { discovery; listener }
    | _ ->
      None

let protocol_info_from_json : Json.json -> protocol_info option =
  fun j ->
    let proto = Json.drop_assoc j |> assoc "eth" in
    match proto with
    | `Assoc fields ->
      (* TODO: use Json.Safe instead of Safe ... *)
      let difficulty = assoc "difficulty" fields |> Json.drop_int |> Z.of_int in
      let genesis =
        try Some (assoc "genesis" fields
                  |> Json.drop_string
                  |> hash256_from_string)
        with _ -> None
      in
      let head = assoc "head" fields
                 |> Json.drop_string
                 |> hash256_from_string
      in
      let network =
        try Some (assoc "network" fields |> Json.drop_int)
        with _ -> None
      in
      Some (Eth { difficulty; genesis; head; network })
    | _ ->
      None

let node_info_from_json : Json.json -> node_info option =
  fun j ->
    match j with
    | `Assoc fields ->
      let enode = assoc "enode" fields |> Json.drop_string in
      let id = assoc "id" fields
               |> Json.drop_string
               |> (fun x -> "0x"^x) (* HACK: for some reason geth does not prefix the hash with 0x. *)
               |> hash512_from_string
      in
      let ip = assoc "ip" fields |> Json.drop_string in
      let listen_addr = assoc "listenAddr" fields |> Json.drop_string in
      let name = assoc "name" fields |> Json.drop_string in
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

let network_info_from_json : Json.json -> network_info option =
  fun j ->
    match j with
    | `Assoc fields ->
      let local_address  = assoc "localAddress" fields |> Json.drop_string in
      let remote_address = assoc "remoteAddress" fields |> Json.drop_string in
      Some { local_address; remote_address }
    | _ ->
      None

let peer_from_json : Json.json -> peer option =
  fun j ->
    match j with
    | `Assoc fields ->
      let caps = assoc "caps" fields |> Json.drop_string_list in
      let id   = assoc "id" fields
                 |> Json.drop_string
                 |> (fun x -> "0x"^x) (* HACK: for some reason geth does not prefix the hash with 0x. *)                      
                 |> hash512_from_string
      in
      let name = assoc "name" fields |> Json.drop_string in
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

let peer_info_from_json : Json.json -> peer_info =
  fun j ->
    let elts = Json.drop_list j in
    List.map (fun x ->
        match peer_from_json x with
        | None     -> failwith "peer_info_from_json: can't parse peer back"
        | Some res -> res
      ) elts
