open Batteries

type address = string
type hash256 = string
type hash512 = string

let address_to_string x = x

let address_from_string x =
  if String.length x != 42 || not (Bitstr.string_is_hex x) then
    failwith "address_from_string: input must be 20 bytes (40 hex chars) 0x-prefixed"
  else
    (x : address)

let hash256_to_string x = x

let hash256_from_string (x : string) =
  if String.length x != 66 || not (Bitstr.string_is_hex x) then
    failwith "hash256_from_string: input must be 32 bytes (64 hex chars) 0x-prefixed"
  else
    (x : hash256)

let hash512_to_string x = x

let hash512_from_string x =
  let len = String.length x in
  if len != 130 || not (Bitstr.string_is_hex x) then
    let open Printf in
    let msg = sprintf "hash512_from_string: input must be 64 bytes (128 hex chars) 0x-prefixed.\
                       Got %s, length %d instead." x len in
    failwith msg
  else
    (x : hash512)


type wei       = int (* Z.t ? *)
type block_id  = int (* Z.t ? *)

let hex i =
  `String Bitstr.(hex_as_string (hex_of_int i))

let zhex i =
  `String Bitstr.(hex_as_string (hex_of_bigint i))

let assoc key fields =
  try List.assoc key fields with
  | Not_found ->
    let json = Yojson.Safe.to_string (`Assoc fields) in
    failwith (Printf.sprintf "assoc: key %s not found in %s" key json)

module Tx =
struct
  
  type t =
    {
      src       : address;
      dst       : address option;
      gas       : Z.t option;
      gas_price : Z.t option;
      value     : Z.t option;
      data      : string;
      nonce     : int option
    }

  type accepted =
    {
      tx : t;
      block_hash   : hash256;
      block_number : int;
      tx_hash      : hash256;
      tx_index     : int
    }
    
  type receipt =
    {
      block_hash          : hash256;
      block_number        : int;
      contract_address    : address option;
      cumulative_gas_used : Z.t;
      gas_used            : Z.t;
      src                 : address;
      dst                 : address option;
      logs                : log list;
      (* unused:
       * logs_bloom : string;
       * root : string; *)
      transaction_hash    : hash256;
      transaction_index   : int
    }

  and log =
    {
      log_address           : address;
      log_topics            : hash256 list;
      log_data              : string; (* hex_string *)
      log_block_number      : int;
      log_transaction_hash  : hash256;
      log_transaction_index : int;
      log_block_hash        : hash256;
      log_index             : int;
      log_removed           : bool
    }

  let to_json (tx : t) =
    let args =
      [ ("from", `String (address_to_string tx.src)) ]
      @ (match tx.dst with Some x -> [("to", `String (address_to_string x))] | _ -> [])
      @ (match tx.gas with Some x -> [("gas", zhex x)] | _ -> [])
      @ (match tx.gas_price with Some x -> [("gasPrice", zhex x)] | _ -> [])
      @ (match tx.value with Some x -> [("value", zhex x)] | _ -> [])
      @ [("data", `String tx.data)]
      @ (match tx.nonce with Some x -> [("nonce", `Int x)] | _ -> [])
    in
    (`Assoc args)

  let accepted_from_json json =
    let fields = Json.drop_assoc json in
    let table  = Hashtbl.of_list fields in
    let find   = Hashtbl.find table in
    let src    = find "from"     |> Json.(address_from_string % drop_string) in
    let dst    = find "to"       |> Json.(address_from_string % drop_string) in
    let gas    = find "gas"      |> Json.drop_bigint_as_string in
    let gasprc = find "gasPrice" |> Json.drop_bigint_as_string in
    let value  = find "value"    |> Json.drop_bigint_as_string in
    let input  = find "input"    |> Json.drop_string in
    let nonce  = find "nonce"    |> Json.drop_int in
    let blkhsh = find "blockHash"   |> Json.(hash256_from_string % drop_string) in
    let blknmb = find "blockNumber" |> Json.drop_int in
    let txhash = find "hash"        |> Json.(hash256_from_string % drop_string) in
    let txindx = find "transactionIndex" |> Json.drop_int in
    {
      tx =
        {
          src;
          dst = Some dst;
          gas = Some gas;
          gas_price = Some gasprc;
          value = Some value;
          data = input;
          nonce = Some nonce
        };

      block_hash = blkhsh;
      block_number = blknmb;
      tx_hash = txhash;
      tx_index = txindx
    }


  let log_from_json json =
    match json with
    | `Assoc fields ->
      let table = Hashtbl.of_list fields in
      let find  = Hashtbl.find table in
      let log_address = find "address" |> Json.drop_string |> address_from_string in
      let log_topics  = find "topics"  |> Json.drop_list |> List.map (hash256_from_string % Json.drop_string) in
      let log_data    = find "data"    |> Json.drop_string in
      let log_block_number = find "blockNumber" |> Json.drop_int_as_string in
      let log_transaction_hash = find "transactionHash" |> Json.drop_string |> hash256_from_string in
      let log_transaction_index = find "transactionIndex" |> Json.drop_int_as_string in
      let log_block_hash = find "blockHash" |> Json.drop_string |> hash256_from_string in
      let log_index = find "logIndex" |> Json.drop_int_as_string in
      let log_removed = find "removed" |> Json.drop_bool in
      {
        log_address;
        log_topics;
        log_data;
        log_block_number;
        log_transaction_hash;
        log_transaction_index;
        log_block_hash;
        log_index;
        log_removed
      }
    | _ ->      
      let s = Yojson.Safe.to_string json in
      failwith ("Types.log_from_json: unexpected json: "^s)
  
  let receipt_from_json j =
    match j with
    | `Null -> None
    | `Assoc fields ->
      let block_hash   = assoc "blockHash" fields |> Json.drop_string in
      let block_number = assoc "blockNumber" fields |> Json.drop_int_as_string in
      let contract_address =
        match assoc "contractAddress" fields with
        | `String addr -> Some addr
        | `Null        -> None
        | _ ->
          failwith "Types.receipt_from_json: unexpected result"
      in
      let cumulative_gas_used = assoc "cumulativeGasUsed" fields |> Json.drop_bigint_as_string in
      let gas_used = assoc "gasUsed" fields |> Json.drop_bigint_as_string in    
      let src = assoc "from" fields |> Json.drop_string |> address_from_string in
      let dst =
        match assoc "to" fields with
        | `String addr -> Some (address_from_string addr)
        | `Null        -> None
        | _ ->
          failwith "Types.receipt_from_json: unexpected result"
      in
      let logs = assoc "logs" fields
                 |> Json.drop_list
                 |> List.map log_from_json
      in
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
    | _ ->
      let s = Yojson.Safe.to_string j in
      failwith ("Types.receipt_from_json: unexpected json: "^s)


end

module Block =
struct
  
  type t =
    {
      number        : int option;
      hash          : hash256 option;
      parent_hash   : hash256;
      nonce         : int option;
      sha3_uncles   : hash256;
      logs_bloom    : string option;
      transactions_root : hash256;
      state_root    : hash256;
      receipts_root : hash256;
      miner         : address;
      difficulty    : Z.t;
      total_difficulty : Z.t;
      extra_data    : string;
      size          : Z.t;
      gas_limit     : Z.t;
      gas_used      : Z.t;
      timestamp     : Z.t;
      transactions  : Tx.accepted list;
      uncles        : hash256 list
    }

  let from_json json =
    let fields   = Json.drop_assoc json in
    let table    = Hashtbl.of_list fields in
    let find     = Hashtbl.find table in
    let number   = find "number"           |> Json.(maybe drop_int_as_string) in
    let hash     = find "hash"             |> Json.(maybe (hash256_from_string % drop_string)) in
    let p_hash   = find "parentHash"       |> Json.(hash256_from_string % drop_string) in
    let nonce    = find "nonce"            |> Json.(maybe drop_int) in
    let sha3unc  = find "sha3Uncles"       |> Json.(hash256_from_string % drop_string) in
    let logsblm  = find "logsBloom"        |> Json.(maybe drop_string) in
    let txs_root = find "transactionsRoot" |> Json.(hash256_from_string % drop_string) in
    let st_root  = find "stateRoot"        |> Json.(hash256_from_string % drop_string) in
    let rc_root  = find "receiptsRoot"     |> Json.(hash256_from_string % drop_string) in
    let miner    = find "miner"            |> Json.(address_from_string % drop_string) in
    let diff     = find "difficulty"       |> Json.drop_bigint_as_string in
    let totdiff  = find "totalDifficulty"  |> Json.drop_bigint_as_string in
    let ex_data  = find "extraData"        |> Json.drop_string in
    let size     = find "size"             |> Json.drop_bigint_as_string in
    let gas_lim  = find "gasLimit"         |> Json.drop_bigint_as_string in
    let gas_used = find "gasUsed"          |> Json.drop_bigint_as_string in
    let tstamp   = find "timestamp"        |> Json.drop_bigint_as_string in
    let txs      = find "transactions"     |> Json.drop_list |> List.map Tx.accepted_from_json in
    let uncles   = find "uncles"           |> Json.drop_list |> List.map Json.(hash256_from_string % drop_string) in
    {
      number;
      hash;
      parent_hash = p_hash;
      nonce;
      sha3_uncles = sha3unc;
      logs_bloom  = logsblm;
      transactions_root = txs_root;
      state_root = st_root;
      receipts_root = rc_root;
      miner;
      difficulty = diff;
      total_difficulty = totdiff;
      extra_data = ex_data;
      size;
      gas_limit = gas_lim;
      gas_used;
      timestamp = tstamp;
      transactions = txs;
      uncles            
    }

end
 




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


type block_info =
  {
    block_root     : hash256;
    block_accounts : ba_info list
  }

and ba_info =
  {
    ba_account   : address;
    ba_balance   : Z.t;
    ba_code      : Evm.bytecode;
    ba_code_hash : hash256;
    ba_nonce     : int;
    ba_root      : hash256;
    ba_storage   : ba_storage
  }

(* The type of the data is not clear from the samples I got. It's clearly a hex string but what is
   the max length? *)
and ba_storage =
  (hash256 * string) list




(* let transaction_to_json : transaction -> Json.json =
 *   fun t ->
 *     let args =
 *       [ ("from", `String (address_to_string t.src)) ]
 *       @ (match t.dst with Some x -> [("to", `String (address_to_string x))] | _ -> [])
 *       @ (match t.gas with Some x -> [("gas", zhex x)] | _ -> [])
 *       @ (match t.gas_price with Some x -> [("gasPrice", zhex x)] | _ -> [])
 *       @ (match t.value with Some x -> [("value", zhex x)] | _ -> [])
 *       @ [("data", `String t.data)]
 *       @ (match t.nonce with Some x -> [("nonce", `Int x)] | _ -> [])
 *     in
 *     (`Assoc args) *)



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

let _0x s = "0x"^s

let block_from_json : Json.json -> block_info =
  fun j ->
    let fields = Json.drop_assoc j in
    let root =
      assoc "root" fields
      |> Json.drop_string
      |> _0x
      |> hash256_from_string
    in
    let accounts = assoc "accounts" fields |> Json.drop_assoc in
    let accounts =
      ListLabels.map accounts ~f:(fun (address, json) ->
          let fields = Json.drop_assoc json in
          let balance = assoc "balance" fields |> Json.drop_bigint_as_string in
          let code =
            assoc "code" fields
            |> Json.drop_string
            |> _0x
            |> Bitstr.hex_of_string
            |> Evm.parse_hexstring
          in
          let code_hash =
            assoc "codeHash" fields |> Json.drop_string |> _0x |> hash256_from_string
          in
          let nonce = assoc "nonce" fields |> Json.drop_int in
          let root  =
            assoc "root" fields |> Json.drop_string |> _0x |> hash256_from_string in
          let storage =
            assoc "storage" fields
            |> Json.drop_assoc
            |> List.map (fun (key, data) ->
                (hash256_from_string (_0x key), Json.drop_string data)
              )
          in
          {
            ba_account = address;
            ba_balance = balance;
            ba_code    = code;
            ba_code_hash = code_hash;
            ba_nonce   = nonce;
            ba_root    = root;
            ba_storage = storage
          }
        )
    in
    {
      block_root     = root;
      block_accounts = accounts
    }
