open CCFun
open Json_encoding

module type LEN = sig
  val length : int
end

module type BINARY = sig
  type t = private string [@@deriving ord, eq, show]

  val of_0x : string -> t
  val of_hex : Hex.t -> t
  val of_binary : ?pos:int -> string -> t
  val encoding : t Json_encoding.encoding
end

module Binary (X : LEN) = struct
  type t = string [@@deriving ord, eq]

  let of_0x x = Hex.to_string (`Hex (String.sub x 2 (2 * X.length)))
  let of_hex = Hex.to_string
  let of_binary ?(pos = 0) x = String.sub x pos X.length

  let show x =
    let (`Hex x) = Hex.of_string x in
    "0x" ^ x

  let pp ppf x = Format.pp_print_string ppf (show x)
  let encoding = conv show of_0x string
end

module Address = Binary (struct let length = 20 end)
module Hash256 = Binary (struct let length = 32 end)
module Hash512 = Binary (struct let length = 64 end)

module Z = struct
  include Z

  let show = to_string
  let pp = pp_print
end

let assoc key fields =
  try List.assoc key fields
  with Not_found ->
    let json = Yojson.Safe.to_string (`Assoc fields) in
    failwith (Printf.sprintf "assoc: key %s not found in %s" key json)

let inth = conv (fun i -> Printf.sprintf "0x%x" i) int_of_string string

let hex =
  conv
    (fun v -> "0x" ^ Hex.(show (of_string v)))
    (fun s ->
      let len = String.length s in
      Hex.to_string (`Hex (String.sub s 2 (len - 2))))
    string

module Tx = struct
  module Log = struct
    type t =
      { blkNum: int;
        blkHash: Hash256.t;
        txIdx: int;
        txHash: Hash256.t;
        logIdx: int;
        address: Address.t;
        topics: Hash256.t array;
        data: string;
        removed: bool }
    [@@deriving show]

    let encoding =
      conv
        (fun { blkNum;
               blkHash;
               txIdx;
               txHash;
               logIdx;
               address;
               topics;
               data;
               removed } ->
          ( blkNum,
            blkHash,
            txIdx,
            txHash,
            logIdx,
            address,
            topics,
            data,
            removed ))
        (fun ( blkNum,
               blkHash,
               txIdx,
               txHash,
               logIdx,
               address,
               topics,
               data,
               removed ) ->
          { blkNum;
            blkHash;
            txIdx;
            txHash;
            logIdx;
            address;
            topics;
            data;
            removed })
        (obj9 (req "blockNumber" inth)
           (req "blockHash" Hash256.encoding)
           (req "transactionIndex" inth)
           (req "transactionHash" Hash256.encoding)
           (req "logIndex" inth)
           (req "address" Address.encoding)
           (req "topics" (array Hash256.encoding))
           (req "data" hex) (req "removed" bool))
  end

  type t =
    { src: Address.t;
      dst: Address.t option;
      gas: Z.t option;
      gas_price: Z.t option;
      value: Z.t option;
      data: string;
      nonce: int option }

  type accepted =
    { tx: t;
      block_hash: Hash256.t;
      block_number: int;
      tx_hash: Hash256.t;
      tx_index: int }

  type receipt =
    { block_hash: Hash256.t;
      block_number: int;
      contract_address: Address.t option;
      cumulative_gas_used: Z.t;
      gas_used: Z.t;
      src: Address.t;
      dst: Address.t option;
      logs: Log.t list;
      (* unused:
       * logs_bloom : string;
       * root : string; *)
      transaction_hash: Hash256.t;
      transaction_index: int }
  [@@deriving show]

  let to_json (tx : t) =
    let args =
      [("from", `String (Address.show tx.src))]
      @ ( match tx.dst with
        | Some x -> [("to", `String (Address.show x))]
        | _ -> [] )
      @ (match tx.gas with Some x -> [("gas", Json.zhex x)] | _ -> [])
      @ ( match tx.gas_price with
        | Some x -> [("gasPrice", Json.zhex x)]
        | _ -> [] )
      @ (match tx.value with Some x -> [("value", Json.zhex x)] | _ -> [])
      @ [("data", `String tx.data)]
      @ match tx.nonce with Some x -> [("nonce", `Int x)] | _ -> [] in
    `Assoc args

  let accepted_from_json json =
    let fields = Json.drop_assoc json in
    let table = CCHashtbl.of_list fields in
    let find = Hashtbl.find table in
    let src = find "from" |> Json.(Address.of_0x % drop_string) in
    let dst = find "to" |> Json.(Address.of_0x % drop_string) in
    let gas = find "gas" |> Json.drop_bigint_as_string in
    let gasprc = find "gasPrice" |> Json.drop_bigint_as_string in
    let value = find "value" |> Json.drop_bigint_as_string in
    let input = find "input" |> Json.drop_string in
    let nonce = find "nonce" |> Json.drop_int in
    let blkhsh = find "blockHash" |> Json.(Hash256.of_0x % drop_string) in
    let blknmb = find "blockNumber" |> Json.drop_int in
    let txhash = find "hash" |> Json.(Hash256.of_0x % drop_string) in
    let txindx = find "transactionIndex" |> Json.drop_int in
    { tx=
        { src;
          dst= Some dst;
          gas= Some gas;
          gas_price= Some gasprc;
          value= Some value;
          data= input;
          nonce= Some nonce };
      block_hash= blkhsh;
      block_number= blknmb;
      tx_hash= txhash;
      tx_index= txindx }

  let receipt_from_json j =
    match j with
    | `Null -> None
    | `Assoc fields ->
        let block_hash =
          assoc "blockHash" fields |> Json.drop_string |> Hash256.of_0x in
        let block_number =
          assoc "blockNumber" fields |> Json.drop_int_as_string in
        let contract_address =
          match assoc "contractAddress" fields with
          | `String addr -> Some (Address.of_0x addr)
          | `Null -> None
          | _ -> failwith "Types.receipt_from_json: unexpected result" in
        let cumulative_gas_used =
          assoc "cumulativeGasUsed" fields |> Json.drop_bigint_as_string in
        let gas_used = assoc "gasUsed" fields |> Json.drop_bigint_as_string in
        let src = assoc "from" fields |> Json.drop_string |> Address.of_0x in
        let dst =
          match assoc "to" fields with
          | `String addr -> Some (Address.of_0x addr)
          | `Null -> None
          | _ -> failwith "Types.receipt_from_json: unexpected result" in
        let logs = [] (* FIXME *) in
        let transaction_hash =
          assoc "transactionHash" fields |> Json.drop_string |> Hash256.of_0x
        in
        let transaction_index =
          assoc "transactionIndex" fields |> Json.drop_int_as_string in
        Some
          { block_hash;
            block_number;
            contract_address;
            cumulative_gas_used;
            gas_used;
            src;
            dst;
            logs;
            transaction_hash;
            transaction_index }
    | _ ->
        let s = Yojson.Safe.to_string j in
        failwith ("Types.receipt_from_json: unexpected json: " ^ s)
end

module Block = struct
  type t =
    { number: int option;
      hash: Hash256.t option;
      parent_hash: Hash256.t;
      nonce: int option;
      sha3_uncles: Hash256.t;
      logs_bloom: string option;
      transactions_root: Hash256.t;
      state_root: Hash256.t;
      receipts_root: Hash256.t;
      miner: Address.t;
      difficulty: Z.t;
      total_difficulty: Z.t;
      extra_data: string;
      size: Z.t;
      gas_limit: Z.t;
      gas_used: Z.t;
      timestamp: Z.t;
      transactions: Tx.accepted list;
      uncles: Hash256.t list }

  let from_json json =
    let fields = Json.drop_assoc json in
    let table = CCHashtbl.of_list fields in
    let find = Hashtbl.find table in
    let number = find "number" |> Json.(maybe drop_int_as_string) in
    let hash = find "hash" |> Json.(maybe (Hash256.of_0x % drop_string)) in
    let p_hash = find "parentHash" |> Json.(Hash256.of_0x % drop_string) in
    let nonce = find "nonce" |> Json.(maybe drop_int) in
    let sha3unc = find "sha3Uncles" |> Json.(Hash256.of_0x % drop_string) in
    let logsblm = find "logsBloom" |> Json.(maybe drop_string) in
    let txs_root =
      find "transactionsRoot" |> Json.(Hash256.of_0x % drop_string) in
    let st_root = find "stateRoot" |> Json.(Hash256.of_0x % drop_string) in
    let rc_root = find "receiptsRoot" |> Json.(Hash256.of_0x % drop_string) in
    let miner = find "miner" |> Json.(Address.of_0x % drop_string) in
    let diff = find "difficulty" |> Json.drop_bigint_as_string in
    let totdiff = find "totalDifficulty" |> Json.drop_bigint_as_string in
    let ex_data = find "extraData" |> Json.drop_string in
    let size = find "size" |> Json.drop_bigint_as_string in
    let gas_lim = find "gasLimit" |> Json.drop_bigint_as_string in
    let gas_used = find "gasUsed" |> Json.drop_bigint_as_string in
    let tstamp = find "timestamp" |> Json.drop_bigint_as_string in
    let txs =
      find "transactions" |> Json.drop_list |> List.map Tx.accepted_from_json
    in
    let uncles =
      find "uncles" |> Json.drop_list
      |> List.map Json.(Hash256.of_0x % drop_string) in
    { number;
      hash;
      parent_hash= p_hash;
      nonce;
      sha3_uncles= sha3unc;
      logs_bloom= logsblm;
      transactions_root= txs_root;
      state_root= st_root;
      receipts_root= rc_root;
      miner;
      difficulty= diff;
      total_difficulty= totdiff;
      extra_data= ex_data;
      size;
      gas_limit= gas_lim;
      gas_used;
      timestamp= tstamp;
      transactions= txs;
      uncles }
end

type port_info = {discovery: int; listener: int}

type protocol_info =
  | Eth of
      { difficulty: Z.t;
        genesis: Hash256.t option;
        head: Hash256.t;
        network: int option }

type node_info =
  { enode: string;
    id: Hash512.t;
    ip: string;
    listen_addr: string;
    name: string;
    ports: port_info;
    protocols: protocol_info }

type network_info = {local_address: string; remote_address: string}

type peer =
  { caps: string list;
    id: Hash512.t;
    name: string;
    network: network_info;
    protocols: protocol_info }

type peer_info = peer list

type block_info = {block_root: Hash256.t; block_accounts: ba_info list}

and ba_info =
  { ba_account: Address.t;
    ba_balance: Z.t;
    ba_code: Evm.bytecode;
    ba_code_hash: Hash256.t;
    ba_nonce: int;
    ba_root: Hash256.t;
    ba_storage: ba_storage }

(* The type of the data is not clear from the samples I got. It's clearly a hex string but what is
   the max length? *)
and ba_storage = (Hash256.t * string) list

(* let transaction_to_json : transaction -> Yojson.Safe.t =
 *   fun t ->
 *     let args =
 *       [ ("from", `String (Address.show t.src)) ]
 *       @ (match t.dst with Some x -> [("to", `String (Address.show x))] | _ -> [])
 *       @ (match t.gas with Some x -> [("gas", zhex x)] | _ -> [])
 *       @ (match t.gas_price with Some x -> [("gasPrice", zhex x)] | _ -> [])
 *       @ (match t.value with Some x -> [("value", zhex x)] | _ -> [])
 *       @ [("data", `String t.data)]
 *       @ (match t.nonce with Some x -> [("nonce", `Int x)] | _ -> [])
 *     in
 *     (`Assoc args) *)

let port_info_from_json : Yojson.Safe.t -> port_info option =
 fun j ->
  match j with
  | `Assoc fields ->
      let discovery = assoc "discovery" fields |> Json.drop_int in
      let listener = assoc "listener" fields |> Json.drop_int in
      Some {discovery; listener}
  | _ -> None

let protocol_info_from_json : Yojson.Safe.t -> protocol_info option =
 fun j ->
  let proto = Json.drop_assoc j |> assoc "eth" in
  match proto with
  | `Assoc fields ->
      (* TODO: use Json.Safe instead of Safe ... *)
      let difficulty = assoc "difficulty" fields |> Json.drop_int |> Z.of_int in
      let genesis =
        try Some (assoc "genesis" fields |> Json.drop_string |> Hash256.of_0x)
        with _ -> None in
      let head = assoc "head" fields |> Json.drop_string |> Hash256.of_0x in
      let network =
        try Some (assoc "network" fields |> Json.drop_int) with _ -> None in
      Some (Eth {difficulty; genesis; head; network})
  | _ -> None

let node_info_from_json : Yojson.Safe.t -> node_info option =
 fun j ->
  match j with
  | `Assoc fields ->
      let enode = assoc "enode" fields |> Json.drop_string in
      let id =
        assoc "id" fields |> Json.drop_string
        |> (fun x -> "0x" ^ x)
        (* HACK: for some reason geth does not prefix the hash with 0x. *)
        |> Hash512.of_0x in
      let ip = assoc "ip" fields |> Json.drop_string in
      let listen_addr = assoc "listenAddr" fields |> Json.drop_string in
      let name = assoc "name" fields |> Json.drop_string in
      let ports =
        match assoc "ports" fields |> port_info_from_json with
        | None -> failwith "node_info_from_json: can't parse port_info"
        | Some port_info -> port_info in
      let protocols =
        match assoc "protocols" fields |> protocol_info_from_json with
        | None -> failwith "node_info_from_json: can't parse protocol_info"
        | Some protocol_info -> protocol_info in
      Some {enode; id; ip; listen_addr; name; ports; protocols}
  | _ -> None

let network_info_from_json : Yojson.Safe.t -> network_info option =
 fun j ->
  match j with
  | `Assoc fields ->
      let local_address = assoc "localAddress" fields |> Json.drop_string in
      let remote_address = assoc "remoteAddress" fields |> Json.drop_string in
      Some {local_address; remote_address}
  | _ -> None

let peer_from_json : Yojson.Safe.t -> peer option =
 fun j ->
  match j with
  | `Assoc fields ->
      let caps = assoc "caps" fields |> Json.drop_string_list in
      let id =
        assoc "id" fields |> Json.drop_string
        |> (fun x -> "0x" ^ x)
        (* HACK: for some reason geth does not prefix the hash with 0x. *)
        |> Hash512.of_0x in
      let name = assoc "name" fields |> Json.drop_string in
      let network =
        match assoc "network" fields |> network_info_from_json with
        | None -> failwith "peer_from_json: can't parse network back"
        | Some network_info -> network_info in
      let protocols =
        match assoc "protocols" fields |> protocol_info_from_json with
        | None -> failwith "peer_from_json: can't parse protocol back"
        | Some protocol -> protocol in
      Some {caps; id; name; network; protocols}
  | _ -> None

let peer_info_from_json : Yojson.Safe.t -> peer_info =
 fun j ->
  let elts = Json.drop_list j in
  List.map
    (fun x ->
      match peer_from_json x with
      | None -> failwith "peer_info_from_json: can't parse peer back"
      | Some res -> res)
    elts

let _0x s = "0x" ^ s

let block_from_json : Yojson.Safe.t -> block_info =
 fun j ->
  let fields = Json.drop_assoc j in
  let root = assoc "root" fields |> Json.drop_string |> _0x |> Hash256.of_0x in
  let accounts = assoc "accounts" fields |> Json.drop_assoc in
  let accounts =
    ListLabels.map accounts ~f:(fun (address, json) ->
        let address = Address.of_0x address in
        let fields = Json.drop_assoc json in
        let balance = assoc "balance" fields |> Json.drop_bigint_as_string in
        let code =
          assoc "code" fields |> Json.drop_string
          |> fun s -> Hex.to_string (`Hex s) |> Evm.parse in
        let code_hash =
          assoc "codeHash" fields |> Json.drop_string |> _0x |> Hash256.of_0x
        in
        let nonce = assoc "nonce" fields |> Json.drop_int in
        let root =
          assoc "root" fields |> Json.drop_string |> _0x |> Hash256.of_0x in
        let storage =
          assoc "storage" fields |> Json.drop_assoc
          |> List.map (fun (key, data) ->
                 (Hash256.of_0x (_0x key), Json.drop_string data)) in
        { ba_account= address;
          ba_balance= balance;
          ba_code= code;
          ba_code_hash= code_hash;
          ba_nonce= nonce;
          ba_root= root;
          ba_storage= storage }) in
  {block_root= root; block_accounts= accounts}
