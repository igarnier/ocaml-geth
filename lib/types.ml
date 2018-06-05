module type FixedLengthString =
sig

  type t

  val bytes : int

  val to_hex : t -> string

  val from_hex : string -> t

end

module Hash256 : FixedLengthString =
struct

  type t = string

  let bytes = 256

  let to_hex (x : t) = x

  let from_hex (x : string) =
    if String.length x != (bytes * 2) then
      failwith "Address.from_hexadecimal_repr: address is not 20 bytes long"
    else
      x
end

module Address : FixedLengthString =
struct

  type t = string

  let bytes = 20

  let to_hex (x : t) = x

  let from_hex (x : string) =
    if String.length x != (bytes * 2) then
      failwith "Address.from_hexadecimal_repr: address is not 20 bytes long"
    else
      x
end

type wei       = int (* Z.t ? *)
type block_id  = int (* Z.t ? *)

type transaction =
  {
    src : Address.t;
    dst : Address.t option;
    gas : int option;
    gas_price : int option;
    value : int option;
    data : string;
    nonce : int option
  }

type transaction_receipt =
  {
    block_hash : string;
    block_number : int;
    contract_address : string option;
    cumulative_gas_used : int;
    src : Address.t; (* from *)
    dst : Address.t option;  (* to *)
    gas_used : int;
    logs : string list;
    (* logs_bloom : string;
     * root : string; *)
    transaction_hash : string;
    transaction_index : int
  }


let hex i =
  `String (Tools.hex_of_int i)

let transaction_to_json : transaction -> Yojson.Basic.json =
  fun t ->
    let args =
      [ ("from", `String (Address.to_hex t.src)) ]
      @ (match t.dst with Some x -> [("to", `String (Address.to_hex x))] | _ -> [])
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
        let src = assoc "from" fields |> Tools.drop_string |> Address.from_hex in
        let dst =
          match assoc "to" fields with
          | `String addr -> Some (Address.from_hex addr)
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

(*
        {
          "blockHash":"0xde1ddc84bfb3a2d2cfd4b8d64ebc35c861bafd93956a17c84d2560bea04af744",
                      "blockNumber":"0x248",
                      "contractAddress":"0x309fcd49e75914729b132466a339b3a9edc58db5",
                      "cumulativeGasUsed":"0xd8e0",
                      "from":"0x4b7815adb6e9b93200d4b377f91fa3a548d395bb",
                      "gasUsed":"0xd8e0",
                      "logs":[],
                      "logsBloom":"0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
                      "root":"0xc4e1180a575f0fdca4e2d969b332d55bcc2ca4031b858dc16df4c12e1c4eb7a7",
                      "to":null,
                      "transactionHash":"0x3f255dc3b38f7618f8cc20c50ed23f3baf16d7920f969ab28f01460754d0755b",
                      "transactionIndex":"0x0"}
*)
