(** Addresses are 20 bytes long, 40 bytes in hex form. *)
type address = private string

(** Hashes are 32 bytes long, 64 bytes in hex form. *)
type hash256 = private string

val address_to_string   : address -> string
val address_from_string : string -> address

val hash256_to_string   : hash256 -> string
val hash256_from_string : string -> hash256


type wei      = int
type block_id = int


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
    block_hash : string;
    block_number : int;
    contract_address : string option;
    cumulative_gas_used : int;
    src : address;
    dst : address option;
    gas_used : int;
    logs : string list;
    (* logs_bloom : string;
     * root : string; *)
    transaction_hash : string;
    transaction_index : int
  }

val transaction_to_json : transaction -> Yojson.Basic.json
val receipt_from_json : Yojson.Basic.json -> transaction_receipt option
