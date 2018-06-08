(** Addresses are 20 bytes long, 40 bytes in hex form + 0x. *)
(* TODO: use Bitstr.hex_string *)
type address = private string

(** hash256 are 32 bytes long, 64 bytes in hex form + 0x. *)
type hash256 = private string

(** hash512 are 64 bytes long, 128 bytes in hex form + 0x. *)
type hash512 = private string


val address_to_string   : address -> string
val address_from_string : string -> address

val hash256_to_string   : hash256 -> string
val hash256_from_string : string -> hash256

val hash512_to_string   : hash512 -> string
val hash512_from_string : string -> hash512

type wei      = int
type block_id = int

type transaction =
  {
    src : address;
    dst : address option;
    gas : int option; (* Z.t *)
    gas_price : int option; (* Z.t *)
    value : int option; (* Z.t *)
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

val transaction_to_json : transaction -> Json.json
val receipt_from_json : Json.json -> transaction_receipt option
val node_info_from_json : Json.json -> node_info option
val peer_info_from_json : Json.json -> peer_info

