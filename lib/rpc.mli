open Types
open Json_encoding

type err = {code: int; msg: string; data: string}

type sub =
  | NewHeads
  | Logs of {addr: Address.t option; topics: Hash256.t array}
  | NewTxs
  | Syncing
[@@deriving show]

type msg = Notification of string * evt | Response of {id: int; resp: resp}

and evt = NewHead of Block.t | Log of Log.t | NewTx of Hash256.t

and resp = Error of err | Subscribed of string [@@deriving show]

val sub : (int * sub) encoding
val msg : msg encoding
