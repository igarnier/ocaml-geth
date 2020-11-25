open Types
open Json_encoding

let q a =
  obj4
    (req "jsonrpc" (constant "2.0"))
    (req "id" int) (req "method" string) (req "params" a)

let n meth a =
  obj3
    (req "jsonrpc" (constant "2.0"))
    (req "method" (constant meth))
    (req "params" a)

type err = {code: int; msg: string; data: string} [@@deriving show]

let err =
  conv
    (fun _ -> assert false)
    (fun (code, msg, data) -> {code; msg; data})
    (obj3 (req "code" int) (req "message" string) (dft "data" string ""))

let or_error a =
  union
    [ case (obj1 (req "result" a)) (fun _ -> assert false) Result.ok;
      case (obj1 (req "error" err)) (fun _ -> assert false) Result.error ]

let r a =
  conv
    (fun _ -> assert false)
    (fun (((), id), x) -> (id, x))
    (merge_objs
       (obj2 (req "jsonrpc" (constant "2.0")) (req "id" int))
       (or_error a))

type sub =
  | NewHeads
  | Logs of {addr: Address.t option; topics: Hash256.t array}
  | NewTxs
  | Syncing
[@@deriving show]

type msg = Notification of string * evt | Response of {id: int; resp: resp}

and evt = NewHead of Block.t | Log of Log.t | NewTx of Hash256.t

and resp = Error of err | Subscribed of string [@@deriving show]

let sub =
  let atom =
    string_enum
      [ ("newHeads", NewHeads); ("newPendingTransactions", NewTxs);
        ("syncing", Syncing) ] in
  let log =
    tup2 (constant "logs")
      (obj2
         (opt "address" Address.encoding)
         (dft "topics" (array Hash256.encoding) [||])) in
  union
    [ case (tup1 atom) (function Logs _ -> None | x -> Some x) (fun x -> x);
      case log
        (function
          | Logs {addr; topics} -> Some ((), (addr, topics)) | _ -> None)
        (fun ((), (addr, topics)) -> Logs {addr; topics}) ]

let sub =
  conv
    (fun (id, args) -> ((), id, "eth_subscribe", args))
    (fun _ -> assert false)
    (q sub)

let notif =
  union
    [ case Block.encoding (fun _ -> assert false) (fun x -> NewHead x);
      case Log.encoding (fun _ -> assert false) (fun x -> Log x);
      case Hash256.encoding (fun _ -> assert false) (fun x -> NewTx x) ]

let msg =
  union
    [ case (r string)
        (fun _ -> assert false)
        (fun (id, resp) ->
          let resp =
            match resp with Error e -> Error e | Ok x -> Subscribed x in
          Response {id; resp});
      case
        (n "eth_subscription"
           (obj2 (req "subscription" string) (req "result" notif)))
        (fun _ -> assert false)
        (fun ((), (), (sub, evt)) -> Notification (sub, evt)) ]
