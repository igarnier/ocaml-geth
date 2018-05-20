open Printf
    
module Http_client = Nethttp_client

let new_account_from_phrase uri (id : Identity.t) =
  let args = Yojson.Basic.(`List [ `String id.login; `String id.pwd ]) in
  Rpc.call uri "parity_newAccountFromPhrase" args
