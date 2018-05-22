open Batteries

(* https://wiki.parity.io/JSONRPC-personal-module#personal_sendtransaction *)

let send_transaction ~uri ~src ~dst ~value ~src_pwd =
  let value = Printf.sprintf "0x%x" value in
  let args  = Yojson.Basic.(
      `List [`Assoc [ ("from", `String src);
                      ("to", `String dst);
                      ("value", `String value) ];
             `String src_pwd
            ])
  in
  Rpc.call uri "personal_sendTransaction" args

