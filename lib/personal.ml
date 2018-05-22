open Batteries

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

(* 
curl --
'
{
  "jsonrpc":"2.0",
  "method":"personal_sendTransaction",
  "params":[{ 
     "from" :"0x004ec07d2329997267Ec62b4166639513386F32E",
     "to"   :"0x00Aa39d30F0D20FF03a22cCfc30B7EfbFca597C2",
     "value":"0xde0b6b3a7640000"}, 
     "user"],
   "id":0 }

' -H "Content-Type: application/json" -X POST localhost:8540 
*)
