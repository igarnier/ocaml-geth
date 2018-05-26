open Batteries
open Ssh

(* A basic shell on top of SSH. *)

type command = string

let of_string s = s
let to_string s = s  

let mkdir dir =
  Printf.sprintf "mkdir -p %s" dir

let cd dir =
  Printf.sprintf "cd %s" dir

let mv name1 name2 =
  Printf.sprintf "mv %s %s" name1 name2

let seq command1 command2 =
  command1 ^ ";" ^ command2

let nohup command =
  Printf.sprintf "nohup %s" command

let background command =
  command^" &"

let cat filename =
  "cat "^filename

let execute ?(read_stderr=false) ?(timeout=100) channel command =
  let open Client.Channel in
  match write channel (command^"\n") with
  | SSH_OK ->
    read_timeout channel read_stderr timeout
  | _ ->
    failwith "Shell.execute: error while writing command to channel"
