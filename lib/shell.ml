open Batteries
open Ssh_client

(* A basic shell on top of SSH. *)

type command = string

let of_string s = s
let to_string s = s  

let mkdir dir =
  Printf.sprintf "mkdir -p %s" dir

let cd dir =
  Printf.sprintf "cd %s" dir

let touch file =
  Printf.sprintf "touch %s" file

let mv name1 name2 =
  Printf.sprintf "mv %s %s" name1 name2

let seq command1 command2 =
  command1 ^ ";" ^ command2

let nohup command =
  Printf.sprintf "nohup %s" command

let stdout_to_file command filename =
  Printf.sprintf "%s > %s" command filename

let stderr_to_file command filename =
  Printf.sprintf "%s > %s" command filename

let background command =
  command^" &"

let cat filename =
  "cat "^filename

let screen command =
  Printf.sprintf "screen -dm %s" command

let flush_stdout channel =
  ignore (Raw.Channel.read_timeout channel false 300)

let execute = Easy.execute
