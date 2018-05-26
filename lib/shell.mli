
type command

val of_string : string -> command
val to_string : command -> string

val mkdir : string -> command
val cd    : string -> command
val mv    : string -> string -> command
val seq   : command -> command -> command
val nohup : command -> command
val background : command -> command
val cat : string -> command

val execute : ?read_stderr:bool -> ?timeout:int -> Ssh.Common.ssh_channel -> command -> string
