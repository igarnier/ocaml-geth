type command

val of_string : string -> command
val to_string : command -> string

val mkdir : string -> command
val cd    : string -> command
val mv    : string -> string -> command
val rm_fr : string -> command  
val touch : string -> command
val seq   : command -> command -> command
val nohup : command -> command
val background : command -> command
val cat    : string -> command
val screen : command -> command

val flush_stdout : Ssh_client.Types.ssh_channel -> unit

val execute : ?read_stderr:bool -> ?read_timeout:int -> Ssh_client.Easy.shell_handle -> command -> string
