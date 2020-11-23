open Json_encoding

type t = {contracts: (string * contract) list; version: string}

and contract = {funcs: ABI.Fun.t list; evts: ABI.Evt.t list; bin: Bitstring.t}

val find_function : contract -> string -> ABI.Fun.t option
val simple : contract encoding
val combined : t encoding
