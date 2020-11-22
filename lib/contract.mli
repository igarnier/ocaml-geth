open Json_encoding

type t = {contracts: (string * contract) list; version: string}

and contract = {abi: ABI.t list; bin: Bitstring.t}

val find_function : contract -> string -> ABI.Fun.t option
val simple : contract encoding
val combined : t encoding
