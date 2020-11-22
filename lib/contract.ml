open Json_encoding

type t = {contracts: (string * contract) list; version: string}

and contract = {abi: ABI.t list; bin: Bitstring.t}

let find_function {abi; _} name =
  List.find_map
    (function ABI.Fun x when String.equal x.name name -> Some x | _ -> None)
    abi

let hex =
  conv Bitstring.string_of_bitstring Bitstring.bitstring_of_string string

module X = Json_encoding.Make (Json_repr.Yojson)

let contract e =
  conv
    (fun {abi; bin} -> ((), (abi, bin)))
    (fun ((), (abi, bin)) -> {abi; bin})
    (merge_objs unit
       (obj2 (req "abi" e) (dft "bin" hex Bitstring.empty_bitstring)))

let simple = contract (list ABI.encoding)

let combined =
  let abis =
    conv
      (fun _ -> assert false)
      (fun x -> X.destruct (list ABI.encoding) (Yojson.Safe.from_string x))
      string in
  conv
    (fun {contracts; version} -> (contracts, version))
    (fun (contracts, version) -> {contracts; version})
    (obj2 (req "contracts" (assoc (contract abis))) (dft "version" string ""))
