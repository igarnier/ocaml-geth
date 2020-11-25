open Json_encoding

type t = {contracts: (string * contract) list; version: string}

and contract = {funcs: ABI.Fun.t list; evts: ABI.Evt.t list; bin: Bitstring.t}

let find_function {funcs; _} name =
  List.find_opt (fun x -> String.equal x.ABI.Fun.name name) funcs

let hex =
  conv Bitstring.string_of_bitstring Bitstring.bitstring_of_string string

module X = Json_encoding.Make (Json_repr.Yojson)

let contract e =
  conv
    (fun {funcs; evts; bin} ->
      let abi =
        List.map (fun x -> ABI.Fun x) funcs
        @ List.map (fun x -> ABI.Event x) evts in
      ((), (abi, bin)))
    (fun ((), (abi, bin)) ->
      let funcs, evts =
        List.fold_left
          (fun (x, y) -> function ABI.Fun a -> (a :: x, y)
            | Event a -> (x, a :: y))
          ([], []) abi in
      {funcs; evts; bin})
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
