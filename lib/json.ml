open CCFun
open Yojson.Safe

type errmsg = {code: int; msg: string}

exception JsonError of errmsg

let hex i = `String (Printf.sprintf "0x%x" i)
let zhex i = `String ("0x" ^ Z.format "%x" i)

(* let hex_of_int x = Printf.sprintf "0x%x" x
 *
 * let hex_of_bigint x = "0x"^(Z.format "%x" x) *)

let clean_dump json = Yojson.Safe.to_string json
let maybe f x = try Some (f x) with Failure _ -> None

let drop_assoc (x : t) =
  match x with
  | `Assoc xs -> xs
  | _ -> failwith ("drop_assoc: bad argument " ^ clean_dump x)

let drop_string (x : t) =
  match x with
  | `String s -> s
  | _ -> failwith ("drop_string: bad argument " ^ clean_dump x)

let drop_int (x : t) =
  match x with
  | `Int n -> n
  | _ -> failwith ("drop_int: bad argument " ^ clean_dump x)

let drop_int64 (x : t) = Int64.of_int (drop_int x)

let drop_bigint (x : t) =
  match x with
  | `Intlit n -> Z.of_string n
  | _ -> failwith ("drop_bigint: bad argument " ^ clean_dump x)

let drop_list (x : t) =
  match x with
  | `List xs -> xs
  | _ -> failwith ("drop_list: bad argument " ^ clean_dump x)

let drop_bool (x : t) =
  match x with
  | `Bool b -> b
  | _ -> failwith ("drop_bool: bad argument " ^ clean_dump x)

let drop_null (x : t) =
  match x with
  | `Null -> ()
  | _ -> failwith ("drop_null: bad argument " ^ clean_dump x)

let drop_int_as_string (x : t) =
  match x with
  | `String n -> int_of_string n
  | _ -> failwith ("drop_int_as_string: bad argument " ^ clean_dump x)

let drop_int64_as_string (x : t) =
  match x with
  | `String n -> Int64.of_int (int_of_string n)
  | _ -> failwith ("drop_int_as_string: bad argument " ^ clean_dump x)

let drop_bigint_as_string (x : t) =
  match x with
  | `String n -> Z.of_string n
  | _ -> failwith ("drop_int_as_string: bad argument " ^ clean_dump x)

let drop_string_list (x : t) =
  match x with
  | `List elts -> List.map drop_string elts
  | _ -> failwith ("drop_string_list: bad argument " ^ clean_dump x)

let parse_error (x : t) =
  let fields = drop_assoc x in
  let code = List.assoc "code" fields |> drop_int in
  let msg = List.assoc "message" fields |> drop_string in
  {code; msg}

module Get = struct
  let result (json : Yojson.Safe.t) =
    let json' = drop_assoc json in
    try Ok (List.assoc "result" json')
    with Not_found -> (
      try
        let errmsg = List.assoc "error" json' |> parse_error in
        Error errmsg
      with Not_found ->
        let s = Yojson.Safe.to_string json in
        failwith ("Json.Get.result: could not parse result " ^ s) )

  let bool json = json |> result |> Result.map drop_bool
  let null json = json |> result |> Result.map drop_null
  let int json = json |> result |> Result.map drop_int
  let bigint json = json |> result |> Result.map drop_bigint

  let int_as_string json =
    json |> result |> Result.map (int_of_string % drop_string)

  let bigint_as_string json =
    json |> result |> Result.map (Z.of_string % drop_string)

  let string json = json |> result |> Result.map drop_string

  let string_list json =
    json |> result |> Result.map (List.map drop_string % drop_list)
end

module GetExn = struct
  let result (json : Yojson.Safe.t) =
    let json' = drop_assoc json in
    try List.assoc "result" json'
    with Not_found ->
      let errmsg =
        try List.assoc "error" json' |> parse_error
        with Not_found -> failwith "Json.result: could not parse result" in
      let s = Yojson.Safe.to_string json in
      failwith ("Jgon.GetExn.result: " ^ errmsg.msg ^ "/" ^ s)

  let bool json = json |> result |> drop_bool
  let null json = json |> result |> drop_null
  let int json = json |> result |> drop_int
  let bigint json = json |> result |> drop_bigint
  let int_as_string json = json |> result |> int_of_string % drop_string
  let bigint_as_string json = json |> result |> Z.of_string % drop_string
  let string json = json |> result |> drop_string
  let string_list json = json |> result |> List.map drop_string % drop_list
end

let from_string x = Yojson.Safe.from_string x
let to_string x = Yojson.Safe.to_string x
