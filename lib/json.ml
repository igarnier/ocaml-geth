open Batteries

type errmsg = { code : int; msg : string }

type 'a result = ('a, errmsg) Result.t

exception JsonError of errmsg

type json = Yojson.Safe.json

let hex i =
  `String (Printf.sprintf "0x%x" i)

let zhex i =
  `String ("0x"^(Z.format "%x" i))

(* let hex_of_int x = Printf.sprintf "0x%x" x
 * 
 * let hex_of_bigint x = "0x"^(Z.format "%x" x) *)

let clean_dump json =
  Yojson.Safe.to_string json

let maybe f =
  fun x ->
    try Some (f x)
    with (Failure _) -> None

let drop_assoc (x : json) =
  match x with
  | `Assoc xs -> xs
  | _ ->
    failwith ("drop_assoc: bad argument "^(clean_dump x))

let drop_string (x : json) =
  match x with
  | `String s -> s
  | _ ->
    failwith ("drop_string: bad argument "^(clean_dump x))
      
let drop_int (x : json) =
  match x with
  | `Int n -> n
  | _ ->
    failwith ("drop_int: bad argument "^(clean_dump x))

let drop_int64 (x : json) =
  Int64.of_int (drop_int x)
  
let drop_bigint (x : json) =
  match x with
  | `Intlit n -> Z.of_string n
  | _ -> failwith ("drop_bigint: bad argument "^(clean_dump x))

let drop_list (x : json) =
  match x with
  | `List xs -> xs
  | _ ->
    failwith ("drop_list: bad argument "^(clean_dump x))

let drop_bool (x : json) =
  match x with
  | `Bool b -> b
  | _ ->
    failwith ("drop_bool: bad argument "^(clean_dump x))
      
let drop_null (x : json) =
  match x with
  | `Null -> ()
  | _ -> failwith ("drop_null: bad argument "^(clean_dump x))

let drop_int_as_string (x : json) =
  match x with
  | `String n -> int_of_string n
  | _ -> failwith ("drop_int_as_string: bad argument "^(clean_dump x))

let drop_int64_as_string (x : json) =
  match x with
  | `String n -> Int64.of_int (int_of_string n)
  | _ -> failwith ("drop_int_as_string: bad argument "^(clean_dump x))

let drop_bigint_as_string (x : json) =
  match x with
  | `String n -> Z.of_string n
  | _ -> failwith ("drop_int_as_string: bad argument "^(clean_dump x))

let drop_string_list (x : json) =
  match x with
  | `List elts ->
    List.map drop_string elts
  | _ ->
    failwith ("drop_string_list: bad argument "^(clean_dump x))

let parse_error (x : json) =
  let fields = drop_assoc x in
  let code   = List.assoc "code" fields |> drop_int in
  let msg    = List.assoc "message" fields |> drop_string in
  { code; msg }

module Get =
struct

  let result (json : Yojson.Safe.json) =
    let json = drop_assoc json in
    try Ok (List.assoc "result" json)
    with Not_found ->
      (try
         let errmsg = List.assoc "error" json |> parse_error in
         Bad errmsg
       with Not_found ->
         failwith "Json.result: could not parse result")

  let bool json =
    json |> result |> (Result.map drop_bool)

  let null json =
    json |> result |> (Result.map drop_null)

  let int json =
    json |> result |> (Result.map drop_int)

  let bigint json =
    json |> result |> (Result.map drop_bigint)

  let int_as_string json =
    json |> result |> (Result.map (int_of_string % drop_string))

  let bigint_as_string json =
    json |> result |> (Result.map (Z.of_string % drop_string))

  let string json =
    json |> result |> (Result.map drop_string)

  let string_list json =
    json |> result |> (Result.map (List.map drop_string % drop_list))

end

module GetExn =
struct

  let result (json : Yojson.Safe.json) =
    let json = drop_assoc json in
    try
      List.assoc "result" json
    with Not_found ->
      (let errmsg =
         try
           List.assoc "error" json |> parse_error
         with Not_found ->
           failwith "Json.result: could not parse result"
       in
       raise (JsonError errmsg))

  let bool json =
    json |> result |> drop_bool

  let null json =
    json |> result |> drop_null

  let int json =
    json |> result |> drop_int

  let bigint json =
    json |> result |> drop_bigint

  let int_as_string json =
    json |> result |> (int_of_string % drop_string)

  let bigint_as_string json =
    json |> result |> (Z.of_string % drop_string)

  let string json =
    json |> result |> drop_string

  let string_list json =
    json |> result |> (List.map drop_string % drop_list)

end

let from_string x = Yojson.Safe.from_string x
let to_string x = Yojson.Safe.to_string x

