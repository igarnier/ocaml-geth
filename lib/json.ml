type json = Yojson.Safe.json

let hex_of_int x = Printf.sprintf "0x%x" x

let clean_dump json =
  Yojson.Safe.to_string json

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

let drop_string_list (x : json) =
  match x with
  | `List elts ->
    List.map drop_string elts
  | _ ->
    failwith ("drop_string_list: bad argument "^(clean_dump x))

let result (json : Yojson.Safe.json) =
  json |> drop_assoc |> List.assoc "result"

let bool json =
  json |> result |> drop_bool

let null json =
  json |> result |> drop_null

let int json =
  json |> result |> drop_int

let bigint json =
  json |> result |> drop_bigint

let int_as_string json =
  json |> result |> drop_string |> int_of_string

let bigint_as_string json =
  json |> result |> drop_string |> Z.of_string

let string json =
  json |> result |> drop_string

let string_list json =
  json |> result |> drop_list |> List.map drop_string

let from_string = Yojson.Safe.from_string
let to_string = Yojson.Safe.to_string                    
