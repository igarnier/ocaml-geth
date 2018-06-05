let hex_of_int x = Printf.sprintf "0x%x" x

let clean_dump json =
  Yojson.Basic.to_string json

let drop_assoc  = function `Assoc xs -> xs | j -> failwith ("drop_assoc: bad argument "^(clean_dump j))
let drop_string = function `String s -> s | j -> failwith ("drop_string: bad argument "^(clean_dump j))
let drop_int    = function `Int n -> n | j -> failwith ("drop_int: bad argument "^(clean_dump j))
let drop_list   = function `List xs -> xs | j -> failwith ("drop_list: bad argument "^(clean_dump j))
let drop_bool   = function `Bool b -> b | j -> failwith ("drop_bool: bad argument "^(clean_dump j))
let drop_null   = function `Null -> () | j -> failwith ("drop_null: bad argument "^(clean_dump j))

let drop_int_as_string json =
  match json with
  | `String n -> int_of_string n
  | _ -> failwith ("drop_int_as_string: bad argument "^(clean_dump json))

let drop_string_list json =
  match json with
  | `List elts ->
    List.map drop_string elts
  | _ ->
    failwith ("drop_string_list: bad argument "^(clean_dump json))


let result (json : Yojson.Basic.json) =
  json |> drop_assoc |> List.assoc "result"

let bool json =
  json |> result |> drop_bool

let null json =
  json |> result |> drop_null

let int json =
  json |> result |> drop_int

let int_as_string json =
  json |> result |> drop_string |> int_of_string

let bigint_as_string json =
  json |> result |> drop_string |> Z.of_string

let string json =
  json |> result |> drop_string

let string_list json =
  json |> result |> drop_list |> List.map drop_string

