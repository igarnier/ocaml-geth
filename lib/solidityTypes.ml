type t =
  (* atoms *)
  | UInt of int
  | Int of int
  | Address
  | Bool
  | Fixed of int * int
  | UFixed of int * int
  | NBytes of int
  | Bytes
  | String
  | Function
  (* composite *)
  | SArray of int * t
  | DArray of t
  | Tuple of t list
[@@deriving eq]

let uint w = UInt w
let int w = Int w
let string = String
let bytes = Bytes
let address = Address

let rec is_dynamic = function
  | Bytes | String | DArray _ -> true
  | Tuple typs -> List.exists is_dynamic typs
  | SArray (_, typ) when is_dynamic typ -> true
  | _ -> false

module Parser = struct
  open Angstrom

  let num =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let mxn = num >>= fun m -> char 'x' *> num >>| fun n -> (m, n)
  let numopt = option None (num >>| Option.some)

  let int =
    option false (char 'u' >>| fun _ -> true)
    >>= fun unsigned ->
    string "int" *> option 256 num
    >>| fun n -> if unsigned then UInt n else Int n

  let fixed =
    option false (char 'u' >>| fun _ -> true)
    >>= fun unsigned ->
    string "fixed" *> option (128, 18) mxn
    >>| fun (m, n) -> if unsigned then UFixed (m, n) else Fixed (m, n)

  let bytes = string "bytes" *> option Bytes (num >>| fun n -> NBytes n)
  let addr = string "address" >>| fun _ -> Address
  let bool = string "bool" >>| fun _ -> Bool
  let str = string "string" >>| fun _ -> String
  let func = string "function" >>| fun _ -> Function

  let tl =
    string "[]"
    >>| (fun _ -> `Dynamic)
    <|> (char '[' *> num <* char ']' >>| fun n -> `Static n)

  let atm = choice [int; fixed; bytes; addr; bool; str; func]

  let tpl expr =
    char '(' *> sep_by (char ',') expr <* char ')' >>| fun x -> Tuple x

  let expr expr =
    atm <|> tpl expr
    >>= fun hd ->
    many tl
    >>| List.fold_left
          (fun a -> function `Static n -> SArray (n, a) | `Dynamic -> DArray a)
          hd

  let t = fix expr
end

let of_string = Angstrom.parse_string ~consume:All Parser.t

let of_string_exn s =
  match of_string s with Ok x -> x | Error err -> failwith err

let rec to_string = function
  (* atoms *)
  | UInt 256 -> "uint"
  | Int 256 -> "int"
  | UInt w -> "uint" ^ string_of_int w
  | Int w -> "int" ^ string_of_int w
  | Address -> "address"
  | Bool -> "bool"
  | Fixed (128, 18) -> "fixed"
  | UFixed (128, 18) -> "ufixed"
  | Fixed (m, n) -> "fixed" ^ string_of_int m ^ "x" ^ string_of_int n
  | UFixed (m, n) -> "ufixed" ^ string_of_int m ^ "x" ^ string_of_int n
  | NBytes n -> "bytes" ^ string_of_int n
  | Bytes -> "bytes"
  | String -> "string"
  | Function -> "function"
  (* composite *)
  | SArray (length, typ) -> to_string typ ^ "[" ^ string_of_int length ^ "]"
  | DArray typ -> to_string typ ^ "[]"
  | Tuple types -> "(" ^ String.concat "," (List.map to_string types) ^ ")"

let pp ppf t = Format.pp_print_string ppf (to_string t)
