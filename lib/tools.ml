let char_is_hex = function
  | '0' .. '9'
  | 'A' .. 'F'
  | 'a' .. 'f' -> true
  | _ -> false

let string_is_hex x =
  let rec loop i len x acc =
    if i = len then
      acc
    else
      loop (i+1) len x (acc && char_is_hex x.[i])
  in
  x.[0] = '0'
  && (x.[1] = 'x' || x.[1] = 'X')
  && (loop 2 (String.length x) x true)

