(* open Batteries
 * 
 * let char_is_hex = function
 *   | '0' .. '9'
 *   | 'A' .. 'F'
 *   | 'a' .. 'f' -> true
 *   | _ -> false
 * 
 * let string_is_hex x =
 *   let rec loop i len x acc =
 *     if i = len then
 *       acc
 *     else
 *       loop (i+1) len x (acc && char_is_hex x.[i])
 *   in
 *   x.[0] = '0'
 *   && (x.[1] = 'x' || x.[1] = 'X')
 *   && (loop 2 (String.length x) x true)
 * 
 * let compress_hex_string x =
 *   assert (string_is_hex x);
 *   let stripped = String.tail x 2 in
 *   Hex.to_string (`Hex stripped)
 * 
 * let int64_to_bitstring (i : int64) =
 *   let hex_of_int = Printf.sprintf "%Lx" i in
 *   compress_hex_string hex_of_int *)
