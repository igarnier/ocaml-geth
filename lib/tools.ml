open Batteries

let bits width z =
  let res = Array.init width (Z.testbit z) in
  Array.rev_in_place res ; res
