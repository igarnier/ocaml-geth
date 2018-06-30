open Batteries

let bits width z =
  let res = Array.init width (Z.testbit z) in
  Array.rev_in_place res;
  res

let twos_complement_bits_of_z (z : Z.t) : string =
  failwith ""
  
  
