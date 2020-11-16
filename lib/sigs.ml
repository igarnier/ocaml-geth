(* -------------------------------------------------------------------------- *)

module type Equalable = sig
  type t

  val equal : t -> t -> bool
end

module type Showable = sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module type Ordered = Map.OrderedType

(* -------------------------------------------------------------------------- *)
