module type S = sig
  type t
  val zero: t
  val one: t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mult: t -> t -> t
  val power: t -> int -> t
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
end
