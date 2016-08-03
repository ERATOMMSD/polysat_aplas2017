module type S = sig
  type t
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
  val print: out_channel -> t -> unit
end
