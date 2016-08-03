module type Base = sig
  type t
  val zero: t
  val one: t
  val neg: t -> t
  val add: t -> t -> t
  val mult: t -> t -> t
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
  val print: out_channel -> t -> unit
end

module type S = sig
  include Base
  val sub: t -> t -> t
  val power: t -> int -> t
  val equal: t -> t -> bool
end

module Make(Base: Base) : S with type t = Base.t = struct
  include Base

  let sub t1 t2 =
    add t1 (neg t2)

  let rec power t n =
    if n > 0 then mult t (power t (n - 1)) else one

  let equal t1 t2 =
    compare t1 t2 = 0
end

module type Op = sig
  type t
  val ( ~- ): t -> t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( ** ): t -> int -> t
end

module Operator(S: S) : Op with type t = S.t = struct
  type t = S.t
  let ( ~- ) = S.neg
  let ( + ) = S.add
  let ( - ) = S.sub
  let ( * ) = S.mult
  let ( ** ) = S.power
end
