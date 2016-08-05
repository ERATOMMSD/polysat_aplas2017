module type Base = sig
  type t
  val zero: t
  val one: t
  val neg: t -> t
  val add: t -> t -> t
  val mult: t -> t -> t
  include Comparable.Base with type t := t
  include Printable.Base with type t := t
end

module type Op = sig
  type t
  val ( ~- ): t -> t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( ** ): t -> int -> t
  include Comparable.Op with type t := t
end

module type S = sig
  include Base
  val sub: t -> t -> t
  val power: t -> int -> t
  val equal: t -> t -> bool
  module Op: Op with type t = t
end

module Make(Base: Base) : S with type t = Base.t =
struct
  include Base

  let sub t1 t2 =
    add t1 (neg t2)

  let rec power t n =
    if n > 0 then mult t (power t (n - 1)) else one

  let equal t1 t2 =
    compare t1 t2 = 0

  module C = Comparable.Make(Base)
  include (C : Comparable.S with type t := t and module Op := C.Op)

  module Op = struct
    include C.Op
    let ( ~- ) = neg
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mult
    let ( ** ) = power
  end

  module P = Printable.Make(Base)
  include (P : Printable.S with type t := t)
end
