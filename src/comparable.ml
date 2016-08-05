module type Base =
sig
  type t
  val compare: t -> t -> int
end

module type Op =
sig
  type t
  val ( = ): t -> t -> bool
  val ( <> ): t -> t -> bool
  val ( < ): t -> t -> bool
  val ( > ): t -> t -> bool
  val ( <= ): t -> t -> bool
  val ( >= ): t -> t -> bool
end

module type S =
sig
  type t
  include Base with type t := t
  val min: t -> t -> t
  val max: t -> t -> t
  val equal: t -> t -> bool
  module Op: Op with type t = t
end

module Make(Base: Base) : S with type t = Base.t =
struct
  include Base

  let min t1 t2 =
    if compare t1 t2 < 0 then t1 else t2

  let max t1 t2 =
    min t2 t1

  let equal t1 t2 =
    compare t1 t2 = 0

  module Op =
  struct
    type t = Base.t
    let ( = ) = equal
    let ( <> ) t1 t2 =
      not (t1 = t2)
    let ( < ) t1 t2 =
      compare t1 t2 < 0
    let ( > ) t1 t2 =
      compare t1 t2 > 0
    let ( <= ) t1 t2 =
      not (t2 > t1)
    let ( >= ) t1 t2 =
      not (t2 < t1)
  end
end
