module PPoly: Polynomial.S with type var = int
                            and type coeff = Num.num

module Poly: sig
  include Polynomial.S with type var = string
                        and type coeff = PPoly.t
  module Matrix: Matrix.S with type elt = t
  val sos: VarSet.t -> int -> Matrix.t * t
  val gen_cone: t list -> VarSet.t -> int -> Matrix.t list * t
  val gen_ideal: t list -> VarSet.t -> int -> Matrix.t list * t
end

type t

val tru: t

val fls: t

val eq: Poly.t -> Poly.t -> t

val neq: Poly.t -> Poly.t -> t

val ge: Poly.t -> Poly.t -> t

val le: Poly.t -> Poly.t -> t

val gt: Poly.t -> Poly.t -> t

val lt: Poly.t -> Poly.t -> t

val disjunction: t -> t -> t

val conjunction: t -> t -> t

val negation: t -> t

val implication: t -> t -> t

val disjunctions: t list -> t

val conjunctions: t list -> t

val pp: Format.formatter -> t -> unit

val vars: t -> Poly.VarSet.t

type conj = { eqzs: Poly.t list; neqzs: Poly.t list; gezs: Poly.t list }

val to_dnf: t -> conj list

module Op: sig
  val ( && ): t -> t -> t
  val ( || ): t -> t -> t
  val not: t -> t
  val ( := ): t -> t -> t
  val ( != ): Poly.t -> Poly.t -> t
  val ( >= ): Poly.t -> Poly.t -> t
  val ( <= ): Poly.t -> Poly.t -> t
  val ( > ): Poly.t -> Poly.t -> t
  val ( < ): Poly.t -> Poly.t -> t
end
