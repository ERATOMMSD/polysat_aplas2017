module PPoly: Polynomial.S with type var = int
                            and type coeff = Num.num

module Poly: Polynomial.S with type var = string
                           and type coeff = PPoly.t

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

val ( && ): t -> t -> t

val ( || ): t -> t -> t

val not: t -> t

val ( := ): t -> t -> t

val ( != ): Poly.t -> Poly.t -> t

val ( >= ): Poly.t -> Poly.t -> t

val ( <= ): Poly.t -> Poly.t -> t

val ( > ): Poly.t -> Poly.t -> t

val ( < ): Poly.t -> Poly.t -> t
