type t =
  | Simplify of Formula.t
  | Interpolant of Formula.t * Formula.t * Formula.Poly.t option * int

val load: string -> t list
