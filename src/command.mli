type t =
  | Simplify of Formula.t
  | Interpolant of Formula.t * Formula.t * int

val load: string -> t list
