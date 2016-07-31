type t =
  | Simplify of Formula.t

val load: string -> t list
