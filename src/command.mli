(** Command parser *)

type t =
  | Simplify of Formula.t
  | Interpolant of Formula.t * Formula.t * Formula.Poly.t option * int

(** [load filename] reads commands from the file [filename]. *)
val load: string -> t list
