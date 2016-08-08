(** MATLAB code generator *)

(** Print out constraints solving code to standard output. *)
val print_code: Formula.Poly.Matrix.t list -> Formula.PPoly.t list -> Formula.Poly.t -> Formula.Poly.t -> unit
