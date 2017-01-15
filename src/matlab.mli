(** MATLAB code generator *)

(** Print out constraints solving code to standard output. *)
val print_code: Formula.t -> Formula.t -> Constraint.sdp list -> unit
