module type S = sig
  include Ring.S
  module VarMap: Map.S
  type var
  type coeff
  val var: var -> t
  val const: coeff -> t
  val eval: coeff VarMap.t -> t -> t
  exception Not_a_constant
  val to_const: t -> coeff
  val ( ?? ): var -> t
  val ( ! ): coeff -> t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( ** ): t -> int -> t
end

module Make(Coeff: Ring.S) (Var: Variable.S) : S with type var = Var.t
                                                  and type coeff = Coeff.t
