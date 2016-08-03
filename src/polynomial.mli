module type S = sig
  include Ring.S
  type var
  module Monomial: Monomial.S with type var = var
  module VarSet: Set.S with type elt = var
                        and type t = Monomial.VarSet.t
  module VarMap: Map.S with type key = var
  type coeff
  val var: var -> t
  val const: coeff -> t
  val eval: coeff VarMap.t -> t -> t
  exception Not_a_constant
  val to_const: t -> coeff
  val to_list: t -> (Monomial.t * coeff) list
  val vars: t -> VarSet.t
  val degree: t -> int
  module Op: sig
    val ( ?: ): var -> t
    val ( ! ): coeff -> t
    val ( ~- ): t -> t
    val ( + ): t -> t -> t
    val ( - ): t -> t -> t
    val ( * ): t -> t -> t
    val ( ** ): t -> int -> t
  end
end

module Make(Coeff: Ring.S) (Var: Variable.S) : S with type var = Var.t
                                                  and type coeff = Coeff.t
