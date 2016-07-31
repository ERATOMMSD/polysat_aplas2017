module type S = sig
  module VarSet: Set.S
  type var
  type t
  val one: t
  val var: var -> int -> t
  val mult: t -> t -> t
  val degree: t -> int
  val vars: t -> VarSet.t
  val var_degree: var -> t -> int
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
end

module Make(Var: Variable.S) : S with type VarSet.elt = Var.t
                                and type var = Var.t
