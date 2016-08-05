(** Polynomials

    This module implement polynomials.
*)

(** Signature of polynomials' variables. *)
module type Variable = sig
  type t
  include Comparable.Base with type t := t
  include Printable.Base with type t := t
end

(** Signature of polynomials' coefficients. *)
module type Coefficient = Ring.Base

(** Signature of operators for polynomials. *)
module type Op = sig
  (** The type of variables. *)
  type var

  (** The type of coefficitnes. *)
  type coeff

  (** The type of polynomials. *)
  type t

  include Ring.Op with type t := t

  (** Prefix notation of {!Polynomial.S.var} *)
  val ( ?: ): var -> t

  (** Prefix notation of {!Polynomial.S.const} *)
  val ( !: ): coeff -> t
end

(** Signature of an implementation of a polynomial module. *)
module type S = sig
  (** The type of variables. *)
  type var

  (** The type of coefficients. *)
  type coeff

  (** The type of polynomials. *)
  type t

  (** Operators. *)
  module Op: Op with type var = var and type coeff = coeff and type t = t

  (** Polynoial can be a ring. *)
  include Ring.S with type t := t and module Op := Op

  (** Monomial module of the variables. *)
  module Monomial: Monomial.S with type var = var

  (** Variable set module. *)
  module VarSet: Set.S with type elt = var
                        and type t = Monomial.VarSet.t

  (** Variable map module. *)
  module VarMap: Map.S with type key = var

  (** [var x] returns the polynomial [x]. *)
  val var: var -> t

  (** [const c] returns the polynomial [c]. *)
  val const: coeff -> t

  (** [eval assign t] returns the polynomial [t] with applying the value
      assignment [assign]. *)
  val eval: coeff VarMap.t -> t -> t

  (**)
  exception Not_a_constant

  (** [to_const t] converts [t] to coefficient value.
      @raise Not_a_constant if [t] is not a constant. *)
  val to_const: t -> coeff

  (** [to_list t] returns the list of pairs of monomial and coefficient. *)
  val to_list: t -> (Monomial.t * coeff) list

  (** [vars t] returns the set of variables occuring in [t]. *)
  val vars: t -> VarSet.t

  (** [degree t] returns the degree of [t]. *)
  val degree: t -> int
end

(** Functor for building an implementation with the given coefficient type and
    variable type. *)
module Make(Coeff: Coefficient) (Var: Variable) : S with type var = Var.t
                                                     and type coeff = Coeff.t
