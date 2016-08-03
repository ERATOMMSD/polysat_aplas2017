(** Polynomial module *)

(** Signature of the polynomial module *)
module type S = sig
  (** Type of variables *)
  type var

  (** Type of coefficients *)
  type coeff

  (** Type of polynomials *)
  type t

  (** Polynoial is ring. *)
  include Ring.S with type t := t

  (** Monomial module for the variables *)
  module Monomial: Monomial.S with type var = var

  (** Variable set module *)
  module VarSet: Set.S with type elt = var
                        and type t = Monomial.VarSet.t

  (** Variable map module *)
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
    variable type *)
module Make(Coeff: Ring.Base) (Var: Variable.S) : S with type var = Var.t
                                                     and type coeff = Coeff.t

(** Signature of an operators for polynomials *)
module type Op = sig
  type var

  type coeff

  type t

  include Ring.Op with type t := t

  (** Prefix notation of {!Polynomial.S.var} *)
  val ( ?: ): var -> t

  (** Prefix notation of {!Polynomial.S.const} *)
  val ( !: ): coeff -> t
end

(** Functor for building an implementation of operators *)
module Operator(Poly: S) : Op with type var = Poly.var
                               and type coeff = Poly.coeff
                               and type t = Poly.t
