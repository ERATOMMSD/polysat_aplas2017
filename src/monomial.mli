(** Monomial module *)

(** Signature of an implementation of monomial module *)
module type S = sig
  (** Type of variables  *)
  type var

  (** Module for variable set *)
  module VarSet: Set.S with type elt = var

  (** Type of monomial *)
  type t

  (** The monoial [1] *)
  val one: t

  (** [var x d] returns the monomial [x^d].
      @raise Invalid_argument if [d] is not positive *)
  val var: var -> int -> t

  (** [mult m1 m2] returns the multiplied monomial [m1 * m2]. *)
  val mult: t -> t -> t

  (** [degree m] returns the degree of [m]. *)
  val degree: t -> int

  (** [vars m] returns the variables occuring [m]. *)
  val vars: t -> VarSet.t

  (** [var_degree x m] returns the degree of [x] in [m]. *)
  val var_degree: var -> t -> int

  (** [compare] defines total ordering between two monomials.  The ordering
      strongly depends on the given variable ordering and could be unexpected
      except equality checks. *)
  val compare: t -> t -> int

  (** Pretty printer *)
  val pp: Format.formatter -> t -> unit

  (** Unformatting printer *)
  val print: out_channel -> t -> unit
end

(** Functor for buliding an implementation with the given variable type *)
module Make(Var: Variable.S) : S with type var = Var.t
