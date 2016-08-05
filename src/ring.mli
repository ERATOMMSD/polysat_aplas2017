(** Rings

    This module implement {e ring} structures.
*)

(** Signature a ring module must satisfy. *)
module type Base =
sig
  (** The type of rings. *)
  type t

  (** The additive identity. *)
  val zero: t

  (** The multiplicative identity. *)
  val one: t

  (** [neg t] returns the additive inverse of [t] *)
  val neg: t -> t

  (** Addition. *)
  val add: t -> t -> t

  (** Multiplication. *)
  val mult: t -> t -> t

  (** This ring is comparable. *)
  include Comparable.Base with type t := t

  include Printable.Base with type t := t
end

(** Signature of operators for a ring. *)
module type Op =
sig
  (** The type of the operation target. *)
  type t

  (** Prefix notation of {!Ring.Base.neg}. *)
  val ( ~- ): t -> t

  (** Infix notation of {!Ring.Base.add}. *)
  val ( + ): t -> t -> t

  (** Infix notation of {!Ring.S.sub}. *)
  val ( - ): t -> t -> t

  (** Infix notation of {!Ring.Base.mult}. *)
  val ( * ): t -> t -> t

  (** Infix notation of {!Ring.S.power}. *)
  val ( ** ): t -> int -> t

  include Comparable.Op with type t := t
end

(** Signature of an implementation of a ring module. *)
module type S =
sig
  (** The type of rings. *)
  type t

  (** Inherit the base signature. *)
  include Base with type t := t

  (** Subtraction. *)
  val sub: t -> t -> t

  (** [power t n] multiplies [t] [n]-times.  Note [power t n] is [one] if [n] is
      not positive. *)
  val power: t -> int -> t

  (** Equality between two rings. *)
  val equal: t -> t -> bool

  (** Operators. *)
  module Op: Op with type t = t
end

(** Functor for building an implementation of a ring *)
module Make(Base: Base) : S with type t = Base.t
