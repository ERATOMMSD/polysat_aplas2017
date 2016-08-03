(** Ring module *)

(** Signature a ring module must satisfy *)
module type Base = sig
  (** Type of ring *)
  type t

  (** Additive identity *)
  val zero: t

  (** Multiplicative identity *)
  val one: t

  (** [neg t] returns the additive inverse of [t] *)
  val neg: t -> t

  (** Addition *)
  val add: t -> t -> t

  (** Multiplication *)
  val mult: t -> t -> t

  (** Total ordering between two rings *)
  val compare: t -> t -> int

  (** Pretty printer *)
  val pp: Format.formatter -> t -> unit

  (** Unformatting printer *)
  val print: out_channel -> t -> unit
end

(** Signature of an implementation of ring module *)
module type S = sig
  include Base

  (** Subtraction *)
  val sub: t -> t -> t

  (** [power t n] multiplies [t] [n]-times.  Note [power t n] is [one] if [n] is
      not positive. *)
  val power: t -> int -> t

  (** Equality between two rings *)
  val equal: t -> t -> bool
end

(** Functor for building an implementation of ring *)
module Make(Base: Base) : S with type t = Base.t

(** Signature of an operators for ring *)
module type Op = sig
  (** Type of ring which is intended {!Ring.Base.t} *)
  type t

  (** Prefix notation of {!Ring.Base.neg} *)
  val ( ~- ): t -> t

  (** Infix notation of {!Ring.Base.add} *)
  val ( + ): t -> t -> t

  (** Infix notation of {!Ring.S.sub} *)
  val ( - ): t -> t -> t

  (** Infix notation of {!Ring.Base.mult} *)
  val ( * ): t -> t -> t

  (** Infix notation of {!Ring.S.power} *)
  val ( ** ): t -> int -> t
end

(** Functor for building an implementation of operators *)
module Operator(S: S) : Op with type t = S.t
