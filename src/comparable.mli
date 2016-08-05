(** Comparable *)

(** Signature for the input of {!Comparable.Make}. *)
module type Base =
sig
  (** The type of comparable objects. *)
  type t

  (** [compare] specifies the total ordering between objects.  [compare t1 t2]
      must return a negative integer, 0, or a positive integer if [t1 < t2], [t1 =
      t2], or [t1 > t2], respectively. *)
  val compare: t -> t -> int
end

(** Signature of operators for comparable objects. *)
module type Op =
sig
  (** The type of comparable objects. *)
  type t

  (** See {!Comparable.S.equal}. *)
  val ( = ): t -> t -> bool

  (** Negation of {!(=)}. *)
  val ( <> ): t -> t -> bool

  val ( < ): t -> t -> bool
  val ( > ): t -> t -> bool
  val ( <= ): t -> t -> bool
  val ( >= ): t -> t -> bool
end

module type S =
sig
  type t
  include Base with type t := t
  val min: t -> t -> t
  val max: t -> t -> t
  val equal: t -> t -> bool
  module Op: Op with type t = t
end

module Make(Base: Base) : S with type t = Base.t
