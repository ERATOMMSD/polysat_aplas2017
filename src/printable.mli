(** Printable *)

(** Signature for the input of {!Make}. *)
module type Base =
sig
  (** The type of printable objects. *)
  type t

  (** Pretty printer. *)
  val pp: Format.formatter -> t -> unit
end

(** Signature of an implementation of the printable type. *)
module type S =
sig
  type t
  include Base with type t := t

  (** [to_string t] converts [t] to the string representation. *)
  val to_string: t -> string
end

module Make(Base: Base) : S with type t = Base.t
