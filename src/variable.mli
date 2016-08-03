(** Variable module *)

(** Signature a variable module must satisfy *)
module type S = sig
  (** Type for varaibles *)
  type t

  (** Total ordering between two variables *)
  val compare: t -> t -> int

  (** Pretty printer *)
  val pp: Format.formatter -> t -> unit

  (** Unformatting printer *)
  val print: out_channel -> t -> unit
end
