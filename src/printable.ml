module type Base =
sig
  type t
  val pp: Format.formatter -> t -> unit
end

module type S =
sig
  type t
  include Base with type t := t
  val to_string: t -> string
end

module Make(Base: Base) : S with type t = Base.t =
struct
  include Base

  let to_string t =
    Format.asprintf "%a" pp t
end
