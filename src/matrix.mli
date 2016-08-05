(** Matrix *)

module type Element = Ring.Base

exception Dimention_error

module type S = sig
  type elt
  type t
  val of_list_list: elt list list -> t
  val to_list_list: t -> elt list list
  val at: int -> int -> t -> elt
  val transpose: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mult: t -> t -> t
  module Op: sig
    val ( ! ): t -> t
    val ( + ): t -> t -> t
    val ( - ): t -> t -> t
    val ( * ): t -> t -> t
  end
  include Printable.S with type t := t
end

module Make(E: Element) : S with type elt = E.t
