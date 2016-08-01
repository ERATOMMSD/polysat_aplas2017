exception Dimention_error

module type S = sig
  type elt
  type t
  val of_list_list: elt list list -> t
  val at: int -> int -> t -> elt
  val transpose: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mult: t -> t -> t
  val pp: Format.formatter -> t -> unit
  module Op: sig
    val ( ! ): t -> t
    val ( + ): t -> t -> t
    val ( - ): t -> t -> t
    val ( * ): t -> t -> t
  end
end

module Make(E: Ring.S) : S with type elt = E.t = struct
  type elt = E.t

  type t = E.t list list

  let of_list_list l =
    if List.length l = 0 then
      raise Dimention_error
    else
      let cols = List.length (List.hd l) in
      if cols = 0 || not (List.for_all (fun r -> List.length r = cols) l) then
        raise Dimention_error
      else
        l

  let at i j t =
    try List.nth (List.nth t i) j with
    | Invalid_argument _ | Failure _ -> raise Dimention_error

  let transpose t =
    List.mapi (fun j _ -> List.map (fun r -> List.nth r j) t) (List.hd t)

  let add t1 t2 =
    try List.map2 (List.map2 E.add) t1 t2 with
    | Invalid_argument _ -> raise Dimention_error

  let sub t1 t2 =
    try List.map2 (List.map2 E.sub) t1 t2 with
    | Invalid_argument _ -> raise Dimention_error

  let mult t1 t2 =
    let t2 = transpose t2 in
    List.map
      (fun r1 ->
         List.map
           (fun r2 ->
              try
                List.fold_left2
                  (fun a a1 a2 -> E.add a (E.mult a1 a2))
                  E.zero r1 r2
              with
              | Invalid_argument _ -> raise Dimention_error)
           t2)
      t1

  let pp fmt t =
    let open Format in
    fprintf fmt "[@[<v>%a@]]"
      (pp_print_list
         (fun fmt r -> fprintf fmt "@[<h>%a@]"
             (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") E.pp)
             r))
      t

  module Op = struct
    let ( ! ) = transpose
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mult
  end
end

