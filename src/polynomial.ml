module type S = sig
  include Ring.S
  type var
  module Monomial: Monomial.S with type var = var
  module VarSet: Set.S with type elt = var
                        and type t = Monomial.VarSet.t
  module VarMap: Map.S with type key = var
  type coeff
  val var: var -> t
  val const: coeff -> t
  val eval: coeff VarMap.t -> t -> t
  exception Not_a_constant
  val to_const: t -> coeff
  val to_list: t -> (Monomial.t * coeff) list
  val vars: t -> VarSet.t
  val degree: t -> int
  module Op: sig
    val ( ?: ): var -> t
    val ( ! ): coeff -> t
    val ( ~- ): t -> t
    val ( + ): t -> t -> t
    val ( - ): t -> t -> t
    val ( * ): t -> t -> t
    val ( ** ): t -> int -> t
  end
end

module Make(Coeff: Ring.S) (Var: Variable.S) : S with type var = Var.t
                                                  and type coeff = Coeff.t = struct
  module Monomial = Monomial.Make(Var)

  module MonoMap = Map.Make(Monomial)

  module VarSet = Monomial.VarSet

  module VarMap = Map.Make(Var)

  type var = Var.t

  type coeff = Coeff.t

  type t = coeff MonoMap.t

  let zero =
    MonoMap.empty

  let one =
    MonoMap.singleton Monomial.one Coeff.one

  let var x =
    MonoMap.singleton (Monomial.var x 1) Coeff.one

  let const c =
    if Coeff.(compare c zero) = 0 then zero else MonoMap.singleton Monomial.one c

  let add t1 t2 =
    MonoMap.union
      (fun _ c1 c2 ->
         let c = Coeff.add c1 c2 in
         if Coeff.(compare c zero) = 0 then None else Some c)
      t1 t2

  let neg t =
    MonoMap.map Coeff.neg t

  let sub t1 t2 =
    add t1 (neg t2)

  let mult t1 t2 =
    let f m c t =
      MonoMap.fold
        (fun mt ct t -> add (MonoMap.singleton (Monomial.mult m mt) (Coeff.mult c ct)) t)
        t
        zero
    in
    MonoMap.fold (fun m c t -> add (f m c t2) t) t1 zero

  let rec power t d =
    assert (d >= 0);
    if d = 0 then one else mult t (power t (d - 1))

  let compare t1 t2 =
    MonoMap.compare Coeff.compare t1 t2

  let eval map t =
    let f m =
      VarSet.fold
        (fun x t ->
           let d = Monomial.var_degree x m in
           match VarMap.find x map with
           | exception Not_found -> mult (power (var x) d) t
           | c -> mult (power (const c) d) t)
        (Monomial.vars m)
        one
    in
    MonoMap.fold (fun m c t -> add (mult (f m) (const c)) t) t zero

  exception Not_a_constant

  let to_const t =
    let m, c = MonoMap.choose t in
    if MonoMap.cardinal t = 1 && Monomial.(compare m one) = 0 then
      c
    else
      raise Not_a_constant

  let to_list t =
    MonoMap.bindings t

  let vars t =
    MonoMap.fold (fun m _ vars -> VarSet.union vars (Monomial.vars m))
      t VarSet.empty

  let degree t =
    MonoMap.fold (fun m _ d -> max d (Monomial.degree m)) t 0

  let pp fmt t =
    let open Format in
    if compare t zero = 0 then
      pp_print_string fmt "0"
    else
      fprintf fmt "@[<b>%a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ + ")
           (fun fmt (m, c) ->
              if Monomial.(compare m one) = 0 then
                fprintf fmt "@[<h>%a@]" Coeff.pp c
              else if Coeff.(compare c one) = 0 then
                fprintf fmt "@[<h>%a@]" Monomial.pp m
              else
                fprintf fmt "@[<h>%a * %a@]" Coeff.pp c Monomial.pp m))
        (MonoMap.bindings t)

  let print out t =
    let open Util.Printf in
    let print_mono out (m, c) =
      if Monomial.(compare m one) = 0 then
        fprintf out "%a" Coeff.print c
      else if Coeff.(compare c one) = 0 then
        fprintf out "%a" Monomial.print m
      else
        fprintf out "%a * %a" Coeff.print c Monomial.print m
    in
    if compare t zero = 0 then
      fprintf out "0"
    else
      fprintf out "%a" (print_list ~sep:" + " print_mono) (MonoMap.bindings t)

  module Op = struct
    let ( ?: ) x = var x
    let ( ! ) = const
    let ( ~- ) t = sub zero t
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mult
    let ( ** ) = power
  end
end

module Float = Make(struct
    type t = float
    let zero = 0.
    let one = 1.
    let neg = (~-.)
    let add = (+.)
    let sub = (-.)
    let mult = ( *.)
    let power t d = t ** float_of_int d
    let compare (t1: float) t2 = compare t1 t2
    let pp = Format.pp_print_float
    let print out t = Printf.fprintf out "%f" t
  end)(struct
    type t = int
    let compare (t1: int) t2 = compare t1 t2
    let pp fmt t = Format.fprintf fmt "x%d" t
    let print out t = Printf.fprintf out "x%d" t
  end)
