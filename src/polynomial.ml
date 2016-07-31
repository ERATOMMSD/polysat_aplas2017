module type S = sig
  include Ring.S
  module VarMap: Map.S
  type var
  type coeff
  val var: var -> t
  val const: coeff -> t
  val eval: coeff VarMap.t -> t -> t
  exception Not_a_constant
  val to_const: t -> coeff
  val ( ?? ): var -> t
  val ( ! ): coeff -> t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( ** ): t -> int -> t
end

module Make(Coeff: Ring.S) (Var: Variable.S) : S with type var = Var.t
                                                  and type coeff = Coeff.t = struct
  module Mono = Monomial.Make(Var)

  module MonoMap = Map.Make(Mono)

  module VarMap = Map.Make(Var)

  type var = Var.t

  type coeff = Coeff.t

  type t = coeff MonoMap.t

  let zero =
    MonoMap.empty

  let one =
    MonoMap.singleton Mono.one Coeff.one

  let var x =
    MonoMap.singleton (Mono.var x 1) Coeff.one

  let const c =
    if Coeff.(compare c zero) = 0 then zero else MonoMap.singleton Mono.one c

  let add t1 t2 =
    MonoMap.union
      (fun _ c1 c2 ->
         let c = Coeff.add c1 c2 in
         if Coeff.(compare c zero) = 0 then None else Some c)
      t1 t2

  let sub t1 t2 =
    MonoMap.union
      (fun _ c1 c2 ->
         let c = Coeff.sub c1 c2 in
         if Coeff.(compare c zero) = 0 then None else Some c)
      t1 t2

  let mult t1 t2 =
    let f m c t =
      MonoMap.fold
        (fun mt ct t -> add (MonoMap.singleton (Mono.mult m mt) (Coeff.mult c ct)) t)
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
      Mono.VarSet.fold
        (fun x t ->
           let d = Mono.var_degree x m in
           match VarMap.find x map with
           | exception Not_found -> mult (power (var x) d) t
           | c -> mult (power (const c) d) t)
        (Mono.vars m)
        one
    in
    MonoMap.fold (fun m c t -> add (mult (f m) (const c)) t) t zero

  exception Not_a_constant

  let to_const t =
    let m, c = MonoMap.choose t in
    if MonoMap.cardinal t = 1 && Mono.(compare m one) = 0 then
      c
    else
      raise Not_a_constant

  let pp fmt t =
    let open Format in
    fprintf fmt "@[<hov>%a@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ + ")
         (fun fmt (m, c) ->
            if Mono.(compare m one) = 0 then
              fprintf fmt "@[<h>%a@]" Coeff.pp c
            else if Coeff.(compare c one) = 0 then
              fprintf fmt "@[<h>%a@]" Mono.pp m
            else
              fprintf fmt "@[<h>%a * %a@]" Coeff.pp c Mono.pp m))
      (MonoMap.bindings t)

  let ( ?? ) x = var x
  let ( ! ) = const
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mult
  let ( ** ) = power
end

module Float = Make(struct
    type t = float
    let zero = 0.
    let one = 1.
    let add = (+.)
    let sub = (-.)
    let mult = ( *.)
    let power t d = t ** float_of_int d
    let compare (t1: float) t2 = compare t1 t2
    let pp = Format.pp_print_float
  end)(struct
    type t = int
    let compare (t1: int) t2 = compare t1 t2
    let pp fmt t = Format.fprintf fmt "x%d" t
  end)
