open Util

module type Variable = sig
  type t
  include Comparable.Base with type t := t
  include Printable.Base with type t := t
end

module type Coefficient = Ring.Base

module type Op = sig
  type var
  type coeff
  type t
  include Ring.Op with type t := t
  val ( ?: ): var -> t
  val ( !: ): coeff -> t
end

module type S = sig
  type var
  type coeff
  type t
  module Op: Op with type var = var and type coeff = coeff and type t = t
  include Ring.S with type t := t and module Op := Op
  module Monomial: Monomial.S with type var = var
  module VarSet: Set.S with type elt = var
                        and type t = Monomial.VarSet.t
  module VarMap: Map.S with type key = var
  val var: var -> t
  val const: coeff -> t
  val eval: coeff VarMap.t -> t -> t
  exception Not_a_constant
  val to_const: t -> coeff
  val to_list: t -> (Monomial.t * coeff) list
  val vars: t -> VarSet.t
  val degree: t -> int
end

module Make(Coeff: Coefficient) (Var: Variable) : S with type var = Var.t
                                                     and type coeff = Coeff.t = struct
  module Monomial = Monomial.Make(Var)

  module MonoMap = Map.Make(Monomial)

  module VarSet = Monomial.VarSet

  module VarMap = Map.Make(Var)

  type var = Var.t

  type coeff = Coeff.t

  module M = Ring.Make(struct
      (* Invariants: A representation map [t] must {e not} have the domain [x]
         such that [MonoMap.find x t = Coeff.zero]. *)
      type t = coeff MonoMap.t

      let zero =
        MonoMap.empty

      let one =
        MonoMap.singleton Monomial.one Coeff.one

      let add t1 t2 =
        MonoMap.union
          (fun _ c1 c2 ->
             let c = Coeff.add c1 c2 in
             (* GCing the domain if the calculated coefficient becomes zero *)
             if Coeff.(compare c zero) = 0 then None else Some c)
          t1 t2

      let neg t =
        MonoMap.map Coeff.neg t

      let mult t1 t2 =
        List.tupling (MonoMap.bindings t1) (MonoMap.bindings t2)
        |> List.map (fun ((m1, c1), (m2, c2)) ->
            (* Note [c1 * c2] could not be zero since [c1] and [c2] are not
                   zero *)
            MonoMap.singleton Monomial.(mult m1 m2) Coeff.(mult c1 c2))
        |> List.fold_left add zero

      let compare t1 t2 =
        MonoMap.compare Coeff.compare t1 t2

      let pp fmt t =
        let open Format in
        if compare t zero = 0 then
          pp_print_string fmt "0"
        else
          fprintf fmt "%a"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ + ")
               (fun fmt (m, c) ->
                  if Monomial.(compare m one) = 0 then
                    fprintf fmt "%a" Coeff.pp c
                  else if Coeff.(compare c one) = 0 then
                    fprintf fmt "%a" Monomial.pp m
                  else
                    fprintf fmt "%a * %a" Coeff.pp c Monomial.pp m))
            (MonoMap.bindings t)
    end)

  include (M : Ring.S with type t = M.t and module Op := M.Op)

  let var x =
    MonoMap.singleton (Monomial.var x 1) Coeff.one

  let const c =
    (* Pay attension invariants. *)
    if Coeff.(compare c zero) = 0 then zero else MonoMap.singleton Monomial.one c

  exception Not_a_constant

  let to_const t =
    if MonoMap.cardinal t = 1 then
      let m, c = MonoMap.choose t in
      if Monomial.(equal m one) then c else raise Not_a_constant
    else
      Coeff.zero

  let to_list t =
    MonoMap.bindings t

  let vars t =
    MonoMap.fold (fun m _ vars -> VarSet.union vars (Monomial.vars m)) t VarSet.empty

  let degree t =
    MonoMap.fold (fun m _ d -> max d (Monomial.degree m)) t 0

  let eval map t =
    let reduce m =
      VarSet.fold
        (fun x t ->
           let d = Monomial.var_degree x m in
           match VarMap.find x map with
           | exception Not_found -> mult (power (var x) d) t
           | c -> mult (power (const c) d) t)
        (Monomial.vars m)
        one
    in
    MonoMap.fold (fun m c t -> add (mult (reduce m) (const c)) t) t zero

  module Op = struct
    type var = Var.t

    type coeff = Coeff.t

    include M.Op

    let ( ?: ) = var

    let ( !: ) = const
  end
end
