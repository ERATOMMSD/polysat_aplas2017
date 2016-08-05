module type Variable = sig
  type t
  include Comparable.Base with type t := t
  include Printable.Base with type t := t
end

module type S = sig
  type var
  module VarSet: Set.S with type elt = var
  type t
  val one: t
  val var: var -> int -> t
  val mult: t -> t -> t
  val degree: t -> int
  val vars: t -> VarSet.t
  val var_degree: var -> t -> int
  include Comparable.S with type t := t
  include Printable.S with type t := t
end

module Make(Var: Variable) : S with type var = Var.t = struct
  type var = Var.t

  module VarSet = Set.Make(Var)

  module VarMap = Map.Make(Var)

  type monomial = int VarMap.t

  type t = monomial

  let one =
    VarMap.empty

  let var x d =
    if d <= 0 then invalid_arg "Monomial.var"
    else VarMap.singleton x d

  let mult t1 t2 =
    VarMap.union (fun _ i1 i2 -> Some (i1 + i2)) t1 t2

  let degree t =
    VarMap.fold (fun _ d acc -> d + acc) t 0

  let vars t =
    VarMap.fold (fun v _ acc -> VarSet.add v acc) t VarSet.empty

  let var_degree x t =
    try VarMap.find x t with Not_found -> 0

  module C = Comparable.Make(struct
      type t = monomial

      let compare t1 t2 =
        VarMap.compare (fun (d1 : int) d2 -> compare d1 d2) t1 t2
    end)
  include (C : Comparable.S with type t := t)

  module P = Printable.Make(struct
      type t = monomial

      let pp fmt t =
        let open Format in
        if degree t = 0 then
          fprintf fmt "1"
        else
          fprintf fmt "%a"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ")
               (fun fmt (x, d) ->
                  if d = 1 then
                    fprintf fmt "%a" Var.pp x
                  else
                    fprintf fmt "%a^%d" Var.pp x d))
            (VarMap.bindings t)
    end)
  include (P : Printable.S with type t:= t)
end
