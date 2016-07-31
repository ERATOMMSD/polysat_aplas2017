module type S = sig
  module VarSet: Set.S
  type var
  type t
  val one: t
  val var: var -> int -> t
  val mult: t -> t -> t
  val degree: t -> int
  val vars: t -> VarSet.t
  val var_degree: var -> t -> int
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
end

module Make(Var: Variable.S) : S with type VarSet.elt = Var.t
                                and type var = Var.t = struct
  module VarSet = Set.Make(Var)

  module VarMap = Map.Make(Var)

  type var = Var.t

  type t = int VarMap.t

  let one =
    VarMap.empty

  let var x d =
    VarMap.singleton x d

  let mult t1 t2 =
    VarMap.union (fun _ i1 i2 -> Some (i1 + i2)) t1 t2

  let degree t =
    VarMap.fold (fun _ d acc -> d + acc) t 0

  let vars t =
    VarMap.fold (fun v _ acc -> VarSet.add v acc) t VarSet.empty

  let var_degree x t =
    try VarMap.find x t with Not_found -> 0

  let compare t1 t2 =
    VarMap.compare (fun (d1 : int) d2 -> compare d1 d2) t1 t2

  let pp fmt t =
    let open Format in
    if degree t = 0 then
      fprintf fmt "1"
    else
      fprintf fmt "@[<h>%a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ")
           (fun fmt (x, d) ->
              if d = 1 then
                fprintf fmt "%a" Var.pp x
              else
                fprintf fmt "%a^%d" Var.pp x d))
        (VarMap.bindings t)
end
