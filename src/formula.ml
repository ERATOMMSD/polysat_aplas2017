module PPoly = struct
  module P = Polynomial.Make(struct
      type t = Num.num
      let zero = Num.num_of_int 0
      let one = Num.num_of_int 1
      let neg = Num.minus_num
      let add = Num.add_num
      let sub = Num.sub_num
      let mult = Num.mult_num
      let power t d = Num.(power_num t (num_of_int d))
      let compare = Num.compare_num
      let pp fmt t = Format.pp_print_string fmt (Num.string_of_num t)
      let print out t = Printf.fprintf out "%s" (Num.string_of_num t)
    end)(struct
      type t = int
      let compare (t1: t) t2 = compare t1 t2
      let pp fmt t = Format.fprintf fmt "a%d" t
      let print out t = Printf.fprintf out "a%d" t
    end)

  include P

  module Op = Polynomial.Operator(P)
end

module Poly = struct
  module P = Polynomial.Make(struct
      include PPoly
      let pp fmt t =
        if List.length (to_list t) = 1 then
          Format.fprintf fmt "@[%a@]" pp t
        else
          Format.fprintf fmt "@[(%a)@]" pp t
    end)(struct
      type t = string
      let compare (t1: t) t2 = compare t1 t2
      let pp fmt t = Format.pp_print_string fmt t
      let print out t = Printf.fprintf out "%s" t
    end)

  include P

  module Op = Polynomial.Operator(P)

  module Matrix = Matrix.Make(P)

  let counter = ref 0

  let new_param () =
    incr counter; const (PPoly.var !counter)

  let sdpvar d =
    let l = Util.List.count 0 d in
    let ut =
      List.map (fun i -> List.map (fun j -> if j < i then new_param () else zero) l) l
      |> Matrix.of_list_list
    in
    let diag =
      List.map (fun i -> List.map (fun j -> if i = j then new_param () else zero) l) l
      |> Matrix.of_list_list
    in
    Matrix.Op.(ut + diag + !ut)

  let sos vars d =
    let vars = List.map var (VarSet.elements vars) in
    let monos =
      List.mapi
        (fun i _ -> Util.List.mult_choose vars i)
        (Util.List.repeat () ((d + 3) / 2))
      |> List.concat
      |> List.map (List.fold_left mult one)
    in
    let psd = sdpvar (List.length monos) in
    let basis = Matrix.of_list_list [monos] in
    (psd, Matrix.(at 0 0 Op.(basis * psd * !basis)))

  let gen_cone tl vars d =
    let rec power = function
      | [] -> [[]]
      | x :: xs ->
          let l = power xs in
          l @ List.map (List.cons x) l
    in
    let combs = power tl |> List.map (List.fold_left mult one) in
    List.fold_left
      (fun (psds, cone) comb ->
         let psd, sos = sos vars d in
         (psd :: psds, Op.(cone + sos * comb)))
      ([], zero) combs

  let gen_ideal tl vars d =
    List.fold_left
      (fun (psds, ideal) t ->
         let psd1, sos1 = sos vars d in
         let psd2, sos2 = sos vars d in
         (psd1 :: psd2 :: psds, Op.(ideal + (sos1 - sos2) * t)))
      ([], zero) tl
end

module PolySet = Set.Make(Poly)

type term =
  | EqZ of Poly.t
  | NeqZ of Poly.t
  | GeZ of Poly.t
[@@deriving ord]

let pp_term fmt term =
  let open Format in
  match term with
  | EqZ p -> fprintf fmt "@[%a == 0@]" Poly.pp p
  | NeqZ p -> fprintf fmt "@[%a != 0@]" Poly.pp p
  | GeZ p -> fprintf fmt "@[%a >= 0@]" Poly.pp p

module ConjSet = Set.Make(struct
    type t = term
    let compare = compare_term
  end)

module DisjSet = Set.Make(ConjSet)

type t = DisjSet.t

let tru = DisjSet.singleton ConjSet.empty

let fls = DisjSet.empty

let of_term term =
  DisjSet.singleton (ConjSet.singleton term)

let disjunction t1 t2 =
  match DisjSet.equal t1 tru, DisjSet.equal t2 tru with
  | true, _ -> t2
  | _, true -> t1
  | _ -> DisjSet.union t1 t2

let conjunction t1 t2 =
  let is_contra conj =
    let eq, neq = ConjSet.fold
        (fun term (eq, neq) ->
           match term with
           | EqZ p -> (PolySet.add p eq, neq)
           | NeqZ p -> (eq, PolySet.add p neq)
           | GeZ _ -> (eq, neq))
        conj (PolySet.empty, PolySet.empty)
    in
    not (PolySet.is_empty (PolySet.inter eq neq))
  in
  DisjSet.fold
    (fun conj1 t ->
       disjunction
         (DisjSet.fold
            (fun conj2 t ->
               let conj = ConjSet.union conj1 conj2 in
               if is_contra conj then t else disjunction (DisjSet.singleton conj) t)
            t2 fls)
         t)
    t1 fls

let negation t =
  let neg_term = function
    | EqZ p -> of_term (NeqZ p)
    | NeqZ p -> of_term (EqZ p)
    | GeZ p -> conjunction (of_term (GeZ Poly.Op.(-p))) (of_term (NeqZ p))
  in
  DisjSet.fold
    (fun conj t ->
       conjunction
         (ConjSet.fold (fun term t -> disjunction (neg_term term) t) conj fls)
         t)
    t tru

let pp fmt t =
  let open Format in
  if DisjSet.equal t tru then
    pp_print_string fmt "TRUE"
  else if DisjSet.equal t fls then
    pp_print_string fmt "FALSE"
  else
    fprintf fmt "@[%a@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ || ")
         (fun fmt conj ->
            fprintf fmt "@[%a@]"
              (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ && ") pp_term)
              (ConjSet.elements conj)))
      (DisjSet.elements t)

let implication t1 t2 =
  disjunction (negation t1) t2

let disjunctions tl =
  List.fold_left disjunction fls tl

let conjunctions tl =
  List.fold_left conjunction tru tl

let eq p1 p2 =
  of_term (EqZ Poly.Op.(p1 - p2))

let neq p1 p2 =
  of_term (NeqZ Poly.Op.(p1 - p2))

let ge p1 p2 =
  of_term (GeZ Poly.Op.(p1 - p2))

let le p1 p2 =
  of_term (GeZ Poly.Op.(p2 - p1))

let gt p1 p2 =
  negation (le p1 p2)

let lt p1 p2 =
  negation (ge p1 p2)

let vars t =
  DisjSet.fold
    (ConjSet.fold
       (fun (EqZ p | NeqZ p | GeZ p) vars -> Poly.VarSet.union vars (Poly.vars p)))
    t Poly.VarSet.empty

type conj = { eqzs: Poly.t list; neqzs: Poly.t list; gezs: Poly.t list }

let to_dnf t =
  let conj conj = ConjSet.fold
      (fun term conj ->
         match term with
         | EqZ p -> { conj with eqzs = p :: conj.eqzs }
         | NeqZ p -> { conj with neqzs = p :: conj.neqzs }
         | GeZ p -> { conj with gezs = p :: conj.gezs })
      conj { eqzs = []; neqzs = []; gezs = [] }
  in
  List.map conj (DisjSet.elements t)

module Op = struct
  let ( && ) = conjunction
  let ( || ) = disjunction
  let not = negation
  let ( := ) = implication
  let ( == ) = eq
  let ( != ) = neq
  let ( >= ) = ge
  let ( <= ) = le
  let ( > ) = gt
  let ( < ) = lt
end
