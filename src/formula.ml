module PPoly = Polynomial.Make(struct
    type t = Num.num
    let zero = Num.num_of_int 0
    let one = Num.num_of_int 1
    let add = Num.add_num
    let sub = Num.sub_num
    let mult = Num.mult_num
    let power t d = Num.(power_num t (num_of_int d))
    let compare = Num.compare_num
    let pp fmt t = Format.pp_print_string fmt (Num.string_of_num t)
  end)(struct
    type t = int
    let compare (t1: t) t2 = compare t1 t2
    let pp fmt t = Format.fprintf fmt "a%d" t
  end)

module Poly = Polynomial.Make(PPoly)(struct
    type t = string
    let compare (t1: t) t2 = compare t1 t2
    let pp fmt t = Format.pp_print_string fmt t
  end)

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
    | GeZ p -> conjunction (of_term (GeZ Poly.(zero - p))) (of_term (NeqZ p))
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
  of_term (EqZ Poly.(p1 - p2))

let neq p1 p2 =
  of_term (NeqZ Poly.(p1 - p2))

let ge p1 p2 =
  of_term (GeZ Poly.(p1 - p2))

let le p1 p2 =
  of_term (GeZ Poly.(p2 - p1))

let gt p1 p2 =
  negation (le p1 p2)

let lt p1 p2 =
  negation (ge p1 p2)

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
