(** Formula module *)

(** Polynomial module for parameterized coefficients *)
module PPoly: sig
  type t

  include Polynomial.S with type var = int
                        and type coeff = Num.num
                        and type t := t
end

(** Polynomial module which coefficients are {!Formula.PPoly} *)
module Poly: sig
  type t

  include Polynomial.S with type var = string
                        and type coeff = PPoly.t
                        and type t := t

  module Matrix: Matrix.S with type elt = t

  val sos: VarSet.t -> int -> Matrix.t * t

  val gen_cone: t list -> VarSet.t -> int -> Matrix.t list * t

  val gen_strict_cone: t list -> int -> Matrix.t list * t * PPoly.t

  val gen_ideal: t list -> VarSet.t -> int -> Matrix.t list * t
end

(** Type of formulae *)
type t

(** Truth value [true] *)
val tru: t

(** Truth value [false] *)
val fls: t

(** [eq p1 p2] returns the formula [p1 == p2]. *)
val eq: Poly.t -> Poly.t -> t

(** [neq p1 p2] returns the formula [p1 != p2]. *)
val neq: Poly.t -> Poly.t -> t

(** [ge p1 p2] returns the formula [p1 >= p2]. *)
val ge: Poly.t -> Poly.t -> t

(** [le p1 p2] returns the formula [p1 <= p2]. *)
val le: Poly.t -> Poly.t -> t

(** [gt p1 p2] returns the formula [p1 > p2]. *)
val gt: Poly.t -> Poly.t -> t

(** [lt p1 p2] returns the formula [p1 < p2]. *)
val lt: Poly.t -> Poly.t -> t

(** [disjunction t1 t2] returns the formula [t1 \/ t2]. *)
val disjunction: t -> t -> t

(** [conjunction t1 t2] returns the formula [t1 /\ t2]. *)
val conjunction: t -> t -> t

(** [negation t] returns the formula [not t]. *)
val negation: t -> t

(** [implication t1 t2] returns the formula [t1 => t2]. *)
val implication: t -> t -> t

(** [disjunctions \[t1;..;tn\]] returns the formula [t1 \/ .. \/ tn]. *)
val disjunctions: t list -> t

(** [conjunctions \[t1;..;tn\]] returns the formula [t1 /\ .. /\ tn]. *)
val conjunctions: t list -> t

(** Pretty printer *)
val pp: Format.formatter -> t -> unit

(** [vars t] returns the set of variables occuring in [t]. *)
val vars: t -> Poly.VarSet.t

(** [syms_ip t] returns the set of SDP variables occuring in the coefficients of [t]. *)
val syms_ip: t -> PPoly.VarSet.t

(** [polys t] returns the set of polynomials. *)
val polys: t -> Poly.t list                    

(** Type of conjunctions of (in)equalities *)
type conj = { eqzs: Poly.t list; gtzs: Poly.t list; gezs: Poly.t list }

(** [to_dnf t] returns the DNF-form of [t]. *)
val to_dnf: t -> conj list

(** Operators for formulae *)
module Op: sig
  (** Infix notation of {!Formula.conjunction} *)
  val ( && ): t -> t -> t

  (** Infix notation of {!Formula.disjunction} *)
  val ( || ): t -> t -> t

  (** Prefix notation of {!Formula.negation} *)
  val not: t -> t

  (** Infix notation of {!Formula.implication} *)
  val ( := ): t -> t -> t

  (** Infix notation of {!Formula.neq} *)
  val ( != ): Poly.t -> Poly.t -> t

  (** Infix notation of {!Formula.ge} *)
  val ( >= ): Poly.t -> Poly.t -> t

  (** Infix notation of {!Formula.le} *)
  val ( <= ): Poly.t -> Poly.t -> t

  (** Infix notation of {!Formula.gt} *)
  val ( > ): Poly.t -> Poly.t -> t

  (** Infix notation of {!Formula.lt} *)
  val ( < ): Poly.t -> Poly.t -> t
end
