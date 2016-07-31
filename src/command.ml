open Sexplib

let rec sexp_of_poly sexp =
  match sexp with
  | Sexp.Atom i ->
      begin match int_of_string i with
      | exception Failure _ -> Formula.Poly.var i
      | n -> Formula.Poly.const (Formula.PPoly.const (Num.num_of_int n))
      end
  | Sexp.List (Sexp.Atom op :: args) ->
      let args = List.map sexp_of_poly args in
      begin match op with
      | "+" -> List.fold_left Formula.Poly.add Formula.Poly.zero args
      | "-" ->
          if List.length args > 1 then
            List.fold_left Formula.Poly.sub (List.hd args) (List.tl args)
          else
            List.fold_left Formula.Poly.sub Formula.Poly.zero args
      | "*" -> List.fold_left Formula.Poly.mult Formula.Poly.one args
      | "/" ->
          let args =
            match
              List.map (fun c -> Formula.PPoly.to_const (Formula.Poly.to_const c)) args
            with
            | exception Formula.Poly.Not_a_constant ->
                Conv.of_sexp_error "division only accepts constants" sexp
            | args -> args
          in
          if List.length args = 0 then
            Conv.of_sexp_error "division requires at least one argument" sexp
          else
            List.fold_left Num.div_num (List.hd args) (List.tl args)
            |> Formula.PPoly.const |> Formula.Poly.const
      | _ -> Conv.of_sexp_error "polynomial formula is expected" sexp
      end
  | _ -> Conv.of_sexp_error "invalid formula" sexp

let rec sexp_of_formula sexp =
  match sexp with
  | Sexp.List (Sexp.Atom "and" :: args) ->
      Formula.conjunctions (List.map sexp_of_formula args)
  | Sexp.List (Sexp.Atom "or" :: args) ->
      Formula.disjunctions (List.map sexp_of_formula args)
  | Sexp.List (Sexp.Atom "not" :: args) ->
      if List.length args = 1 then
        Formula.not (sexp_of_formula (List.hd args))
      else
        Conv.of_sexp_error "negation accepts only one argument" sexp
  | Sexp.List (Sexp.Atom "=>" :: args) ->
      if List.length args > 1 then
        let args = List.rev_map sexp_of_formula args in
        List.fold_left
          (fun c p -> Formula.implication p c) (List.hd args) (List.tl args)
      else
        Conv.of_sexp_error "implication requires at least two argument" sexp
  | Sexp.List (Sexp.Atom op :: args) ->
      let bin op = function
        | [x; y] ->
            let x = sexp_of_poly x in
            let y = sexp_of_poly y in
            op x y
        | _ -> Conv.of_sexp_error "binary relation expects exact two polynomials" sexp
      in
      let op = match op with
        | "=" -> Formula.eq
        | "!=" -> Formula.neq
        | ">=" -> Formula.ge
        | "<=" -> Formula.le
        | ">" -> Formula.gt
        | "<" -> Formula.lt
        | _ -> Conv.of_sexp_error "logical formula is expected" sexp
      in
      bin op args
  | _ -> Conv.of_sexp_error "invalid formula" sexp

type t =
  | Simplify of Formula.t

let sexp_of_t sexp =
  match sexp with
  | Sexp.List (Sexp.Atom command :: args) ->
      begin match command with
      | "simplify" ->
          if List.length args = 1 then
            Simplify (sexp_of_formula (List.hd args))
          else
            Conv.of_sexp_error "simplify accepts only one argument" sexp
      | _ -> Conv.of_sexp_error "unknown command" sexp
      end
  | _ -> Conv.of_sexp_error "invalid formula" sexp

let load filename =
  Sexp.load_sexps_conv_exn filename sexp_of_t
