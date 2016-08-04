open OUnit2

module M = Monomial.Make(struct
    type t = string
    let compare = compare
    let pp = Format.pp_print_string
    let print out s = Printf.fprintf out "%s" s
  end)

open M

let tests = "monomial.ml" >::: [
    "degree of one is zero" >:: (fun ctxt ->
        assert_equal ~ctxt (degree one) 0
      );
    "no variables in one" >:: (fun ctxt ->
        assert_equal ~ctxt ~cmp:VarSet.equal (vars one) VarSet.empty
      );
    "non-positive degree is disallowed" >:: (fun ctx ->
        assert_raises (Invalid_argument "Monomial.var") (fun () -> var "x" 0)
      );
    "one is left identity" >:: (fun ctxt ->
        let m = var "x" 5 in
        assert_equal ~ctxt (mult one m) m
      );
    "one is right identity" >:: (fun ctxt ->
        let m = var "x" 9 in
        assert_equal ~ctxt (mult m one) m
      );
    "mult1" >:: (fun ctxt ->
        let m = var "x" 2 in
        assert_equal ~ctxt (var_degree "x" (mult m m)) 4
      );
    "mult2" >:: (fun ctxt ->
        let m1 = var "x" 2 in
        let m2 = var "y" 3 in
        assert_equal ~ctxt ~cmp:VarSet.equal
          (vars (mult m1 m2)) (VarSet.of_list ["x"; "y"])
      );
    "compare1" >:: (fun ctxt ->
        assert_equal ~ctxt (compare one one) 0
      );
    "compare2" >:: (fun ctxt ->
        let m = var "x" 2 in
        assert_equal ~ctxt (compare (mult m m) (var "x" 4)) 0
      );
  ]
