open Polynomial
open OUnit2

module M = Make(struct
    type t = int
    let zero = 0
    let one = 1
    let neg = ( ~- )
    let add = ( + )
    let mult = ( * )
    let compare = compare
    let pp = Format.pp_print_int
  end)(struct
    type t = string
    let compare = compare
    let pp = Format.pp_print_string
  end)

open M

(* a simple test is a triple (test label, expected value, test code) *)
let simple_tests = [
  "concerning zero", Op.(zero + one * zero), zero;
  "const addition", Op.(!:1 + !:2), Op.(!:3);
  "const multiplication", Op.(!:3 * !:4), Op.(!:12);
  "misc1", Op.((!:1 + ?:"x") ** 2), Op.(!:1 + !:2 * ?:"x" + ?:"x" ** 2);
  "misc2", Op.((!:1 + ?:"x") * (!:2 + ?:"y")), Op.(!:2 + !:2 * ?:"x" + ?:"y" + ?:"x" * ?:"y");
] |> List.map (fun (label, expect, result) ->
    label >:: (fun ctxt ->
        assert_equal ~ctxt ~cmp:equal ~printer:(fun p -> Format.asprintf "%a" pp p)
          expect result))

let tests = "polynomial.ml" >::: [
    test_list simple_tests;
    "constance1" >:: (fun ctxt ->
        assert_equal 4 (to_const (const 4)));
    "constance2" >:: (fun ctxt ->
        assert_equal 0 (to_const (const 0)));
  ]
