open Ring
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
    let print out n = Printf.fprintf out "%d" n
  end)

module Op = Operator(M)

open M

(* a simple test is a triple (test label, expected value, test code) *)
let simple_tests = [
  "zero is additive left identity", 2, Op.(zero + 2);
  "zero is additive right identity", 1, Op.(1 + zero);
  "one is multiplicative left identity", 2, Op.(one * 2);
  "one is multiplicative right identity", 4, Op.(4 * one);
  "neg neg", 3, Op.(-(-3));
  "neg zero", zero, Op.(-zero);
  "neg is inverse", zero, Op.(2 + -2);
  "sub x x is zero", zero, Op.(1 - 1);
  "sub add", 2, Op.(2 + 2 - 2);
  "0 power is one", one, Op.(2 ** 0);
  "power2", Op.(-5 * -5), Op.(-5 ** 2);
] |> List.map (fun (label, expect, result) ->
    label >:: (fun ctxt ->
        assert_equal ~ctxt ~cmp:equal ~printer:(fun r -> Format.asprintf "%a" pp r)
          expect result))

let tests = "Ring" >::: [
    test_list simple_tests;
  ]
