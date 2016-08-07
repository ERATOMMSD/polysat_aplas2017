open Formula
open OUnit2

let simple_tests = [
  "tru && tru = tru", tru, Op.(tru && tru);
  "tru && fls = fls", fls, Op.(tru && fls);
  "fls && tru = fls", fls, Op.(fls && tru);
  "fls && fls = fls", fls, Op.(fls && fls);
  "tru || tru = tru", tru, Op.(tru || tru);
  "tru || fls = tru", tru, Op.(tru || fls);
  "fls || tru = tru", tru, Op.(fls || tru);
  "fls || fls = fls", fls, Op.(fls || fls);
  "not tru = fls", fls, Op.(not tru);
  "not fls = tru", tru, Op.(not fls);
] |> List.map (fun (label, expect, result) ->
    label >:: (fun ctxt ->
        assert_equal ~ctxt ~printer:(fun f -> Format.asprintf "%a" pp f)
          expect result))

let tests = "Formula" >::: [
    test_list simple_tests;
  ]
