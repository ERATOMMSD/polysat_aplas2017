open Util
open OUnit2
open Format

let list_equal_as_set ?(cmp=compare) l1 l2 =
  (List.sort cmp l1) = (List.sort cmp l2)

let pp_list pp fmt l =
  fprintf fmt "[%a]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,") pp)
    l

let simple_list_tests = [
  "tupling1", [[]], List.tupling [];
  "tupling2", [[1];[2];[3]], List.tupling [[1;2;3]];
  "tupling3", [[1;4];[1;5];[1;6];[2;4];[2;5];[2;6];[3;4];[3;5];[3;6]], List.tupling [[1;2;3];[4;5;6]];
] |> List.map (fun (label, expect, result) ->
    label >:: (fun ctxt ->
        assert_equal ~ctxt ~cmp:list_equal_as_set
          ~printer:(fun ll -> asprintf "%a" (pp_list (pp_list pp_print_int)) ll)
          expect result))

let tests = "util.ml" >::: [
    "List module" >::: [
      test_list simple_list_tests;
    ];
  ]
