open OUnit2

let () = run_test_tt_main begin "Polysat" >::: [
    TestUtil.tests;
    TestMonomial.tests;
    TestRing.tests;
    TestPolynomial.tests;
  ]
  end
