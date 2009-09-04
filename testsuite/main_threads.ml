open OUnit

let all_tests =
  [
    Test_threadpool.tests;
  ]

let _ = OUnit.run_test_tt_main ("All" >::: all_tests)
