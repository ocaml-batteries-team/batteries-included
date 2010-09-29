open OUnit

let all_tests =
  [
    Test_base64.tests;
    Test_unix.tests;
(*    Test_print.tests;
    Test_toplevel.tests; *)
    Test_pmap.tests;
    Test_vect.tests;
    Test_file.tests;
    Test_string.tests;
    Test_substring.tests;
    Test_digest.tests;
    Test_enum.tests;
    Test_set.tests;
    Test_dynarray.tests;
    Test_stack.tests;
  ]

let () = 
  print_endline "Running Batteries tests";
  ignore(OUnit.run_test_tt_main ("All" >::: all_tests));

