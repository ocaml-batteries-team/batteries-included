open OUnit

let all_tests =
  [
    Test_std.tests;
    Test_base64.tests;
    Test_unix.tests;
(*    Test_print.tests;
    Test_toplevel.tests; *)
    Test_map.tests;
    (* pmap is actually tested in test_map.ml, as they share their
       implementation *)
    Test_multipmap.tests;
    Test_vect.tests;
    Test_file.tests;
    Test_string.tests;
    Test_substring.tests;
    Test_digest.tests;
    Test_enum.tests;
    Test_set.tests;
    Test_dynarray.tests;
    Test_stack.tests;
    Test_mappable.tests;
    Test_hashcons.tests;
    Test_mapfunctors.tests;
    Test_lazy_pattern.tests;
  ]

let () = 
  ignore(OUnit.run_test_tt_main ("All" >::: all_tests));

