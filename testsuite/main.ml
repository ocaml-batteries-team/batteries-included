module X = Test_interface

open OUnit

let all_tests =
  [
    Test_pervasives.tests;
(*    Test_base64.tests; Replaced by simple quickcheck rules inline *)
 (*   Test_unix.tests; Moved to inline tests in BatUnix *)
(*    Test_print.tests;
    Test_toplevel.tests; *)
    Test_map.tests;
    (* pmap is actually tested in test_map.ml, as they share their
       implementation *)
    Test_multipmap.tests;
(*     Test_vect.tests; Moved inline to BatVect *)
    Test_file.tests;
(*    Test_string.tests; Moved inline to BatString *)
    Test_substring.tests;
    Test_digest.tests;
    Test_enum.tests;
    Test_set.tests;
    Test_dynarray.tests;
    Test_stack.tests;
    Test_mappable.tests;
    Test_hashcons.tests;
    Test_mapfunctors.tests;
    Test_optparse.tests;
    Test_uref.tests;
    Test_bitset.tests;
    Test_container.tests;
    Test_random.tests;
    Test_bounded.tests;
  ]

let () =
  ignore(OUnit.run_test_tt_main ("All" >::: all_tests));
