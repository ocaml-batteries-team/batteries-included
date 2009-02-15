open Testing

let tests =
  [ Test_string.test_1;
    Test_string.test_2;
    Test_string.test_3;
    Test_string.test_4;
    Test_string.test_5;
    Test_string.test_6;
    Test_string.test_7;
    (Test_digest.name, Test_digest.test);
    Test_enum.test_array;
    Test_enum.test_array2;
    Test_enum.test_list;
    Test_enum.test_list2;
    Test_enum.test_string;
    Test_enum.test_string2;
    Test_enum.test_bigarray;
    Test_enum.test_bigarray2;
    Test_enum.test_bigarray3;
    Test_enum.test_rope;
    Test_enum.test_rope2;
    Test_enum.test_UTF8;
    Test_enum.test_UTF82;
];;

init ();;

List.iter 
    (fun (name, go) -> 
       try  result name (go ())
       with e -> result name (Err (Printf.sprintf "Cannot run test:%s\n" (Printexc.to_string e))))
tests;;
    

finish ()
