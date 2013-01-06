open OUnit
open BatSubstring
open BatPervasives

let test_dropr =
  let aeq = assert_equal ~printer:identity in
  [
    begin "dropr empty" >:: fun () ->
      aeq "" (to_string (dropr (const true) (of_string "")));
      aeq "" (to_string (dropr (const false) (of_string "")))
    end;
    begin "dropr none" >:: fun () ->
      aeq "foo" (to_string (dropr (const false) (of_string "foo")))
    end;
    begin "dropr all" >:: fun () ->
      aeq "" (to_string (dropr (const true) (of_string "foo")))
    end;
    begin "dropr some" >:: fun () ->
      aeq "f" (to_string (dropr ((=) 'o') (of_string "foo")))
    end;
  ];;

let test_dropl =
  let aeq = assert_equal ~printer:identity in
  [
    begin "dropl empty" >:: fun () ->
      aeq "" (to_string (dropl (const true) (of_string "")));
      aeq "" (to_string (dropl (const false) (of_string "")))
    end;
    begin "dropl none" >:: fun () ->
      aeq "foo" (to_string (dropl (const false) (of_string "foo")))
    end;
    begin "dropl all" >:: fun () ->
      aeq "" (to_string (dropl (const true) (of_string "foo")))
    end;
    begin "dropl some" >:: fun () ->
      aeq "oo" (to_string (dropl ((=) 'f') (of_string "foo")))
    end;
  ];;

let test_taker =
  let aeq = assert_equal ~printer:identity in
  [
    begin "taker empty" >:: fun () ->
      aeq "" (to_string (taker (const true) (of_string "")));
      aeq "" (to_string (taker (const false) (of_string "")))
    end;
    begin "taker none" >:: fun () ->
      aeq "" (to_string (taker (const false) (of_string "foo")))
    end;
    begin "taker all" >:: fun () ->
      aeq "foo" (to_string (taker (const true) (of_string "foo")))
    end;
    begin "taker some" >:: fun () ->
      aeq "oo" (to_string (taker ((=) 'o') (of_string "foo")))
    end;
  ];;

let test_takel =
  let aeq = assert_equal ~printer:identity in
  [
    begin "takel empty" >:: fun () ->
      aeq "" (to_string (takel (const true) (of_string "")));
      aeq "" (to_string (takel (const false) (of_string "")))
    end;
    begin "takel none" >:: fun () ->
      aeq "" (to_string (takel (const false) (of_string "foo")))
    end;
    begin "takel all" >:: fun () ->
      aeq "foo" (to_string (takel (const true) (of_string "foo")))
    end;
    begin "takel some" >:: fun () ->
      aeq "f" (to_string (takel ((=) 'f') (of_string "foo")))
    end;
  ];;

let to_strings (x,y) = to_string x, to_string y

let test_splitr =
  let printer (s1,s2) = Printf.sprintf "(%S,%S)" s1 s2 in
  let aeq = assert_equal ~printer in
  [
    begin "splitr empty" >:: fun () ->
      aeq ("","") (to_strings (splitr (const true) (of_string "")));
      aeq ("","") (to_strings (splitr (const false) (of_string "")))
    end;
    begin "splitr none" >:: fun () ->
      aeq ("foo","")
        (to_strings (splitr (const false) (of_string "foo")))
    end;
    begin "splitr all" >:: fun () ->
      aeq ("","foo") (to_strings (splitr (const true) (of_string "foo")))
    end;
    begin "splitr some" >:: fun () ->
      aeq ("f","oo") (to_strings (splitr ((=) 'o') (of_string "foo")))
    end;
  ];;

let test_splitl =
  let printer (s1,s2) = Printf.sprintf "(%S,%S)" s1 s2 in
  let aeq = assert_equal ~printer in
  [
    begin "splitl empty" >:: fun () ->
      aeq ("","") (to_strings (splitl (const true) (of_string "")));
      aeq ("","") (to_strings (splitl (const false) (of_string "")))
    end;
    begin "splitl none" >:: fun () ->
      aeq ("","foo")
        (to_strings (splitl (const false) (of_string "foo")))
    end;
    begin "splitl all" >:: fun () ->
      aeq ("foo","") (to_strings (splitl (const true) (of_string "foo")))
    end;
    begin "splitl some" >:: fun () ->
      aeq ("f","oo") (to_strings (splitl ((=) 'f') (of_string "foo")))
    end;
  ];;

let test_slice =
  let printer sus =
    let (s,i,n) = base sus in Printf.sprintf "(%S,%d,%d)" s i n
  in
  let cmp sus1 sus2 = to_string sus1 = to_string sus2 in
  let aeq = assert_equal ~printer ~cmp in
  [
    begin "slice empty" >:: fun () ->
      aeq (empty ()) (slice (empty ()) 0 None)
    end;
    begin "slice all" >:: fun () ->
      aeq (of_string "foo") (slice (of_string "foo") 0 None);
      aeq (of_string "foo") (slice (of_string "foo") 0 (Some 3));
    end;
    begin "slice none" >:: fun () ->
      aeq (of_string "") (slice (of_string "foo") 3 None);
      aeq (of_string "") (slice (of_string "foo") 3 (Some 0));
    end;
    begin "slice some" >:: fun () ->
      aeq (of_string "oo") (slice (of_string "foo") 1 None);
      aeq (of_string "oo") (slice (of_string "foo") 1 (Some 2));
    end;
    begin "slice pick" >:: fun () ->
      aeq (of_string "i") (slice (of_string "jim") 1 (Some 1));
    end;
  ];;

let test_index_from =
  let aeq = assert_equal ~printer:string_of_int in
  [
    begin "index from" >:: fun () ->
      aeq (index_from (of_string "foobar") 2 'b')
	(2+index (triml 2 (of_string "foobar")) 'b')
    end;
  ];;

let test_rindex_from =
  let aeq = assert_equal ~printer:string_of_int in
  [
    begin "rindex from" >:: fun () ->
      aeq (rindex_from (of_string "foobar") 2 'b')
	(rindex (trimr 2 (of_string "foobar")) 'b')
    end;
  ];;

let test_is_prefix = 
  let aeq = assert_equal ~printer:string_of_bool in
  [
    begin "is_prefix" >:: fun () ->
      aeq (is_prefix "foo" (of_string "foobar")) true;
      aeq (is_prefix "foj" (of_string "foobar")) false;
      aeq (is_prefix "foobarz" (of_string "foobar")) false;
      aeq (is_prefix "foobar" (of_string "foobar")) true;
    end;
  ];;

let test_enum =
  let ss = of_string "testing" in
  let test_enum substring = ss |> to_string |> BatString.enum in
  [
    begin "enum" >:: fun () ->
      assert_equal (ss |> enum |> BatString.of_enum) "testing";
      assert_equal (size ss) (ss |> enum |> BatEnum.count) ~printer:string_of_int;
      assert_equal (ss |> enum |> BatString.of_enum)
                   (ss |> test_enum |> BatString.of_enum)
    end
  ]

let test_iteri = 
  let ss = of_string "test" in
  let mark = ref false in
  let r = ref [] in
  ss |> iteri (fun i _ -> mark := true; r := i::(!r) );
  [
    begin "iteri" >:: fun () ->
      assert_equal !mark true ~printer:string_of_bool;
      assert_equal (List.rev !r) [0;1;2;3]
    end
  ]

let tests = "Substring" >::: [
  "dropr" >::: test_dropr;
  "dropl" >::: test_dropl;
  "taker" >::: test_taker;
  "takel" >::: test_takel;
  "splitr" >::: test_splitr;
  "splitl" >::: test_splitl;
  "slice" >::: test_slice;
  "index_from" >::: test_index_from;
  "is_prefix" >::: test_is_prefix;
  "enum" >::: test_enum;
  "test_iteri" >::: test_iteri;
];;
