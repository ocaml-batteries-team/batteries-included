open OUnit
open BatString

let string = "Jon \"Maddog\" Orwant"

open BatEnum
(*
let test_take_and_skip () =
  let foo s : string list =
    let e = enum s in
      [? List : of_enum (f e) |
         f <- List : [take 5; skip 3 %> take 5; take 5 ; identity] ?]
  in
    assert_equal ~printer:(Printf.sprintf2 "%a" (List.print String.print_quoted))
      ["Jon \""; "dog\" "; "Orwan"; "t"]
      (foo string)
*)

let test_starts_with () =
  let check expected prefix =
    let s = match expected with true -> "" | false -> "not " in
      if starts_with string prefix <> expected then
        assert_failure (Printf.sprintf "String %S should %sstart with %S"
                          string s prefix)
  in
    check true "Jon";
    check false "Jon \"Maddog\" Orwants";
    check false "Orwants"

let test_ends_with () =
  let check expected suffix =
    let s = match expected with true -> "" | false -> "not " in
      if ends_with string suffix <> expected then
        assert_failure (Printf.sprintf "String %S should %send with %S"
                          string s suffix)
  in
    check true "want";
    check false "I'm Jon \"Maddog\" Orwant";
    check false "Jon"

let assert_int = assert_equal ~printer:string_of_int
let big_pattern, big_string =
  BatRandom.init 0;
  let pattern = BatString.init 300 (fun _ -> BatRandom.char ()) in
  let string = BatString.init 600 (fun _ -> BatRandom.char ()) in
  pattern, string ^ pattern ^ string

let test_find (find :string -> ?stop:int -> string -> int -> int) () =
  let str = "abcd" in
  assert_int 0 (find "a" str 0);
  assert_int 1 (find "b" str 0);
  assert_int 2 (find "c" str 2);
  assert_int 3 (find "d" str 1);
  assert_int 0 (find "" str 0);
  assert_int 2 (find "" str 2);
  assert_int 4 (find "" str 4);
  assert_raises Not_found (fun () -> find "d" ~stop:3 str 2);
  assert_raises Not_found (fun () -> find "b" str 2);
  assert_raises Not_found (fun () -> find "d" str 4);
  assert_raises (Invalid_argument "String.find*")
    (fun () -> find "" str ~-1);
  assert_raises (Invalid_argument "String.find*")
    (fun () -> find "" str 5);
  let str = "ababababa" in
  assert_int 0 (find "aba" str 0);
  assert_int 6 (find "aba" str 5);
  assert_int 0 (find str str 0);
  assert_raises Not_found (fun () -> find str str 1);
  assert_int 600 (find big_pattern big_string 0);
  ()

let test_rfind (find :string -> ?stop:int -> string -> int -> int) () =
  let str = "abcd" in
  assert_int 0 (find "a" str 3);
  assert_int 1 (find "b" str 2);
  assert_int 2 (find "c" str 4);
  assert_int 3 (find "d" str 4);
  assert_int 0 (find "" str 0);
  assert_int 2 (find "" str 2);
  assert_int 4 (find "" str 4);
  assert_raises Not_found (fun () -> find "a" ~stop:1 str 4);
  assert_raises Not_found (fun () -> find "c" str 2);
  assert_raises Not_found (fun () -> find "d" str 3);
  assert_raises (Invalid_argument "String.rfind*")
    (fun () -> find "" str ~-1);
  assert_raises (Invalid_argument "String.rfind*")
    (fun () -> find "" str 5);
  let str = "ababababa" in
  assert_int 6 (find "aba" str 9);
  assert_int 4 (find "aba" str 7);
  assert_int 2 (find "aba" str 6);
  assert_int 0 (find str str 9);
  assert_raises Not_found (fun () -> find str str 8);
  assert_int 600 (find big_pattern big_string 1500);
  ()

let test_find_all () =
  assert_equal
    (BatList.of_enum (BatString.find_all "aba" "ababababa"))
    [0;2;4;6]

let test_rfind_all () =
  assert_equal
    (BatList.of_enum (BatString.rfind_all "aba" "ababababa"))
    [6;4;2;0]

let test_nsplit () =
  let printer = BatPrintf.sprintf2 "%a" (BatList.print BatString.print) in
  let check exp s sep = assert_equal ~printer exp (nsplit s sep) in
    check ["a"; "b"; "c"] "a/b/c" "/";
    check [""; "a"; "b"; "c"; ""; ""] "/a/b/c//" "/";
    check [""; "a"; "b"; "c"; ""; ""] "FOOaFOObFOOcFOOFOO" "FOO"

let assert_no_raises : ?msg:string -> (unit -> 'a) -> 'a =
  fun ?(msg="Function raised an exception when none was expected.") f ->
    try
      f ()
    with exn ->
      assert_failure (msg ^ " " ^ Printexc.to_string exn)

let test_exists () =
  let check haystack needle expected =
    let msg =
      Printf.sprintf "exists \"%s\" \"%s\" = %b"
	(String.escaped haystack) (String.escaped needle)
	expected
    in
      assert_equal
	~msg
	(assert_no_raises ~msg:(msg ^ " raised exception ")
	   (fun () -> BatString.exists haystack needle))
	expected
  in
    check "" "" true;
    check "a" "" true;
    check "" "a" false;
    check "ab" "a" true;
    check "ab" "b" true;
    check "ab" "c" false

let tests = "String" >::: [
  (*  "Taking and skipping" >:: test_take_and_skip; *)
  "Start with" >:: test_starts_with;
  "Ends with" >:: test_ends_with;
  "Finding" >::: [
    "find_from" >:: test_find
      (fun pattern ?stop text start ->
        let stop = match stop with None -> BatString.length text |Some x -> x in
        let i = BatString.find_from text start pattern in
        if i > stop - String.length pattern then raise Not_found else i);
    "find_simple" >:: test_find BatString.Find.find_simple;
    "find_horspool" >:: test_find BatString.Find.find_horspool;
    "find_adaptive" >:: test_find BatString.Find.find_adaptive;
    "find_all" >:: test_find_all;
    "rfind_from" >:: test_rfind
      (fun pattern ?(stop=0) text start ->
        let i = BatString.rfind_from text (start-1) pattern in
        if i < stop then raise Not_found else i);
    "rfind_simple" >:: test_rfind BatString.Find.rfind_simple;
    "rfind_horspool" >:: test_rfind BatString.Find.rfind_horspool;
    "rfind_adaptive" >:: test_rfind BatString.Find.rfind_adaptive;
    "rfind_all" >:: test_rfind_all;
  ];
  "Splitting with nsplit" >:: test_nsplit;
  "Exists" >:: test_exists;
]
