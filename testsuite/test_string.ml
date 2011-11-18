open OUnit
open BatString

let string = "Jon \"Maddog\" Orwant"

open BatEnum
(*
let test_take_and_skip () =
  let foo s : string list =
    let e = enum s in
      [? List : of_enum (f e) |
         f <- List : [take 5; skip 3 |- take 5; take 5 ; identity] ?]
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
  "Splitting with nsplit" >:: test_nsplit;
  "Exists" >:: test_exists;
]
