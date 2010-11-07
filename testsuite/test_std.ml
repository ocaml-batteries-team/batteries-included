open OUnit
open Batteries_uni

let test_using () =
  let obj = (ref 0), (ref 0) in
  let dispose (_,closed) = closed := 5 in
  let f (run,_) = run := 7; 42 in
  let r = with_dispose ~dispose f obj in
  let printer = string_of_int in
  let run, closed = obj in
  assert_equal ~printer 42 r;
  assert_equal ~printer 7 (!run);
  assert_equal ~printer 5 (!closed)

let tests = "Std" >::: [
  "using" >:: test_using
];;
