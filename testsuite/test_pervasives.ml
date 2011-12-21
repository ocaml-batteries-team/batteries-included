open OUnit
open Batteries

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

type test1 =
  | A of int
  | B of float * float
  | C of string * test1

type test2 = {
  a : int;
  b : float * float;
  c : string * test2 option;
}

type test3 = {
  f1 : float;
  f2 : float;
  f3 : float;
}

let test_dump () =
  let test str value =
    assert_equal ~msg:str ~printer:(fun x -> x) str (BatPervasives.dump value) in

  (* integers *)
  test "0" None;
  test "0" false;
  test "1" true;
  test "17" 17;

  (* lists *)
  (* despite the specialized list-spotting routine, [] is printed as
     0 as they have the same representation *)
  test "0" [];
  test "[1; 2]" [1; 2];

  (* algebraic datatypes *)
  test "(1)" (A 1);
  test "Tag1 (2., 3.)" (B (2.,3.));
  test "Tag2 (\"foo\", (1))" (C ("foo", A 1));

  test "(1, (2., 3.), [\"foo\"])" {a = 1; b = (2., 3.); c = "foo", None};

  (* lazy *)
  (* lazy immediate values are not lazyfied!
     test "0" (lazy 0); *)
  test "<lazy>" (lazy (ignore ()));

  (* closures *)
  test "<closure>" (fun x -> x);

  (* objects *)
  let obj = object
    val x = 2
    val z = 3.
    method foo = "bar" end in
  test (Printf.sprintf "Object #%d (2, 3.)" (Oo.id obj)) obj;

  (* infix, forward? *)

  (* string *)
  let str = "foo \"bar\"\n" in
  test (Printf.sprintf "%S" str) str;

  (* double *)
  let test_float x = test (string_of_float x) x in
  List.iter test_float
    [0.; 1.; -2.; max_float; min_float; epsilon_float; nan];
  for i = 0 to 1000 do
    test_float (Random.float max_float);
    test_float (Random.float min_float);
  done;

  (* abstract? *)

  (* custom? *)

  (* final? *)


  (* double array or struct *)
  let test_arr arr v =
    test (BatIO.to_string (BatArray.print BatFloat.print) arr) v in
  test "()" ([| |] : float array);
  test_arr [| 0.; 1.; 2. |] [| 0.; 1.; 2. |];
  test_arr [| 0.; 1.; 2. |] { f1 = 0.; f2 = 1.; f3 = 2. };

  ()

let tests = "Std" >::: [
  "using" >:: test_using;
  "dump" >:: test_dump;
];;
