open Batteries
open OUnit

module DA = DynArray

let s1 = DA.of_list [1;2;3]
let s2 = DA.of_list [1;2]

let asseq_int = assert_equal ~printer:(DA.print Int.print |> IO.to_string)
let asseq_str = assert_equal ~printer:identity

let test_dynarray_filter () =
  let e = BatDynArray.create () in
  BatDynArray.add e "a";
  BatDynArray.add e "b";
  BatDynArray.keep ((=) "a") e;
  asseq_str (BatDynArray.get e 0) "a"


let tests = "Set" >::: [
  "Dynarray_filter" >:: test_dynarray_filter;
]
