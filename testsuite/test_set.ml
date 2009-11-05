open Batteries
open OUnit

module IS = Set.IntSet

let of_list l = List.fold_left (fun a i -> IS.add i a) IS.empty l

let s1 = of_list [1;2;3]
let s2 = of_list [1;2]

let test_subset_compare () =
  assert_equal (IS.compare_subset s1 s2) (-1);
  assert_equal (IS.compare_subset s2 s1) 1

let tests = "Set" >::: [
  "Subset_compare" >:: test_subset_compare;
]
