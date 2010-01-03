open Batteries
open OUnit

module IS = Set.IntSet

let of_list l = List.fold_left (fun a i -> IS.add i a) IS.empty l

let s1 = of_list [1;2;3]
let s2 = of_list [1;2]

let asseq_int = assert_equal ~printer:string_of_int

let test_subset_compare () =
  asseq_int 1 (IS.compare_subset s1 s2);
  asseq_int (-1) (IS.compare_subset s2 s1)

let tests = "Set" >::: [
  "Subset_compare" >:: test_subset_compare;
]
