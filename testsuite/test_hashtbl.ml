open Batteries
open OUnit

(* regression tests for
   https://github.com/ocaml-batteries-team/batteries-included/issues/609 *)

module IntIdHash = struct
    type t = int
    let hash t = t
    let equal = (=)
end

let test_issue_609_1 () =
  let module H = BatHashtbl.Make(IntIdHash) in
  let h = H.create 7 in
  H.replace h min_int [];
  let v = H.find_default h (-max_int) [] in
  assert_equal v []

let test_issue_609_2 () =
  let module H = BatHashtbl.Make(IntIdHash) in
  let h = H.create 7 in
  H.add h 0 [];
  H.remove_all h 0;
  assert_bool "0 was removed" (not (H.mem h 0))

let test_issue_1038 () =
  let module M = struct
    type t = int * string
    let equal ((i, _) : t) ((i', _) : t) = Int.equal i i'
    let hash ((_, s) : t) = Hashtbl.hash s
  end in
  let module H = BatHashtbl.Make(M) in
  let elem = (1, "Hello") in
  let h = H.of_list [(elem, ())] in
  assert_bool "the element is found" (H.mem h elem)

let tests = "Hashtbl" >::: [
  "PR#609 (1)" >:: test_issue_609_1;
  "PR#609 (2)" >:: test_issue_609_2;
  "PR#1038" >:: test_issue_1038;
]
