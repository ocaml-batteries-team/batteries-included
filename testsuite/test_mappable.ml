open OUnit

(* The purpose of this test file is to test properties that should be
   verified by all instances of a given interface, here
   BatInterfaces.Mappable.

   It is very minimal for now : it only check for one property, and
   only a few of the Mappable modules (it is actually a regression
   test for a very specific bug). New properties will be added, and
   hopefully they will be verified against all Mappable modules.
*)

module TestMappable
  (M : sig
    include BatEnum.Enumerable

    include BatInterfaces.Mappable
    with type 'a mappable = 'a enumerable
  end)
  =
struct
  (* The property we test is that the order in which the [map]
     function traverse the structure (applying a given function on
     each element) is the same as the order of the [enum] function of
     the module (the order in which the elements are produced in the
     enumeration).
  *)
  let test_map_evaluation_order printer t =
    let elems_in_enum_order = BatList.of_enum (M.enum t) in
    let elems_in_map_order =
      let li = ref [] in
      ignore (M.map (fun x -> li := x :: !li) t);
      List.rev !li in
    assert_equal ~printer:(BatIO.to_string (BatList.print printer))
      elems_in_enum_order
      elems_in_map_order
end

let test_list_mappable () =
  let module T = TestMappable(BatList) in
  T.test_map_evaluation_order BatInt.print [1; 2; 3]

let test_array_mappable () =
  let module T = TestMappable(BatArray) in
  T.test_map_evaluation_order BatInt.print [|1; 2; 3|]
(*
let test_pair_mappable () =
  let module T = TestMappable(BatTuple.Tuple2) in
  T.test_map_evaluation_order BatInt.print (1, 2)
 *)

let tests = "Mappable" >::: [
  "Array" >:: test_array_mappable;
  "List" >:: test_list_mappable;
(*  "Pair" >:: test_pair_mappable;*)
]
