open OUnit
open BatRandom
open BatPervasives

let print_enum out enum =
  BatEnum.print (fun out (c, _) -> BatPrintf.fprintf out "%d" c) out enum

let assert_equal_enums enum_1 enum_2 =
  match BatEnum.compare compare (enum_1 ()) (enum_2 ()) with
    | 0 -> (* pass *) ()
    | _ ->
        assert_failure
          (BatPrintf.sprintf2 "Expected %a, got %a"
             print_enum (enum_1 ()) print_enum (enum_2 ()))

let assert_equal_maps map_1 map_2 =
  let enum_1 () = BatPMap.enum map_1 in
  let enum_2 () = BatPMap.enum map_2 in
  assert_equal_enums enum_1 enum_2

let gen_map state bound count =
  let keys = BatEnum.take count (State.enum_int state bound) in
  BatPMap.of_enum (BatEnum.map (fun x -> (x,x)) keys)

let test_traversal_order () =
  let init = State.make [|0|] in
  let map = gen_map init 10 50 in
  let enum_1 () = BatPMap.enum map
  and enum_2 () =
    let list = BatRefList.empty () in
      BatPMap.iter (fun k v -> BatRefList.push list (k, v)) map;
      BatRefList.backwards list
  in
  assert_equal_enums enum_1 enum_2

let test_split () =
  let do_test map v =
    let m1, vo, m2 = BatPMap.split v map in
    assert_equal_maps m1 (BatPMap.filteri (fun k _ -> k < v) map);
    assert_equal_maps m2 (BatPMap.filteri (fun k _ -> k > v) map);
    assert_equal vo (if BatPMap.mem v map then Some v else None)
  in
  let init = State.make [|0|] in
  for i = 0 to 50 do
    let bound = 40 in
    let count = i * 5 in
    do_test (gen_map init bound count) (State.int init bound)
  done

let test_multimap_empty_assoc_lists () =
  let module M = BatMultiPMap in
  let map =
    M.add 0 "foo" M.empty |> M.add 0 "bar" |> M.add 0 "sna" |>
    M.remove 0 "foo" |> M.remove 0 "bar" |> M.remove 0 "sna"
  in
    if M.mem 0 map then
      assert_failure
        (Printf.sprintf "map[0] should be empty but contains %d bindings\n"
           (BatPSet.cardinal (M.find 0 map)))

let tests = "PMap" >::: [
  "traversal order iter vs. enum" >:: test_traversal_order;
  "split" >:: test_split;
  "MultiPMap: removing empty association lists" >:: test_multimap_empty_assoc_lists;
]
