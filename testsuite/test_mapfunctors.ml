open OUnit
open BatRandom
open BatPervasives

module MkTest (MkMap : functor (Ord : BatInterfaces.OrderedType)
                 -> BatMap.S with type key = Ord.t) =
struct
  (* This is basically Test_pmap, but specialized for MkMap(Int) *)
  module Map = MkMap (BatInt)

  let print_enum out enum =
    BatEnum.print begin
      fun out (c, _) ->
        BatPrintf.fprintf out "%d" c
    end out enum

  let assert_equal_enums enum_1 enum_2 =
    match BatEnum.compare compare (enum_1 ()) (enum_2 ()) with
      | 0 -> (* pass *) ()
      | _ ->
          assert_failure
            (BatPrintf.sprintf2 "Expected %a, got %a"
               print_enum (enum_1 ()) print_enum (enum_2 ()))

  let assert_equal_maps map_1 map_2 =
    let enum_1 () = Map.enum map_1 in
    let enum_2 () = Map.enum map_2 in
    assert_equal_enums enum_1 enum_2

  let gen_map state bound count =
    let keys = BatEnum.take count (State.enum_int state bound) in
    Map.of_enum (BatEnum.map (fun x -> (x, x)) keys)

  let test_traversal_order () =
    let init = State.make [|0|] in
    let map = gen_map init 10 50 in
    let enum_1 () = Map.enum map
    and enum_2 () =
      let list = BatRefList.empty () in
      Map.iter (fun k v -> BatRefList.push list (k, v)) map;
      BatRefList.backwards list
    in
    assert_equal_enums enum_1 enum_2

  let tests = [
    "traversal order iter vs. enum" >:: test_traversal_order ;
  ]
end

let tests =
  let module MT1 = MkTest (BatMap.Make) in
  let mt1_tests = "Map.Make" >::: MT1.tests in
  let module MT2 = MkTest (BatSplay.Map) in
  let mt2_tests = "Splay.Make" >::: MT2.tests in
  "Generic Map tests" >::: [
    mt1_tests ;
    mt2_tests ;
  ]
