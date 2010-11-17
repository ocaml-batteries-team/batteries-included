open OUnit
open BatRandom
open BatPervasives

let print_enum out enum =
  BatEnum.print (fun out (c, _) -> BatPrintf.fprintf out "%d" c) out enum

let test_traversal_order () =
  let init = State.make [|0|] in
  let keys = BatEnum.take 50 (State.enum_int init 10) in
  let map  = BatPMap.of_enum (BatEnum.map (fun x -> (x,x)) keys) in
  let enum_1 () = BatPMap.enum map
  and enum_2 () =
    let list = BatRefList.empty () in
      BatPMap.iter (fun k v -> BatRefList.push list (k, v)) map;
      BatRefList.backwards list
  in
    match BatEnum.compare compare (enum_1 ()) (enum_2 ()) with
      | 0 -> (* pass *) ()
      | _ ->
          assert_failure
            (BatPrintf.sprintf2 "Expected %a, got %a"
               print_enum (enum_1 ()) print_enum (enum_2 ()))

let tests = "PMap" >::: [
  "traversal order iter vs. enum" >:: test_traversal_order;
]
