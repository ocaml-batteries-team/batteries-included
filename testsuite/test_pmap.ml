open OUnit
open Random

let print_enum out enum =
  Enum.print (fun out (c, _) -> Printf.fprintf out "%d" c) out enum

let test_traversal_order () =
  let init = State.make [|0|] in
  let keys = Enum.take 50 (State.enum_int init 10) in
  let map  = PMap.of_enum (Enum.map (fun x -> (x,x)) keys) in
  let enum_1 () = PMap.enum map
  and enum_2 () =
    let list = Ref_list.empty () in
      PMap.iter (fun k v -> Ref_list.push list (k, v)) map;
      Ref_list.backwards list
  in
    match Enum.compare compare (enum_1 ()) (enum_2 ()) with
      | 0 -> (* pass *) ()
      | _ ->
          assert_failure
            (Printf.sprintf2 "Expected %a, got %a"
               print_enum (enum_1 ()) print_enum (enum_2 ()))

(* This test is incorrect *)
let test_multimap_empty_assoc_lists () =
  open Multi_pmap in
  let map =
    add 0 "foo" empty |> add 0 "bar" |> add 0 "sna" |>
    remove 0 "foo" |> remove 0 "bar" |> remove 0 "sna"
  in
    if not (mem 0 map) then
      assert_failure
        (Printf.sprintf "map[0] should be empty but contains %d bindings\n"
           (PSet.cardinal (find 0 map)))

let tests = "PMap" >::: [
  "traversal order iter vs. enum" >:: test_traversal_order;
(* This test is incorrect *)
(* "MultiPMap: removing empty association lists" >:: test_multimap_empty_assoc_lists; *)
]
