open OUnit
open BatPervasives
open BatMultiPMap

let test_multimap_empty_assoc_lists () =
  let map =
    add 0 "foo" empty |> add 0 "bar" |> add 0 "sna" |>
    remove 0 "foo" |> remove 0 "bar" |> remove 0 "sna"
  in
    if mem 0 map then
      assert_failure
        (Printf.sprintf "map[0] should be empty but contains %d bindings\n"
           (BatPSet.cardinal (find 0 map)))

let tests = "MultiPMap" >::: [
  "MultiPMap: removing empty association lists" >:: test_multimap_empty_assoc_lists;
]
