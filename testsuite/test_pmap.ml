open Random

let print_enum out enum =
  Enum.print (fun out (c, _) -> Printf.fprintf out "%d" c) out enum

let test_1 =
  ("PMap: traversal order iter vs. enum",
   fun () ->
  let init = State.make [|0|] in
  let keys = Enum.take 50 (State.enum_int   init 10) in
  let map  = PMap.of_enum (Enum.map (fun x -> (x,x)) keys) in
  let enum_1 () = PMap.enum map
  and enum_2 () = 
    let list = Ref_list.empty () in
      PMap.iter (fun k v -> Ref_list.push list (k, v)) map;
      Ref_list.backwards list
  in
    match Enum.compare compare (enum_1 ()) (enum_2 ()) with
      | 0 -> Testing.Pass
      | _ -> Testing.Fail (Printf.sprintf2 "Hoping: %a\n\tGot: %a" print_enum (enum_1 ()) print_enum (enum_2 ()))
  )

(*This test is incorrenct: [find] always returns a set
let test_2 =
  ("MultiPMap: removing empty association lists",
   fun () ->
   open Multi_pmap in
  let map = remove 0 "sna" (remove 0 "bar" (remove 0 "foo" (add 0 "sna" (add 0 "bar" (add 0 "foo" empty)))))
  in
    if mem 0 map then Testing.Pass
    else Testing.Fail (Printf.sprintf2 "map[0] should be empty but contains %d bindings\n" (PSet.cardinal (find 0 map)))
  )
*)
