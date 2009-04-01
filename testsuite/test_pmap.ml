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

