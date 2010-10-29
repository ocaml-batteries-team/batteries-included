(* cd .. && ocamlbuild benchsuite/test_maps.native && _build/benchsuite/test_maps.native *)

(* The purpose of this test is to compare different implementation of
   the Map associative data structure. *)

let total_length = 500_000

let (-|) = BatStd.(-|)
let (<|) = BatStd.(<|)

module MapBench (M : sig val input_length : int end) = struct
  let input_length = M.input_length

  let repeat = 3

  let nb_iter =
    max 10 (total_length / input_length)
  
  let () = Printf.printf "%d iterations\n" nb_iter

  let random_key () = Random.int input_length
  let random_value () = Random.int input_length

  let random_inputs random_elt () =
    BatList.init input_length (fun _ -> random_elt ())

  let make_samples tests =
    Benchmark.latencyN ~repeat (Int64.of_int nb_iter)
      (List.map (fun (name, test) -> name, test, ()) tests)

  (* we don't use BatInt to ensure that the same comparison function
     is used (PMap use Pervasives.compare by default), in order to
     have comparable performance results. *)
  module StdMap = BatMap.Make(struct type t = int let compare = compare end)

  module PMap = BatPMap

  let same_elts stdmap pmap =
    BatList.of_enum (StdMap.enum stdmap)
    = BatList.of_enum (PMap.enum pmap)

  (* A benchmark for key insertion *)
  let create_input_keys =
    random_inputs random_key ()
  let create_input_values =
    random_inputs random_value ()

  let create_std_map () =
    BatList.fold_left2
      (fun t k v -> StdMap.add k v t)
      StdMap.empty
      create_input_keys
      create_input_values
    
  let create_poly_map () =
    BatList.fold_left2
      (fun t k v -> PMap.add k v t)
      PMap.empty
      create_input_keys
      create_input_values
    
  let std_created_map = create_std_map ()
  let poly_created_map = create_poly_map ()

  let () =
    assert (same_elts std_created_map poly_created_map)

  let samples_create = make_samples
    [ "stdmap create", ignore -| create_std_map;
      "pmap create", ignore -| create_poly_map ]

  (* A benchmark for fast import *)
  let key_val_list =
    BatList.combine
      create_input_keys
      create_input_values

  let import_std_map () =
    StdMap.of_enum (BatList.enum key_val_list)
    
  let import_poly_map () =
    PMap.of_enum (BatList.enum key_val_list)

  let () =
    let std_imported_map = import_std_map () in
    assert (same_elts std_imported_map poly_created_map);
    let poly_imported_map = import_poly_map () in
    assert (same_elts std_created_map poly_imported_map);
    ()

  let samples_import = make_samples
    [ "stdmap import", ignore -| import_std_map;
      "pmap import", ignore -| import_poly_map ]

  (* A benchmark for key lookup *)
  let lookup_keys =
    random_inputs random_key ()

  let lookup_std_map () =
    List.iter
      (fun k -> ignore (StdMap.mem k std_created_map))
      lookup_keys

  let lookup_poly_map () =
    List.iter
      (fun k -> ignore (StdMap.mem k std_created_map))
      lookup_keys

  let samples_lookup = make_samples
    [ "stdmap lookup", lookup_std_map;
      "pmap lookup", lookup_poly_map ]

  (* A benchmark for merging *)
  let random_pairlist () =
    BatList.combine
      (random_inputs random_key ())
      (random_inputs random_value ())

  let p1 = random_pairlist ()
  let p2 = random_pairlist ()

  let merge_fun k a b =
    if k mod 2 = 0 then None else Some ()

  let merge_std_map =
    let m1 = StdMap.of_enum (BatList.enum p1) in
    let m2 = StdMap.of_enum (BatList.enum p2) in
    fun () ->
      StdMap.merge merge_fun m1 m2

  let merge_poly_map, merge_unsafe_poly_map =
    let m1 = PMap.of_enum (BatList.enum p1) in
    let m2 = PMap.of_enum (BatList.enum p2) in
    (fun () -> PMap.merge merge_fun m1 m2),
    (fun () -> PMap.merge_unsafe merge_fun m1 m2)

  let () =
    assert (same_elts (merge_std_map ()) (merge_poly_map ()));
    assert (same_elts (merge_std_map ()) (merge_unsafe_poly_map ()));
    ()

  let samples_merge = make_samples
    [ "stdmap merge", ignore -| merge_std_map;
      "pmap merge", ignore -| merge_poly_map;
      "pmap merge_unsafe", ignore -| merge_unsafe_poly_map ]

  (* compare fold-based and merge-based union *)
  let fold_union, merge_union, merge_unsafe_union, unsafe_union =
    let m1 = PMap.of_enum (BatList.enum p1) in
    let m2 = PMap.of_enum (BatList.enum p2) in
    let merge_fun k a b = if a <> None then a else b in
    (fun () -> PMap.foldi PMap.add m1 m2),
    (fun () -> PMap.merge merge_fun m1 m2),
    (fun () -> PMap.merge_unsafe merge_fun m1 m2),
    (fun () -> PMap.union_unsafe m1 m2)


  let () =
    let li m = BatList.of_enum (PMap.enum m) in
    assert (li (fold_union ()) = li (merge_union ()));
    assert (li (fold_union ()) = li (merge_unsafe_union ()));
    ()

  let samples_union = make_samples
    [ "fold-based union", ignore -| fold_union;
      "merge-based union", ignore -| merge_union;
      "merge-unsafe union", ignore -| merge_unsafe_union;
      "unsafe union", ignore -| unsafe_union ]

  (* A benchmark for key removal *)
  let remove_keys =
    random_inputs random_key ()

  let remove_std_map () =
    List.fold_left
      (fun t k -> StdMap.remove k t)
      std_created_map
      remove_keys

  let remove_poly_map () =
    List.fold_left
      (fun t k -> PMap.remove k t)
      poly_created_map
      remove_keys

  let () =
    assert (same_elts (remove_std_map ()) (remove_poly_map ()))

  let samples_remove = make_samples
    [ "stdmap remove", ignore -| remove_std_map;
      "pmap remove", ignore -| remove_poly_map ]

  let () =
    List.iter
      (print_newline -| Benchmark.tabulate)
      [
        samples_create;
        samples_import;
        samples_lookup;
        samples_merge;
        samples_union;
        samples_remove;
      ]
end

let big_length = 100_000
let small_length = 500

let () =
  Printf.printf "Test with small maps (length = %d)\n" small_length;
  let () =
    let module M = MapBench(struct let input_length = small_length end) in
    () in

  print_newline ();
  print_newline ();
  
  Printf.printf "Test with big maps (length = %d)\n" big_length;
  let () =
    let module M = MapBench(struct let input_length = big_length end) in
    () in


  ()

  
