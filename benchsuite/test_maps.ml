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

  let random_input () =
    BatList.init input_length (fun _ -> Random.int input_length)

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
    random_input ()
  let create_input_values =
    random_input ()

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

  let samples_create =
    Benchmark.latencyN ~repeat (Int64.of_int nb_iter) [
      "stdmap create", ignore -| create_std_map, ();
      "pmap create", ignore -| create_poly_map, ();
    ]

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

  let samples_import =
    Benchmark.latencyN ~repeat (Int64.of_int nb_iter) [
      "stdmap import", ignore -| import_std_map, ();
      "pmap import", ignore -| import_poly_map, ();
    ]

  (* A benchmark for key lookup *)
  let lookup_keys =
    random_input ()

  let lookup_std_map () =
    List.iter
      (fun k -> ignore (StdMap.mem k std_created_map))
      lookup_keys

  let lookup_poly_map () =
    List.iter
      (fun k -> ignore (StdMap.mem k std_created_map))
      lookup_keys

  let samples_lookup =
    Benchmark.latencyN ~repeat (Int64.of_int nb_iter) [
      "stdmap lookup", lookup_std_map, ();
      "pmap lookup", lookup_poly_map, ();
    ]

  (* A benchmark for key removal *)
  let remove_keys =
    random_input ()

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

  let samples_remove =
    Benchmark.latencyN ~repeat (Int64.of_int nb_iter) [
      "stdmap remove", ignore -| remove_std_map, ();
      "pmap remove", ignore -| remove_poly_map, ();
    ]


  let () =
    List.iter
      (print_newline -| Benchmark.tabulate)
      [
        samples_create;
        samples_import;
        samples_lookup;
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

  
