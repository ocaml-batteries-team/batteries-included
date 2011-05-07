(* cd .. && ocamlbuild benchsuite/bench_set.native && _build/benchsuite/bench_set.native *)

(* The purpose of this test is to compare different implementation of
   the Set data structure. *)

let total_length = 500_000

let (-|) = BatStd.(-|)
let (<|) = BatStd.(<|)

module SetBench (M : sig val input_length : int end) = struct
  let input_length = M.input_length

  let nb_iter =
    max 10 (total_length / input_length)
  
  let () = Printf.printf "%d iterations\n" nb_iter

  let random_elt () = Random.int input_length

  let random_inputs () =
    BatList.init input_length (fun _ -> random_elt ())

  let make_samples input tests () =
    Benchmark.throughputN 1
      (List.map (fun (name, test) -> name, test, input) tests)

  (* we don't use BatInt to ensure that the same comparison function
     is used (PMap use Pervasives.compare by default), in order to
     have comparable performance results. *)
  module StdSet = BatSet.Make(struct type t = int let compare = compare end)

  module PSet = BatPSet

  let same_elts stdset pset =
    BatList.of_enum (StdSet.enum stdset)
    = BatList.of_enum (PSet.enum pset)

  (* A benchmark for key insertion *)
  let create_std_set input =
    List.fold_left
      (fun t e -> StdSet.add e t)
      StdSet.empty input
    
  let create_poly_set input =
    List.fold_left
      (fun t e -> PSet.add e t)
      PSet.empty input

  let create_input = random_inputs ()

  let std_created_set = create_std_set create_input
  let poly_created_set = create_poly_set create_input

  let () =
    assert (same_elts std_created_set poly_created_set)

  let samples_create = make_samples create_input
    [ "stdset create", ignore -| create_std_set;
      "pset create", ignore -| create_poly_set ]

  (* A benchmark for fast import *)
  let import_std_set input =
    StdSet.of_enum (BatList.enum input)
    
  let import_poly_set input =
    PSet.of_enum (BatList.enum input)

  let import_input = create_input

  let () =
    let std_imported_set = import_std_set import_input in
    assert (same_elts std_imported_set poly_created_set);
    let poly_imported_set = import_poly_set import_input in
    assert (same_elts std_created_set poly_imported_set);
    ()

  let samples_import = make_samples import_input
    [ "stdset import", ignore -| import_std_set;
      "pset import", ignore -| import_poly_set ]

  (* A benchmark for key lookup *)
  let lookup_input =
    random_inputs ()

  let lookup_std_set input =
    List.iter
      (fun k -> ignore (StdSet.mem k std_created_set))
      input

  let lookup_poly_set input =
    List.iter
      (fun k -> ignore (PSet.mem k poly_created_set))
      input

  let samples_lookup = make_samples lookup_input
    [ "stdset lookup", lookup_std_set;
      "pset lookup", lookup_poly_set ]

  (* A benchmark for key removal *)
  let remove_input =
    random_inputs ()

  let remove_std_set input =
    List.fold_left
      (fun t k -> StdSet.remove k t)
      std_created_set input

  let remove_poly_set input =
    List.fold_left
      (fun t k -> PSet.remove k t)
      poly_created_set input

  let () =
    assert (same_elts
              (remove_std_set remove_input)
              (remove_poly_set remove_input))

  let samples_remove = make_samples remove_input
    [ "stdset remove", ignore -| remove_std_set;
      "pset remove", ignore -| remove_poly_set ]


  let () =
    let create = samples_create () in
    let import = samples_import () in
    let lookup = samples_lookup () in
    let remove = samples_remove () in
    List.iter
      (print_newline -| Benchmark.tabulate)
      [
        create;
        import;
        lookup;
        remove;
      ]
end

let big_length = 100_000
let small_length = 500

let () =
  Printf.printf "Test with small sets (length = %d)\n%!" small_length;
  let () =
    let module M = SetBench(struct let input_length = small_length end) in
    () in

  print_newline ();
  print_newline ();
  
  Printf.printf "Test with big sets (length = %d)\n%!" big_length;
  let () =
    let module M = SetBench(struct let input_length = big_length end) in
    () in


  ()

  
