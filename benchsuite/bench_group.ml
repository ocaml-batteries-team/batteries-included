(* Run with:
     make bench BENCH_TARGETS=benchsuite/bench_group.native
 *)
open Batteries

module HT = Hashtbl

let group_current cmp lst =
  let sorted = List.sort cmp lst in
  let fold first rest = List.fold_left
      (fun (acc, agr, last) elem ->
        if (cmp last elem) = 0 then (acc, elem::agr, elem)
        else (agr::acc, [elem], elem)
      )
      ([], [first], first)
      rest
  in
  match sorted with
  | [] -> []
  | hd::tl ->
    begin
      let groups, lastgr, _ = fold hd tl in
      List.rev_map List.rev (lastgr::groups)
    end

let group_berenger cmp = function
  | [] -> []
  | l ->
    let rev_cmp a b = cmp b a in
    let sorted = List.sort rev_cmp l in
    match sorted with
      | x :: xs ->
        let local, global, _ =
          List.fold_left
            (fun (local_acc, global_acc, last) y ->
              if 0 = cmp last y then
                (y :: local_acc, global_acc, y)
              else
                ([y], local_acc :: global_acc, y)
            )
            ([x], [], x)
            xs
        in
        local :: global
      | _ -> assert false

let group_ht_plus_set (type a) cmp = function
  | []  -> []  (* FBR: really necessary? *)
  | [x] -> [x] (* FBR: really necessary? *)
  | l ->
    let seen = Hashtbl.create 10 in
    let module S = Set.Make(struct type t = a let compare = cmp end) in
    let sorted =
      List.fold_left
        (fun acc x ->
          try
            let count = HT.find seen x in
            count := !count + 1;
            acc
          with Not_found ->
            HT.add seen x (ref 1);
            S.add x acc
        )
        S.empty
        l
    in
    failwith "not implemented yet"

let group_map (type a) cmp li =
  let module M = Map.Make(struct type t = a let compare = cmp end) in
  let rec gather m = function
    | [] -> m
    | x::xs -> gather (M.modify_def [] x (fun xs -> x::xs) m) xs
  in
  let return m = List.rev (M.fold (fun _x xs li -> xs::li) m []) in
  return (gather M.empty li)

let implems = [
  "current", group_current;
  "berenger", group_berenger;
  "map", group_map;
]

let equiv_result res1 res2 =
  List.for_all2
    (fun group1 group2 ->
      List.sort compare group1 = List.sort compare group2
    )
    res1 res2

let do_bench length name =
  let seq_list = (* [1; ...; length] *)
    List.init length (fun i -> 1 + i) in
  let random_list =
    let state = Random.State.make [|1;2;3;4;5|] in
    List.init length (fun _ -> 1 + Random.State.int state length)
  in

  let test group =
    let sqrt_length = int_of_float (sqrt (float_of_int length)) in
    [
      (* sequential small-ish groups *)
      group (fun x y -> compare (x / 10) (y / 10)) seq_list;
      (* random singleton groups *)
      group compare random_list;
      (* random small-ish groups *)
      group (fun x y -> compare (x / 10) (y / 10)) random_list;
      (* random large-ish groups *)
      group (fun x y -> compare (x / sqrt_length) (y / sqrt_length)) random_list;
    ]
  in

  (* first check that the functions agree (~ are correct) *)
  let reference_result = test group_current in
  let () =
    let check (name, implem) =
      let result = test implem in
      if not (List.for_all2 equiv_result reference_result result)
      then failwith (Printf.sprintf "implementation %s seems incorrect" name);
    in
    List.iter check implems
  in

  (* then measure speed *)
  let run group iters =
    for i=1 to iters do
      ignore (test group);
    done
  in

  Bench.bench_n
    (List.map
       (fun (impl_name, implem) -> (name^" "^impl_name, run implem))
       implems)

let () =
  let insane =
    (* run for only a few samples to avoid taking hours *)
    let open Bench in
    let old_samples = config.samples in
    config.samples <- 3;
    let result = do_bench 1_000_000 "insanely long" in
    config.samples <- old_samples;
    result
  in

  let benchs = [
    do_bench 100 "short";
    do_bench 1000 "long";
    do_bench 10_000 "very long";
    insane;
  ] in
  List.iter (print_newline % Bench.run_outputs) benchs


(*
short map (53.68 us) is 5.6% faster than
short current (56.86 us) which is 1.2% faster than
short bereger (57.56 us)
long bereger (914.24 us) is probably (alpha=29.99%) same speed as
long current (919.00 us) which is 9.6% faster than
long map (1.02 ms)
very long bereger (16.67 ms) is 7.4% faster than
very long current (18.00 ms) which is probably (alpha=31.02%) same speed as
very long map (18.10 ms)
*)
