open Printf
open Batteries

exception Done_measuring

(*****************************************)
(*  PROBABILITY DISTRIBUTIONS            *)
(*****************************************)


let d_rand n_min n_max seed =
  Random.init seed;
  printf "#Random: S%d Min%d Max%d\n" seed n_min n_max;
  fun () -> n_min + Random.int (n_max-n_min)

let d_count n_min n_max _ =
  printf "#Count: Min%d Max%d\n" n_min n_max;
  let next_int =
    let i = ref n_min in
    fun () -> let ret = !i in incr i; if !i >= n_max then i := n_min; ret
  in
  fun () -> next_int ()

(*****************************************)
(*   ONE SECOND TESTING                  *)
(*****************************************)

let count_f f time trials rand_list =
  let list = ref [] in
  let list_len = ref 0 in
  printf "#Len\tIterations\n%!";
  list := rand_list 10 [];
  let test_n n =
    list := rand_list (n - !list_len) !list;
    list_len := n;
    let t0 = Sys.time() in
    ignore(f n !list); (* ignore the first run *)
    if Sys.time () -. t0 > 1. then raise Done_measuring;
    printf "%d\t%!" n;
    let t0 = Sys.time() in let t1 = t0 +. time in
    let count = ref (-1) in
    while Sys.time () < t1 do
      incr count;
      ignore(f n !list);
    done;
    printf "%d\n%!" !count;
    if !count < 2 then raise Done_measuring
  in
  List.iter test_n trials

(*****************************************)
(*   TESTS                               *)
(*****************************************)

let rec count_fall acc = function
    1 -> acc
  | n when n land 1 = 0 -> if acc > 10_000 then acc else count_fall (acc+1) (n / 2)
  | n -> if acc > 10_000 then acc else count_fall (acc+1) (n * 3 + 1)
let acc_cf x a = a + (count_fall 0 x)
let sumprod h1 h2 a = a + (h1 * h2)

let nth_test = "Nth", fun f n l -> ignore (f l (n/2))
and map_test1 = "Map: *2", fun f n l -> ignore (f (( * )2) l)
and map_test2 = "Map: /2 or *3+1", fun f n l -> ignore (f (count_fall 0) l)
and fold_right_test1 = "Fold_right: sum", fun f n l -> ignore (f (+) l 0)
and fold_right_test2 = "Fold_right: sum fall", fun f n l -> ignore (f acc_cf l 0)
and fold_right2_test1 = "Fold_right2: sum prod", fun f n l -> ignore (f sumprod l l 0)
and map2_test = "Map2: +", fun f n l -> ignore (f (+) l l)
and append_test = "Append self", fun f n l -> ignore (f l l)
and flatten_test = "Flatten three-copies", fun f n l -> ignore (f [l; l; l])
and filter_test1 = "Filter less_than 100", fun f n l -> ignore (f (fun n -> n < 100) l)
and filter_test2 = "Filter greater-than 100", fun f n l -> ignore (f (fun n -> n > 100) l)

(*****************************************)
(*  IMPLEMENTATIONS                      *)
(*****************************************)

let fold_right_max = 1000
let fold_right_chunk_size = 500

let fold_right3 f li init =
  let rec fold_chunk li =
    let (n, init) = jump 0 li in
    partial_fold init li n
  and jump n = function
    | [] -> (n, init)
    | _::tl when n < fold_right_chunk_size -> jump (n + 1) tl
    | li -> (n, fold_chunk li)
  and partial_fold partial_init li = function
    | 0 -> partial_init
    | n -> match li with
        | [] -> assert false
        | hd::tl -> f hd (partial_fold partial_init tl (n -1))  in
  let rec loop n = function
    | [] -> init
    | h :: t when n < fold_right_max -> f h (loop (n+1) t)
    | li -> fold_chunk li
  in loop 0 li

let core_map f l = Legacy.List.rev (Legacy.List.rev_map f l)
let core_fold_right f l a = Legacy.List.fold_left f a (Legacy.List.rev l)

type impl_type = [`Gallium | `Extlib | `Core | `Bluestorm]

let nth_impls = [`Gallium, Legacy.List.nth;
		 `Extlib, List.nth;
(*		 `Core, Core.Std.List.nth_exn*) ]
and map_impls = [`Gallium, Legacy.List.map;
		 `Extlib, List.map;
		 `Core, core_map ]
and fold_right_impls = [`Gallium, Legacy.List.fold_right;
			`Extlib, List.fold_right;
			`Bluestorm, fold_right3;
			`Core, core_fold_right]
and map2_impls = [`Gallium, Legacy.List.map2;
		  `Extlib, List.map2;
		  `Core, (fun f l1 l2 -> Legacy.List.rev (Legacy.List.rev_map2 f l1 l2)) ]
and fold_right2_impls = [`Gallium, Legacy.List.fold_right2;
			 `Extlib, List.fold_right2 ]

and append_impls = [`Gallium, Legacy.List.append;
		    `Extlib, List.append]
and flatten_impls = [`Gallium, Legacy.List.flatten;
		     `Extlib, List.flatten]
and remove_assoc_impls = [`Gallium, Legacy.List.remove_assoc;
			  `Extlib, List.remove_assoc]
and remove_assq_impls = [`Gallium, Legacy.List.remove_assq;
			 `Extlib, List.remove_assq]
and split_impls = [`Gallium, Legacy.List.split;
		   `Extlib, List.split]
and filter_impls = [`Gallium, Legacy.List.filter;
	      `Extlib, List.filter]
and find_all_impls = [`Gallium, Legacy.List.find_all;
		`Extlib, List.find_all]
and partition_impls = [`Gallium, Legacy.List.partition;
		`Extlib, List.partition]

(*****************************************)
(*  FRAMEWORK                            *)
(*****************************************)

let desc_to_str = function
    `Gallium -> "Gallium" | `Extlib -> "Extlib"
  | `Bluestorm -> "Bluestorm" | `Core -> "Core"

let make_tests (t_desc,test) impls =
  List.map (fun (i_desc,f) -> i_desc, count_f (test f)) impls


let tests =
  [
    "nth", make_tests nth_test nth_impls;
    "map", make_tests map_test1 map_impls;
    "mapx", make_tests map_test2 map_impls;
    "folr", make_tests fold_right_test1 fold_right_impls;
    "folrx", make_tests fold_right_test2 fold_right_impls;
    "map2", make_tests map2_test map2_impls;
(* has more complex performance characteristics than above *)
    "foldr2", make_tests fold_right2_test1 fold_right2_impls;
    "append", make_tests append_test append_impls;
    "flatten", make_tests flatten_test flatten_impls;
    "filter1", make_tests filter_test1 filter_impls;
    "filter2", make_tests filter_test2 filter_impls;
  ]

(*****************************************)
(*  GLOBALS FOR ARGUMENT HANDLING        *)
(*****************************************)
module Ref_list = RefList

let seed = ref (-1)
let time = ref 1.
let todo = Ref_list.empty() (* list of string test names *)
let distro = ref (d_rand 0 5000)
let int1 = ref 0 and int2 = ref 0
let impls : impl_type Ref_list.t = Ref_list.empty () (* list of impl_type values *)
let max_i = ref max_int

open Arg2
(*****************************************)
(*  MAIN                                 *)
(*****************************************)

let () =
  let set_f x = Ref_list.push todo x in
  let args =
    [ (Both ('s', "seed"), [Int_var seed], [], "Set random number seed");
      (Both ('g', "gallium"), [Unit (fun () -> Ref_list.push impls `Gallium)], [], "Test Gallium's implementation");
      (Both ('e', "extlib"), [Unit (fun () -> Ref_list.push impls `Extlib)], [], "Test Extlib's implementation");
      (Both ('b', "bluestorm"), [Unit (fun () -> Ref_list.push impls `Bluestorm)], [], "Test Bluestorm's implementation");
      (Both ('c', "core"), [Unit (fun () -> Ref_list.push impls `Core)], [], "Test Jane Street's implementation");
      (Both ('t', "time"), [Float_var time], [], "Set test duration (float)");
      (Both ('i', "max-i"), [Int_var max_i], [], "Maximum list length to test");
      (Both_extra ('R',"rand","min max"), [Int_var int1; Int_var int2; Unit (fun () -> distro := (d_rand !int1 !int2))], [], "Set Distribution of list values to a random min-max distribution")
    ]
  and usage_info = "t_list [-s seed] [test_name]"
  and descr = "Test various list function implementations"
  and notes = "by Eric Norige" in
  Arg2.parse args set_f usage_info descr notes;
  if !seed = -1 then (Random.self_init (); seed := Random.bits ());
  let trials = (* generate list of trial counts *)
    let dup_by n l = l @ (List.map (( * ) n) l) in
    [1; 2; 4; 7] |> dup_by 10 |> dup_by 100 |> dup_by 10000 |>
	List.filter (fun n -> n < !max_i) in
  let get_f test_name = (* get the test function by its name *)
    List.assoc test_name tests |>
	List.filter (fun (t, _) -> List.mem t (Ref_list.to_list impls))
  in
  let do_test (t,test) = try
    let rand_f = !distro !seed in
    let rec rand_list n li =
      if n <= 0 then li
      else rand_list (n-1) ((rand_f ())::li)
    in
    test !time trials rand_list with Done_measuring -> () in
  todo
    |> Ref_list.to_list
    |> List.rev_map get_f
    |> List.flatten
    |> List.iter do_test

