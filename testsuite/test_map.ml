open BatStd
module R = BatRandom
module U = OUnit

let print_enum out enum =
  BatEnum.print (fun out (c, _) -> BatPrintf.fprintf out "%d" c) out enum

let assert_equal_enums enum_1 enum_2 =
  match BatEnum.compare compare (enum_1 ()) (enum_2 ()) with
    | 0 -> (* pass *) ()
    | _ ->
        U.assert_failure
          (BatPrintf.sprintf2 "Expected %a, got %a"
             print_enum (enum_1 ()) print_enum (enum_2 ()))

let assert_equal_maps map_1 map_2 =
  let enum_1 () = BatPMap.enum map_1 in
  let enum_2 () = BatPMap.enum map_2 in
  assert_equal_enums enum_1 enum_2

let test_traversal_order () =
  let init = R.State.make [|0|] in
  let keys = BatEnum.take 50 (R.State.enum_int init 10) in
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
          U.assert_failure
            (BatPrintf.sprintf2 "Expected %a, got %a"
               print_enum (enum_1 ()) print_enum (enum_2 ()))

let gen_map state bound count =
  let keys = BatEnum.take count (R.State.enum_int state bound) in
  BatPMap.of_enum (BatEnum.map (fun x -> (x,x)) keys)

let test_split () =
  let do_test map v =
    let m1, vo, m2 = BatPMap.split v map in
    assert_equal_maps m1 (BatPMap.filteri (fun k _ -> k < v) map);
    assert_equal_maps m2 (BatPMap.filteri (fun k _ -> k > v) map);
    U.assert_equal vo (if BatPMap.mem v map then Some v else None)
  in
  let init = R.State.make [|0|] in
  for i = 0 to 50 do
    let bound = 40 in
    let count = i * 5 in
    do_test (gen_map init bound count) (R.State.int init bound)
  done


let (>:), (>::), (>:::) = U.(>:), U.(>::), U.(>:::)
let (@?) = U.(@?)
let (@!) msg (exn, f) = U.assert_raises ~msg exn f

(* This functor is intended the features that are common in both the
   functorized Map and the polymorphic PMap data structures.

   Currently, those two modules have a different interfaces : there
   are functions in one that aren't present in another. The tests are
   therefore not exhaustive : only common features are tested (but all
   such functions are tested), and PMap-specific functions should be
   tested separately. As we hope, however, to make the feature set of
   both module converge in the long term, more features of one will be
   added to the other, and eventually all the features of both will be
   present here.


   Functions that are currently Map-specific :
     compare, equal, keys, values

   Functions that are currently PMap-specific :
     extract, pop, union, diff, intersect, add_carry, partition

   Functions that have been added in 3.12 stdlib's Map, but not yet
   added in Batteries :
     merge

   Functions that have been added in 3.12 stdlib's map, already
   present in batteries's Map or PMap:
     for_all, exists, filter, `bindings` (to_list)
*)
module TestMap
  (M: sig
    type 'a m
    type key = int

    (* tested functions *)
    val empty : 'a m
    val is_empty : _ m -> bool
    val singleton : key -> 'a -> 'a m
    val find : key -> 'a m -> 'a
    val add : key -> 'a -> 'a m -> 'a m
    val remove : key -> 'a m -> 'a m
    val mem : key -> _ m -> bool
    val cardinal : _ m -> int
    val min_binding : 'a m -> (key * 'a)
    val max_binding : 'a m -> (key * 'a)
    val modify : key -> ('a -> 'a) -> 'a m -> 'a m
    val modify_def : 'a -> key -> ('a -> 'a) -> 'a m -> 'a m

    val fold : ('a -> 'b -> 'b) -> 'a m -> 'b -> 'b
    val foldi : (key -> 'a -> 'b -> 'b) -> 'a m -> 'b -> 'b
    val iter : ('a -> unit) -> 'a m -> unit
    val iteri : (key -> 'a -> unit) -> 'a m -> unit
    val map : ('a -> 'b) -> 'a m -> 'b m
    val mapi : (key -> 'a -> 'b) -> 'a m -> 'b m
    val filter : ('a -> bool) -> 'a m -> 'a m
    val filteri : (key -> 'a -> bool) -> 'a m -> 'a m
    val filter_map : ('a -> 'b option) -> 'a m -> 'b m
    val filteri_map : (key -> 'a -> 'b option) -> 'a m -> 'b m

    val enum : 'a m -> (key * 'a) BatEnum.t
    val backwards : 'a m -> (key * 'a) BatEnum.t
    val of_enum : (key * 'a) BatEnum.t -> 'a m

    val for_all : (key -> 'a -> bool) -> 'a m -> bool
    val exists : (key -> 'a -> bool) -> 'a m -> bool

    val choose : 'a m -> (key * 'a)
    val split : key -> 'a m -> ('a m * 'a option * 'a m)

    val print :
      ?first:string -> ?last:string -> ?sep:string ->
      ('a BatInnerIO.output -> key -> unit) ->
      ('a BatInnerIO.output -> 'c -> unit) ->
      'a BatInnerIO.output -> 'c m -> unit

  end)
= struct

  let li t = BatList.of_enum (M.enum t)
  let il li = M.of_enum (BatList.enum li)

  let eq_li ?msg cmp_elt print_elt l1 l2 =
    let cmp t1 t2 =
      let cmp = BatPair.compare ~c1:BatInt.compare ~c2:cmp_elt in
      0 = BatList.make_compare cmp t1 t2 in
    let printer =
      BatIO.to_string -| BatList.print <| BatPair.print BatInt.print print_elt in
    U.assert_equal ?msg ~cmp ~printer l1 l2

  let eq ?msg cmp_elt print_elt t1 t2 =
    eq_li ?msg cmp_elt print_elt (li t1) (li t2)

  let test_is_empty () =
    "empty is empty" @? M.is_empty M.empty;
    "singleton is not empty" @? not (M.is_empty <| M.singleton 1 ());
    ()

  let test_singleton () =
    let k, v = 1, 'a' in
    "remove k (singleton k v) is empty" @?
      M.is_empty (M.remove k (M.singleton k v));
    "find k (singleton k v) is v" @?
      (M.find k (M.singleton k v) = v);
    "to_list (singleton k v) = [(k, v)]" @?
      (li (M.singleton k v) = [(k, v)]);
    ()

  let test_add () =
    let t = il [(3,4); (5, 6)] in
    eq BatInt.compare BatInt.print
      (M.add 1 4 (M.add 1 7 t))
      (M.add 1 4 t);
    eq BatInt.compare BatInt.print
      (M.add 4 8 t)
      (il [(3,4); (4,8); (5,6)]);
    ()

  let test_cardinal () =
    let k, k', v = 1, 2, 3 in
    "cardinal empty = 0" @?
      (M.cardinal M.empty = 0);
    "cardinal (singleton k v) = 1" @?
      (M.cardinal (M.singleton k v) = 1);
    "k <> k' => cardinal (add k' v (singleton k v)) = 2" @?
      (k <> k' && M.cardinal (M.add k' v (M.singleton k v)) = 2);
    "mem k t => cardinal (remove k t) = cardinal t - 1" @?
      (let t = il [k,v; k',v] in
       M.cardinal (M.remove k t) = M.cardinal t - 1);
    ()

  let test_find () =
    let t = il [(3,4); (5, 6)] in
    "find 3 t = 4" @? (M.find 3 t = 4);
    "find 4 t -> Not_found" @!
      (Not_found, fun () -> M.find 6 t);
    let test_cardinal k v t =
      "cardinal (add k v t) = cardinal t + (mem k t ? 0 : 1)" @?
        (M.cardinal (M.add k v t) =
            M.cardinal t + if M.mem k t then 0 else 1) in
    test_cardinal 3 0 t;
    test_cardinal 57 0 t;
    ()

  let test_remove () =
    let t = il [(3,4); (5, 6)] in
    "find k (remove k (add k v (add k v' t))) -> Not_found" @!
      (Not_found, fun () ->
        let k, v, v', t = 1, 4, 5, t in
        M.find k (M.remove k (M.add k v (M.add k v' t))));
    let test_cardinal k t =
      "cardinal (remove k t) = cardinal t - (mem k t ? 1 : 0)" @?
        (M.cardinal (M.remove k t) =
            M.cardinal t - if M.mem k t then 1 else 0) in
    test_cardinal 3 t;
    test_cardinal 57 t;
    ()

  let test_mem () =
    let k, k', v = 1, 2, () in
    "mem k (singleton k v)" @? M.mem k (M.singleton k v);
    "not (mem k (singleton k' v))" @? not (M.mem k (M.singleton k' v));
    ()

  let test_min_binding () =
    let t = il [(2, 0); (1,2); (3, 4); (2, 0)] in
    "min_binding [(2, 0); (1,2); (3, 4); (2, 0)] = (1, 2)" @?
      (M.min_binding t = (1, 2));
    ()

  let test_max_binding () =
    let t = il [(2, 0); (1,2); (3, 4); (2, 0)] in
    "max_binding [(2, 0); (1,2); (3, 4); (2, 0)] = (3, 4)" @?
      (M.max_binding t = (3, 4));
    ()

  let test_modify () =
    let k, k', f, t = 1, 2, ((+) 1), il [(1,2); (3, 4)] in
    "mem k t => find k (modify k f t) = f (find k t)" @?
      (M.find k (M.modify k f t) = f (M.find k t));
    "not (mem k' t) => modify k' f t -> Not_found" @!
      (Not_found, fun () -> M.modify k' f t);
    ()

  let test_modify_def () =
    let sum t = M.fold (+) t 0 in
    let t = il [(1, 2); (3, 4)] in
    let test k t =
      "sum (modify_def 1 k (+1) t) = sum t + (mem k t ? 1 : 2)" @?
        (sum (M.modify_def 1 k ((+)1) t) = sum t + if M.mem k t then 1 else 2) in
    test 1 t;
    test 57 t;
    ()

  let test_choose () =
    "choose empty -> Not_found" @!
      (Not_found, fun () -> M.choose M.empty);
    let t = il [(1,2); (3,4)] in
    "mem (fst (choose t)) t" @?
      (M.mem (M.choose t |> fst) t);
    ()

  let test_split () =
    let k, v, t = 1, 2, il [0,1; 2,3; 4,5] in
    "split k empty = (empty, None, empty)" @?
      (let (l, m, r) = M.split k M.empty in
       M.is_empty l && m = None && M.is_empty r);
    "split k (singleton k v) = (empty, Some v, empty)" @?
      (let (l, m, r) = M.split k (M.singleton k v) in
       M.is_empty l && m = Some v && M.is_empty r);
    "split 2 [0,1; 2,3; 4,5] = [0,1], Some 3, [4,5]" @?
      (let (l, m, r) = M.split 2 t in
       li l = [0,1] && m = Some 3 && li r = [4,5]);
    "split 1 [0,1; 2,3; 4,5] = [0,1], None, [2,3; 4,5]" @?
      (let (l, m, r) = M.split 1 t in
       li l = [0,1] && m = None && li r = [2,3; 4,5]);
    "split (fst (min_binding t)) t = (empty, Some (snd (min_binding t)), remove_min_binding t)" @?
      (let mk, mv = M.min_binding t in
       let (l, m, r) =  M.split mk t in
       M.is_empty l && m = Some mv && li r = li (M.remove mk r));
    "split (fst (max_binding t)) t = (empty, Some (snd (max_binding t)), remove_max_binding t)" @?
      (let mk, mv = M.max_binding t in
       let (l, m, r) =  M.split mk t in
       li l = li (M.remove mk l) && m = Some mv && M.is_empty r);
    ()

  let test_for_all_exists () =
    let test (msg, for_all) =
      let (@?) str = (@?) (Printf.sprintf "[%s] %s" msg str) in
      "for_all (fun _ _ -> false) empty" @?
        for_all (fun _ _ -> false) M.empty;
      "for_all (fun _ _ -> true) empty" @?
        for_all (fun _ _ -> true) M.empty;
      let k, v = 1, 2 in
      "for_all (fun _ _ -> true) (singleton k v)" @?
        for_all (fun _ _ -> true) (M.singleton k v);
      "not (for_all (fun _ _ -> false) (singleton k v))" @?
        not (for_all (fun _ _ -> false) (M.singleton k v));
      "for_all (fun k' _ -> k = k') (singleton k v)" @?
        for_all (fun k' _ -> k = k') (M.singleton k v);
      "for_all (=) [0,0; 1,1]" @?
        for_all (=) (il [0,0; 1,1]);
      "not (for_all (=) [0,0; 1,2])" @?
        not (for_all (=) (il [0,0; 1,2]));
      ()
    in
    let not_not_exists f li =
      let not_f k v = not (f k v) in
      not (M.exists not_f li) in
    List.iter test
      [ "for_all", M.for_all;
        "not not exists", not_not_exists ]

  let test_print () =
    let test str li =
      let str' =
        BatIO.to_string
          (M.print ~first:"{" ~last:"}" ~sep:", "
             BatInt.print BatInt.print)
          (il li) in
      U.assert_equal
        ~msg:"printing test"
        ~cmp:(fun x y -> 0 = String.compare x y)
        ~printer:(fun x -> x)
        str' str in
    test "{}" [];
    test "{0: 1}" [(0, 1)];
    test "{0: 1, 2: 3}" [(0, 1); (2, 3)];
    ()

  let test_enums () =
    (* test enum, of_enum, backwards *)
    let test_of_enum f name_f t =
      eq ~msg:(Printf.sprintf "of_enum (%s t) = t" name_f)
        BatInt.compare BatInt.print
        (M.of_enum (f t)) t in
    List.iter (fun (f, name_f) ->
      test_of_enum f name_f (il []);
      test_of_enum f name_f (il [(0,1); (4,5); (2, 3)]))
      [
        M.enum, "enum";
        M.backwards, "backwards";
      ]

  let reindex (f : M.key -> 'a -> 'b) : 'a -> 'b =
    let count = ref (-1) in
    fun x -> incr count; f !count x

  let test_iterators () =
    (* we test all iter(i)/fold(i)/map(i)/filter(i)_map in one go, by
       building a common filteri_map implementation, by using
       side-effects for iter/map, and a referenced counter for non-i
       variants (assumes consecutive keys from 0 to N-1).

       In particular, the side-effects assume that all iterators
       process the elements in increasing key order. This was not true
       of PMap iterator functions, and I have changed them to respect
       that invariant (be it exposed in the documented or
       unspecified).

       I don't pretend this test is strong enough, but it's the less
       cumbersome that I could find, and it should still catch a wide
       range of regressions (obvious breakage, application order
       change...), and has already spotted instances of such issues.
    *)

    let from_filteri_map f t =
      li (M.filteri_map f t) in

    let from_filter_map f t =
      li (M.filter_map (reindex f) t) in

    let of_foldi f k v acc =
      match f k v with
        | None -> acc
        | Some v' -> (k,v')::acc in

    let from_foldi f t =
      List.rev <| M.foldi (of_foldi f) t [] in

    let from_fold f t =
      List.rev <| M.fold (reindex (of_foldi f)) t [] in

    let of_iteri acc f k v =
      match f k v with
        | None -> ()
        | Some v' -> acc := (k, v') :: !acc in

    let from_iteri f t =
      let acc = ref [] in
      M.iteri (of_iteri acc f) t;
      List.rev !acc in

    let from_iter f t =
      let acc = ref [] in
      M.iter (reindex (of_iteri acc f)) t;
      List.rev !acc in

    let of_mapi acc f k v =
      of_iteri acc f k v;
      v in

    let from_mapi f t =
      let acc = ref [] in
      let res = M.mapi (of_mapi acc f) t in
      eq ~msg:"iterators test : mapi result test"
        BatInt.compare BatInt.print
        t res;
      List.rev !acc in

    let from_map f t =
      let acc = ref [] in
      let res = M.map (reindex (of_mapi acc f)) t in
      eq ~msg:"iterators test : map result test"
        BatInt.compare BatInt.print
        t res;
      List.rev !acc in

    let from_filteri f t =
      t
      |> M.filteri (fun k v -> f k v <> None)
      |> M.mapi
          (fun k v ->
            match f k v with
              | None -> assert false
              | Some v' -> v')
      |> li in

    let from_filter f t =
      t
      |> M.filter (reindex (fun k v -> f k v <> None))
      |> M.mapi
          (fun k v ->
            match f k v with
              | None -> assert false
              | Some v' -> v')
      |> li in

    (* I took care to write the input unsorted, to observe potential
       sorting bugs *)
    let t = il [(4, 4); (5, 5); (3, 3); (0, 0); (6, 6); (2, 2); (1, 1)] in

    (* the function which all filteri_map implementations will use *)
    let f k v =
      if k mod 2 = 0 then Some (v + 1)
      else None in

    (* result (in sorted order) *)
    let result = [(0, 1); (2, 3); (4, 5); (6, 7)] in

    List.iter
      (fun (name, filteri_map_n) ->
        let msg = Printf.sprintf "iterators test : %s" name in
        eq_li ~msg BatInt.compare BatInt.print result (filteri_map_n f t))
      [
        "filteri_map", from_filteri_map;
        "filter_map", from_filter_map;
        "foldi", from_foldi;
        "fold", from_fold;
        "iteri", from_iteri;
        "iter", from_iter;
        "mapi", from_mapi;
        "map", from_map;
        "filteri", from_filteri;
        "filter", from_filter;
      ]

  let tests = [
    "test_is_empty" >:: test_is_empty;
    "test_singleton" >:: test_singleton;
    "test_cardinal" >:: test_cardinal;
    "test_add" >:: test_add;
    "test_find" >:: test_find;
    "test_remove" >:: test_remove;
    "test_mem" >:: test_mem;
    "test_min_binding" >:: test_min_binding;
    "test_max_binding" >:: test_max_binding;
    "test_modify" >:: test_modify;
    "test_modify_def" >:: test_modify_def;
    "test_choose" >:: test_choose;
    "test_split" >:: test_split;
    "test_for_all_exists" >:: test_for_all_exists;
    "test_print" >:: test_print;
    "test_enums" >:: test_enums;
    "test_iterators" >:: test_iterators;
  ]
end

module M = struct
  module M = BatMap.Make(BatInt)
  include M
  type 'a m = 'a M.t

  let fold f = M.fold (fun _ -> f)
  let foldi = M.fold

  let iter f = M.iter (fun _ -> f)
  let iteri = M.iter

  let filter_map f = M.filter_map (fun _ -> f)
  let filteri_map = M.filter_map
end

module P = struct
  module M = BatPMap
  include M

  type key = int
  type 'a m = (key, 'a) M.t

  let singleton k v = M.singleton ?cmp:None k v

  let of_enum t = M.of_enum ?cmp:None t

  let iter f = M.iter (fun _ -> f)
  let iteri = M.iter

  let filter_map f = M.filter_map (fun _ -> f)
  let filteri_map = M.filter_map

  let exists = M.exists_f
end

module TM = TestMap(M)
module TP = TestMap(P)

let tests = "(P)Map" >::: [
  "traversal order iter vs. enum" >:: test_traversal_order;
  "split" >:: test_split;
  "usual tests on Map" >::: TM.tests;
  "usual tests on PMap" >::: TP.tests;
]
