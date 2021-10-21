open Batteries
module U = OUnit

module IS = Set.Make(Int)

let of_list l = List.fold_left (fun a i -> IS.add i a) IS.empty l

let s1 = of_list [1;2;3]
let s2 = of_list [1;2]

let asseq_int = U.assert_equal ~printer:string_of_int

let test_subset_compare () =
  asseq_int 1 (IS.compare_subset s1 s2);
  asseq_int (-1) (IS.compare_subset s2 s1)


let (>:), (>::), (>:::) = U.(>:), U.(>::), U.(>:::)
let (@?) = U.(@?)
let (@!) msg (exn, f) = U.assert_raises ~msg exn f

(* This functor is intended the features that are common in both the
   functorized Set and the polymorphic Set data structures.

   Currently, those two modules have a different interfaces : there
   are functions in one that aren't present in another. The tests are
   therefore not exhaustive : only common features are tested (but all
   such functions are tested), and Set-specific functions should be
   tested separately. As we hope, however, to make the feature set of
   both module converge in the long term, more features of one will be
   added to the other, and eventually all the features of both will be
   present here.
*)
module TestSet
  (S: sig
    type s
    type elt = int

    val equal : s -> s -> bool

    (* tested functions *)
    val empty : s
    val is_empty : s -> bool
    val singleton : elt -> s
    val add : elt -> s -> s
    val remove : elt -> s -> s
    val mem : elt -> s -> bool
    val cardinal : s -> int
    val min_elt : s -> elt
    val max_elt : s -> elt

    val pop : s -> elt * s

    val fold : (elt -> 'b -> 'b) -> s -> 'b -> 'b
    val iter : (elt -> unit) -> s -> unit
    val filter : (elt -> bool) -> s -> s

    val enum : s -> elt BatEnum.t
    val backwards : s -> elt BatEnum.t
    val of_enum : elt BatEnum.t -> s

    val for_all : (elt -> bool) -> s -> bool
    val exists : (elt -> bool) -> s -> bool

    val partition : (elt -> bool) -> s -> s * s

    val choose : s -> elt
    val split : elt -> s -> s * bool * s

    val union : s -> s -> s
    val inter : s -> s -> s
    val diff : s -> s -> s
    val sym_diff : s -> s -> s
    val disjoint : s -> s -> bool

    val min_elt_opt : s -> elt option
    val max_elt_opt : s -> elt option
    val update : elt -> elt -> s -> s
    val find_opt : elt -> s -> elt option
    val find_first : (elt -> bool) -> s -> elt
    val find_first_opt : (elt -> bool) -> s -> elt option
    val find_last : (elt -> bool) -> s -> elt
    val find_last_opt : (elt -> bool) -> s -> elt option
    val choose_opt : s -> elt option
    val map : (elt -> elt) -> s -> s
    val map_endo : (elt -> elt) -> s -> s
    val filter_map : (elt -> elt option) -> s -> s
    val filter_map_endo : (elt -> elt option) -> s -> s
    val add_seq : elt BatSeq.t -> s -> s
    val of_seq : elt BatSeq.t -> s
    val to_seq : s -> elt BatSeq.t
    val to_rev_seq : s -> elt BatSeq.t
    val to_seq_from : elt -> s -> elt BatSeq.t
    val elements : s -> elt list
          
    val print :
      ?first:string -> ?last:string -> ?sep:string ->
      ('a BatInnerIO.output -> elt -> unit) ->
      'a BatInnerIO.output -> s -> unit
  end)
= struct

  let li t = BatList.of_enum (S.enum t)
  let il li = S.of_enum (BatList.enum li)
  let s1 = il [1;2;3]

  let eq_li ?msg cmp_elt print_elt l1 l2 =
    let cmp t1 t2 =
      0 = BatList.compare cmp_elt t1 t2 in
    let printer =
      BatIO.to_string @@ BatList.print print_elt in
    U.assert_equal ?msg ~cmp ~printer l1 l2

  let eq ?msg cmp_elt print_elt t1 t2 =
    eq_li ?msg cmp_elt print_elt (li t1) (li t2)

  let (@=) msg (t1, t2) =
    eq ~msg BatInt.compare BatInt.print t1 t2

  let test_is_empty () =
    "empty is empty" @? S.is_empty S.empty;
    "singleton is not empty" @? not (S.is_empty @@ S.singleton 1);
    ()

  let test_singleton () =
    let k = 1 in
    "remove k (singleton k) is empty" @?
      S.is_empty (S.remove k (S.singleton k));
    "mem k (singleton k)" @?
      (S.mem k (S.singleton k));
    "to_list (singleton k) = [k]" @?
      (li (S.singleton k) = [k]);
    ()

  let test_add () =
    let k, t = 1, il [3; 5] in
    "add k (add k t) = add k t" @=
      (S.add k (S.add k t), S.add k t);
    "add 4 [3; 5] = [3; 4; 5]" @=
        (S.add 4 t, il [3; 4; 5]);
    "add returns phys eq. set" @=
      (s1, (S.add 2 s1));
    ()

  let test_cardinal () =
    let k, k' = 1, 2 in
    "cardinal empty = 0" @?
      (S.cardinal S.empty = 0);
    "cardinal (singleton k) = 1" @?
      (S.cardinal (S.singleton k) = 1);
    "k <> k' => cardinal (add k' (singleton k)) = 2" @?
      (k <> k' && S.cardinal (S.add k' (S.singleton k)) = 2);
    "mem k t => cardinal (remove k t) = cardinal t - 1" @?
      (let t = il [k; k'] in
       S.cardinal (S.remove k t) = S.cardinal t - 1);
    ()

  let test_remove () =
    let t = il [3; 5] in
    "not (mem k (remove k (add k (add k t))))" @?
      (let k, t = 1, t in
        not (S.mem k (S.remove k (S.add k (S.add k t)))));
    let test_cardinal k t =
      "cardinal (remove k t) = cardinal t - (mem k t ? 1 : 0)" @?
        (S.cardinal (S.remove k t) =
            S.cardinal t - if S.mem k t then 1 else 0) in
    test_cardinal 3 t;
    test_cardinal 57 t;
    "remove 4 s1 == s1" @? (s1 == (S.remove 4 s1));
    "remove 4 empty == empty" @? (S.empty == (S.remove 4 S.empty));
    ()

  let test_update () =
    "update 2 2 s1 == s1" @? (s1 == S.update 2 2 s1);
    "update 2 5 s1 == of_list[1;3;5]" @= (il [1;3;5], S.update 2 5 s1);
    ()

  let test_mem () =
    let k, k' = 1, 2 in
    "mem k (singleton k)" @? S.mem k (S.singleton k);
    "not (mem k (singleton k'))" @? not (S.mem k (S.singleton k'));
    ()

  let test_min_elt () =
    let t = il [2; 1; 3; 2] in
    "min_elt [2; 1; 3; 2] = 1" @?
      (S.min_elt t = 1);
    "min_elt empty -> Not_found" @!
      (Not_found, fun () -> S.min_elt S.empty);
    ()

  let test_max_elt () =
    let t = il [2; 1; 3; 2] in
    "max_elt [2; 1; 3; 2] = 3" @?
      (S.max_elt t = 3);
    "max_elt empty -> Not_found" @!
      (Not_found, fun () -> S.max_elt S.empty);
    ()

  let test_min_elt_opt () =
    let t = il [2; 1; 3; 2] in
    "min_elt_opt [2; 1; 3; 2] = Some 1" @?
      (S.min_elt_opt t = Some 1);
    "min_elt_opt [] = None" @?
      (S.min_elt_opt S.empty = None);
    ()

  let test_max_elt_opt () =
    let t = il [2; 1; 3; 2] in
    "max_elt_opt [2; 1; 3; 2] = Some 3" @?
      (S.max_elt_opt t = Some 3);
    "max_elt_opt [] = None" @?
      (S.max_elt_opt S.empty = None);
    ()

  let test_choose () =
    "choose empty -> Not_found" @!
      (Not_found, fun () -> S.choose S.empty);
    let t = il [1; 3] in
    "mem (choose t) t" @?
      (S.mem (S.choose t) t);
    ()

  let test_choose_opt () =
    "choose_opt empty = None" @?
      (S.choose_opt S.empty = None);
    let t = il [1; 3] in
    "mem (Option.get (choose t)) t" @?
      (S.mem (Option.get (S.choose_opt t)) t);
    ()

  let test_pop () =
    "pop empty -> Not_found" @!
      (Not_found, fun () -> S.pop S.empty);
    let t = il [1; 2; 3; 4] in
    "not (mem (fst (pop t)) (snd (pop t)))" @?
      (not @@ S.mem (fst @@ S.pop t) (snd @@ S.pop t));
    "let (k,t') = pop t in add k t' = t" @?
      (let k, t' = S.pop t in S.equal (S.add k t') t);
    ()

  let test_split () =
    let k, _v, t = 1, 2, il [0; 1; 2; 4; 5] in
    "split k empty = (empty, false, empty)" @?
      (let (l, p, r) = S.split k S.empty in
       S.is_empty l && p = false && S.is_empty r);
    "split k (singleton k) = (empty, true, empty)" @?
      (let (l, p, r) = S.split k (S.singleton k) in
       S.is_empty l && p = true && S.is_empty r);
    "split 2 [0; 1; 2; 4; 5] = [0; 1], true, [4; 5]" @?
      (let (l, p, r) = S.split 2 t in
       li l = [0;1] && p = true && li r = [4;5]);
    "split 3 [0; 1; 2; 4; 5] = [0; 1; 2], false, [4; 5]" @?
      (let (l, p, r) = S.split 3 t in
       li l = [0;1;2] && p = false && li r = [4;5]);
    "split (min_elt t) t = (empty, true, remove_min_elt t)" @?
      (let mk = S.min_elt t in
       let (l, p, r) =  S.split mk t in
       S.is_empty l && p = true && li r = li (S.remove mk r));
    "split (max_elt t) t = (remove_max_elt t, true, empty)" @?
      (let mk = S.max_elt t in
       let (l, p, r) =  S.split mk t in
       li l = li (S.remove mk l) && p = true && S.is_empty r);
    ()

  let test_partition () =
    let t = il [0; 1; 2; 3; 4] in
    let p k = k mod 2 = 0 in
    "partition (fun k -> k mod 2 = 0) [0; 1; 2; 3; 4] = [0; 2; 4], [1; 3]" @?
      (let l, r = S.partition p t in
       li l = [0; 2; 4] && li r = [1; 3]);
    "partition (fun _ -> true) t = t, empty" @?
      (let l, r = S.partition (fun _ -> true) t in
       S.equal l t && S.is_empty r);
    "partition (fun _ -> false) t = empty, t" @?
      (let l, r = S.partition (fun _ -> false) t in
       S.is_empty l && S.equal r t);
    ()


  let test_union () =
    "union [1; 2; 3] [2; 3; 4] = [1; 2; 3; 4]" @=
      (il [1; 2; 3; 4], S.union (il [1; 2; 3]) (il [2; 3; 4]));
    "union [1; 2; 3] [2; 3] = [1; 2; 3]" @=
      (il [1; 2; 3], S.union (il [1; 2; 3]) (il [2; 3]));
    "union [2; 3] [2; 3; 4] = [2; 3; 4]" @=
      (il [2; 3; 4], S.union (il [2; 3]) (il [2; 3; 4]));
    "union [2; 3] [2; 3] = [2; 3]" @=
      (il [2; 3], S.union (il [2; 3]) (il [2; 3]));
    "union [2] [] = [2]" @=
      (il [2], S.union (il [2]) (il []));
    "union [] [3] = [3]" @=
      (il [3], S.union (il []) (il [3]));
    ()

  let test_inter () =
    "inter [1; 2; 3] [2; 3; 4] = [2; 3]" @=
      (il [2; 3], S.inter (il [1; 2; 3]) (il [2; 3; 4]));
    "inter [1; 2; 3] [2; 3] = [2; 3]" @=
      (il [2; 3], S.inter (il [1; 2; 3]) (il [2; 3]));
    "inter [2; 3] [2; 3; 4] = [2; 3]" @=
      (il [2; 3], S.inter (il [2; 3]) (il [2; 3; 4]));
    "inter [2; 3] [2; 3] = [2; 3]" @=
      (il [2; 3], S.inter (il [2; 3]) (il [2; 3]));
    "inter [2] [] = []" @=
      (il [], S.inter (il [2]) (il []));
    "inter [] [3] = []" @=
      (il [], S.inter (il []) (il [3]));
    ()

  let test_diff () =
    "diff [1; 2; 3] [2; 3; 4] = [1]" @=
      (il [1], S.diff (il [1; 2; 3]) (il [2; 3; 4]));
    "diff [1; 2; 3] [2; 3] = [1]" @=
      (il [1], S.diff (il [1; 2; 3]) (il [2; 3]));
    "diff [2; 3] [2; 3; 4] = []" @=
      (il [], S.diff (il [2; 3]) (il [2; 3; 4]));
    "diff [2; 3] [2; 3] = []" @=
      (il [], S.diff (il [2; 3]) (il [2; 3]));
    "diff [2] [] = [2]" @=
      (il [2], S.diff (il [2]) (il []));
    "diff [] [3] = []" @=
      (il [], S.diff (il []) (il [3]));
    ()

  let test_sym_diff () =
    "sym_diff [1; 2; 3] [2; 3; 4] = [1; 4]" @=
      (il [1; 4], S.sym_diff (il [1; 2; 3]) (il [2; 3; 4]));
    "sym_diff [1; 2; 3] [2; 3] = [1]" @=
      (il [1], S.sym_diff (il [1; 2; 3]) (il [2; 3]));
    "sym_diff [2; 3] [2; 3; 4] = [4]" @=
      (il [4], S.sym_diff (il [2; 3]) (il [2; 3; 4]));
    "sym_diff [2; 3] [2; 3] = []" @=
      (il [], S.sym_diff (il [2; 3]) (il [2; 3]));
    "sym_diff [2] [] = [2]" @=
      (il [2], S.sym_diff (il [2]) (il []));
    "sym_diff [] [3] = [3]" @=
      (il [3], S.sym_diff (il []) (il [3]));
    ()

  let test_disjoint () =
    "disjoint [1] [1] = false" @?
      (neg2 S.disjoint (il [1]) (il [1]));
    "disjoint [1] [2] = true" @?
      (S.disjoint (il [1]) (il [2]));
    "disjoint [] [2] = true" @?
      (S.disjoint (il []) (il [2]));
    "disjoint [1] [] = true" @?
      (S.disjoint (il [1]) (il []));
    "disjoint [1; 2] [3; 4] = true" @?
      (S.disjoint (il [1; 2]) (il [3; 4]));
    "disjoint [1; 2; 3] [1; 4; 5] = false" @?
      (neg2 S.disjoint (il [1; 2; 3]) (il [1; 4; 5]));
    ()

  let test_find_opt () =
    "find_opt 1 (of_list [1;2;3;4;5;6;7;8])" @?
      ((S.find_opt 1 (il [1;2;3;4;5;6;7;8])) = Some 1);
    "find_opt 1 (of_list [1;2;3;4;5;6;7;8])" @?
      ((S.find_opt 8 (il [1;2;3;4;5;6;7;8])) = Some 8);
    "find_opt 2 (of_list [1])" @?
      ((S.find_opt (2) (il [1])) = None);
    ()
    
  let test_find_first () =
    "find_first (fun x -> x >= 0) s1" @? (S.find_first (fun x -> x >= 0) s1 = 1);
    "find_first (fun x -> x >= 1) s1" @? (S.find_first (fun x -> x >= 1) s1 = 1);
    "find_first (fun x -> x >= 2) s1" @? (S.find_first (fun x -> x >= 2) s1 = 2);
    "find_first (fun x -> x >= 3) s1" @? (S.find_first (fun x -> x >= 3) s1 = 3);
    "find_first (fun x -> x >= 4) s1" @? (try ignore(S.find_first (fun x -> x >= 4) s1); false with Not_found -> true);
    "find_first (fun x -> x >= 3) S.empty" @? (try ignore(S.find_first (fun x -> x >= 3) S.empty); false with Not_found -> true);
    ()

  let test_find_first_opt () =
    "find_first_opt (fun x -> x >= 0) s1" @? (S.find_first_opt (fun x -> x >= 0) s1 = Some 1);
    "find_first_opt (fun x -> x >= 1) s1" @? (S.find_first_opt (fun x -> x >= 1) s1 = Some 1);
    "find_first_opt (fun x -> x >= 2) s1" @? (S.find_first_opt (fun x -> x >= 2) s1 = Some 2);
    "find_first_opt (fun x -> x >= 3) s1" @? (S.find_first_opt (fun x -> x >= 3) s1 = Some 3);
    "find_first_opt (fun x -> x >= 4) s1" @? (S.find_first_opt (fun x -> x >= 4) s1 = None  );
    "find_first_opt (fun x -> x >= 3) S.empty" @? (S.find_first_opt (fun x -> x >= 3) S.empty = None  );
    ()
  
  let test_find_last () =
    "find_last_opt (fun x -> x <= 1) s1" @? (          (S.find_last (fun x -> x <= 1) s1) = 1);
    "find_last_opt (fun x -> x <= 2) s1" @? (          (S.find_last (fun x -> x <= 2) s1) = 2);
    "find_last_opt (fun x -> x <= 3) s1" @? (          (S.find_last (fun x -> x <= 3) s1) = 3);
    "find_last_opt (fun x -> x <= 4) s1" @? (          (S.find_last (fun x -> x <= 4) s1) = 3);
    "find_last_opt (fun x -> x <= 0) s1" @? (try ignore(S.find_last (fun x -> x <= 0) s1); false with Not_found -> true);
    "find_last_opt (fun x -> x <= 3) S.empty" @? (try ignore(S.find_last (fun x -> x <= 3) S.empty); false with Not_found -> true);
    ()

  let test_find_last_opt () =
    "find_last_opt s1 " @? ((S.find_last_opt (fun x -> x <= 0) s1) = None  );
    "find_last_opt s1 " @? ((S.find_last_opt (fun x -> x <= 1) s1) = Some 1);
    "find_last_opt s1 " @? ((S.find_last_opt (fun x -> x <= 2) s1) = Some 2);
    "find_last_opt s1 " @? ((S.find_last_opt (fun x -> x <= 3) s1) = Some 3);
    "find_last_opt s1 " @? ((S.find_last_opt (fun x -> x <= 4) s1) = Some 3);
    "find_last_opt S.empty " @? ((S.find_last_opt (fun x -> x <= 3) S.empty) = None  );
    ()

  let test_add_seq () =
    "add_seq [1;2;3] [3;4]" @= (il [1;2;3;4], S.add_seq (BatSeq.of_list [1;2;3]) (il [3;4]));
    "add_seq [1;2]   [3;4]" @= (il [1;2;3;4], S.add_seq (BatSeq.of_list [1;2])   (il [3;4]));
    "add_seq []      [3;4]" @= (il [3;4],     S.add_seq (BatSeq.of_list [])      (il [3;4]));
    "add_seq [1;2]   []   " @= (il [1;2],     S.add_seq (BatSeq.of_list [1;2])   (il []));
    "add_seq []      []   " @= (il [],        S.add_seq (BatSeq.of_list [])      (il []));
    ()

  let test_of_seq () =
    "of_seq [1;2;3;4]" @= (il [1;2;3;4], S.of_seq (BatSeq.of_list [1;2;4;3]));
    "of_seq []"        @= (il []       , S.of_seq (BatSeq.of_list []));
    ()

  let test_to_seq () = 
    "to_seq [1;2;3;4]" @? (BatSeq.equal (BatSeq.of_list [1;2;3;4]) (S.to_seq (il [4;1;3;2])));
    "to_seq []"        @? (BatSeq.equal (BatSeq.of_list [])        (S.to_seq (il [])));
    ()

  let test_to_rev_seq () =
    "to_rev_seq [1;2;3;4]" @? (BatSeq.equal (BatSeq.of_list [4;3;2;1]) (S.to_rev_seq (il [4;1;3;2])));
    "to_rev_seq []"        @? (BatSeq.equal (BatSeq.of_list [])        (S.to_rev_seq (il [])));
    ()
 
  let test_to_seq_from () =
    "to_seq_from 0 [1;2;3;4]" @? (BatSeq.equal (BatSeq.of_list [1;2;3;4]) (S.to_seq_from 0 (il [4;1;3;2])));
    "to_seq_from 3 [1;2;3;4]" @? (BatSeq.equal (BatSeq.of_list [3;4    ]) (S.to_seq_from 3 (il [4;1;3;2])));
    "to_seq_from 5 [1;2;3;4]" @? (BatSeq.equal (BatSeq.of_list [       ]) (S.to_seq_from 5 (il [4;1;3;2])));
    "to_seq_from 5 []"        @? (BatSeq.equal (BatSeq.of_list [       ]) (S.to_seq_from 5 (il []       )));
    ()

  let test_for_all_exists () =
    let test (msg, for_all) =
      let (@?) str = (@?) (Printf.sprintf "[%s] %s" msg str) in
      "for_all (fun _ -> false) empty" @?
        for_all (fun _ -> false) S.empty;
      "for_all (fun _ -> true) empty" @?
        for_all (fun _ -> true) S.empty;
      let k = 1 in
      "for_all (fun _ -> true) (singleton k)" @?
        for_all (fun _ -> true) (S.singleton k);
      "not (for_all (fun _ -> false) (singleton k))" @?
        not (for_all (fun _ -> false) (S.singleton k));
      "for_all (fun k' -> k = k') (singleton k)" @?
        for_all (fun k' -> k = k') (S.singleton k);
      ()
    in
    let not_not_exists f li =
      not (S.exists (neg f) li) in
    List.iter test
      [ "for_all", S.for_all;
        "not not exists", not_not_exists ]

  let test_print () =
    let test str li =
      let str' =
        BatIO.to_string
          (S.print ~first:"{" ~last:"}" ~sep:", " BatInt.print)
          (il li) in
      U.assert_equal
        ~msg:"printing test"
        ~cmp:(fun x y -> 0 = String.compare x y)
        ~printer:(fun x -> x)
        str' str in
    test "{}" [];
    test "{0}" [0];
    test "{0, 2}" [0; 2];
    ()


  let test_enums () =
    (* test enum, of_enum, backwards *)
    let test_of_enum f name_f t =
      eq ~msg:(Printf.sprintf "of_enum (%s t) = t" name_f)
        BatInt.compare BatInt.print
        (S.of_enum (f t)) t in
    List.iter (fun (f, name_f) ->
      test_of_enum f name_f (il []);
      test_of_enum f name_f (il [0; 4; 2]))
      [
        S.enum, "enum";
        S.backwards, "backwards";
        (fun s -> BatList.enum (S.elements s)), "enum bindings";
      ]

  let test_map () =
    "map (x -> 1) [1;2;3] == [1]" @=
      (S.map (fun _x -> 1) (il [1;2;3]), il [1]);
    "map (x -> x+5) [1;2;3] == [6;7;8]" @=
      (S.map (fun x -> x+5) (il [1;2;3]), il [6;7;8]);
    "map (x->x) [1;2;3] == [1;2;3]" @=
      (S.map (fun x -> x) (il [1;2;3]), il [1;2;3]);
    "map (x->x) [] == []" @=
      (S.map (fun x -> x+1) S.empty, S.empty);
    ()

  let test_map_endo () =
    "map_endo (x -> 1) [1;2;3] == [1]" @=
      (S.map_endo (fun _x -> 1) (il [1;2;3]), il [1]);
    "map_endo (x -> x+5) [1;2;3] == [6;7;8]" @=
      (S.map_endo (fun x -> x+5) (il [1;2;3]), il [6;7;8]);
    "map_endo (x->x) [1;2;3] == [1;2;3] (test phys eq)" @?
      (let s = il [1;2;3] in s == (S.map_endo (fun x -> x) s));
    "map_endo (x->x) [1;2;3] == [1;2;3] (test phys eq)" @?
      (let s = S.empty in s == (S.map_endo (fun x -> x+1) s));
    ()

  let test_filter () =
    "filter (fun x -> x < 10) [1;2;3] (phys eq)" @?
      (let s = il [1;2;3] in s == (S.filter (fun x -> x < 10) s));
    "filter (fun x -> x > 10) [] (phys eq)" @?
      (let s = S.empty in s == (S.filter (fun x -> x > 10) s));
    "filter (fun x -> x > 10) [] (phys eq)" @=
      (S.filter (fun x -> x > 10) (il [0;10;20;30]), il [20;30]);
    ()

  let test_filter_map () =
    "filter_map (fun x -> Some x) [1;2;3]" @=
      (il [1;2;3], S.filter_map (fun x -> Some x) (il [1;2;3]));
    "filter_map (fun x -> Some x) [] (phys eq)" @=
      (S.empty, S.filter_map (fun x -> Some x) S.empty);
    "filter_map (fun x -> if x < 3 then Some (-x) else None) [1;2;3;4] = [-1;-2]" @=
      (S.filter_map (fun x -> if x < 3 then Some (-x) else None) (il [1;2;3;4]), il [-1;-2]);
    ()

  let test_filter_map_endo () =
    "filter_map_endo (fun x -> Some x) [1;2;3] (phys eq)" @?
      (let s = il [1;2;3] in s == (S.filter_map_endo (fun x -> Some x) s));
    "filter_map_endo (fun x -> Some x) [] (phys eq)" @?
      (let s = S.empty    in s == (S.filter_map_endo (fun x -> Some x) s));
    "filter_map_endo (fun x -> if x < 3 then Some (-x) else None) [1;2;3;4] = [-1;-2]" @=
      (S.filter_map_endo (fun x -> if x < 3 then Some (-x) else None) (il [1;2;3;4]), il [-1;-2]);
    ()

  let test_iterators () =
    (* we test all iter/fold/filter in one go, by building a common
       filter implementation (using side-effects for iter). *)

    let from_filter p t =
      li (S.filter p t)
    in

    let from_fold p t =
      let acc e li =
        (if p e then [e] else []) @ li in
      List.rev @@ S.fold acc t []
    in

    let from_iter p t =
      let acc = ref [] in
      S.iter (fun e -> if p e then acc := e :: !acc) t;
      List.rev !acc
    in

    (* I took care to write the input unsorted, to observe potential
       sorting bugs *)
    let t = il [4; 5; 3; 0; 6; 2; 1] in

    (* the predicate which all filteri implementations will use *)
    let p e = (e mod 2 = 0) in

    (* result (in sorted order) *)
    let result = [0; 2; 4; 6] in

    List.iter
      (fun (name, filter_n) ->
        let msg = Printf.sprintf "iterators test : %s" name in
        eq_li ~msg BatInt.compare BatInt.print result (filter_n p t))
      [
        "fold", from_fold;
        "iter", from_iter;
        "filter", from_filter;
      ]

  let tests = [
    "test_is_empty" >:: test_is_empty;
    "test_singleton" >:: test_singleton;
    "test_cardinal" >:: test_cardinal;
    "test_add" >:: test_add;
    "test_remove" >:: test_remove;
    "test_mem" >:: test_mem;
    "test_min_elt" >:: test_min_elt;
    "test_max_elt" >:: test_max_elt;
    "test_choose" >:: test_choose;
    "test_split" >:: test_split;
    "test_partition" >:: test_partition;
    "test_union" >:: test_union;
    "test_inter" >:: test_inter;
    "test_diff" >:: test_diff;
    "test_sym_diff" >:: test_sym_diff;
    "test_disjoint" >:: test_disjoint;
    "test_for_all_exists" >:: test_for_all_exists;
    "test_print" >:: test_print;
    "test_enums" >:: test_enums;
    "test_iterators" >:: test_iterators;
    "test_pop" >:: test_pop;
    "test_find_opt" >:: test_find_opt;
    "test_find_first" >:: test_find_first;
    "test_find_first_opt" >:: test_find_first_opt;
    "test_find_last" >:: test_find_last;
    "test_find_last_opt" >:: test_find_last_opt;
    "test_update" >:: test_update;
    "test_min_elt_opt" >:: test_min_elt_opt;
    "test_max_elt_opt" >:: test_max_elt_opt;
    "test_choose_opt" >:: test_choose_opt;
    "test_add_seq" >:: test_add_seq;
    "test_of_seq" >:: test_of_seq;
    "test_to_seq" >:: test_to_seq;
    "test_to_rev_seq" >:: test_to_rev_seq;
    "test_to_seq_from" >:: test_to_seq_from;
    "test_map" >:: test_map;
    "test_map_endo" >:: test_map_endo;
    "test_filter_map_endo" >:: test_filter_map_endo;
    "test_filter" >:: test_filter;
  ]
end

module S = struct
  include BatSet.Make(BatInt)
  type s = t
  let map_endo = map
  let filter_map_endo = filter_map
end

module P = struct
  module S = BatSet

  type elt = int
  include S
  type s = elt t

  let inter = intersect
end

module TS = TestSet(S)
module TP = TestSet(P)


let tests = "Set" >::: [
  "Subset_compare" >:: test_subset_compare;
  "usual tests on Set.Make" >::: TS.tests;
  "usual tests on PSet" >::: TP.tests;
]
