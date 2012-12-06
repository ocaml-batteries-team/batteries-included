open OUnit

module type Container = sig
  type 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val iter_right : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_right : ('a -> 'b) -> 'a t -> 'b t
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_lefti : (int -> 'acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_right : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val enum : 'a t -> 'a BatEnum.t
  val backwards : 'a t -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> 'a t
  val of_backwards : 'a BatEnum.t -> 'a t
  val length : 'a t -> int
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val exists : ('a -> bool) -> 'a t -> bool
  val for_all : ('a -> bool) -> 'a t -> bool
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val update : 'a t -> int -> ('a -> 'a) -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val last : 'a t -> 'a
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val of_array : 'a array -> 'a t
  val to_array : 'a t -> 'a array
  val of_list_backwards : 'a list -> 'a t
  val to_list_backwards : 'a t -> 'a list
  val cons : 'a t -> 'a -> 'a t
  val snoc : 'a t -> 'a -> 'a t
  val tail : 'a t -> 'a t
  val init : 'a t -> 'a t
  val hd : 'a t -> 'a
  val find : ('a -> bool) -> 'a t -> 'a
  val find_right : ('a -> bool) -> 'a t -> 'a
  val is_empty : _ t -> bool
  val printer_delim : string * string
  val print : ?first:string -> ?last:string -> ?sep:string
    -> ('a BatInnerIO.output -> 'b -> unit)
    -> 'a BatInnerIO.output -> 'b t -> unit
  val invariants : _ t -> unit
  val insert : 'a t -> int -> 'a -> 'a t
  val delete : 'a t -> int -> 'a t
  val delete_range : 'a t -> int -> int -> 'a t
  val build : int -> (int -> 'a) -> 'a t
  val mem : 'a t -> 'a -> bool
  val memq : 'a t -> 'a -> bool
  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  val find_index : ('a -> bool) -> 'a t -> int
  val reduce_left : ('a -> 'a -> 'a) -> 'a t -> 'a
  val make : int -> 'a -> 'a t
(* sort, stable_sort, split_at, iter2, for_all2, take, drop, find_map, reduce, max, min, reverse *)
end

exception NotImplemented

let ni1 = fun _ -> raise NotImplemented
let ni2 = fun _ _ -> raise NotImplemented
let ni3 = fun _ _ _ -> raise NotImplemented
let ni4 = fun _ _ _ _ -> raise NotImplemented
let ni_print ?first:_ ?last:_ ?sep:_ = ni3

module DllistContainer : Container = struct
  include BatDllist
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  and hd = get
  and get l i =
    if i < 0 || i >= length l then raise Exit else get (skip l i)
  and snoc l x = let l = copy l in ignore (append (prev l) x); l
  and cons l x = let l = copy l in prepend l x
  and append l1 l2 = let l1 = copy l1 in let l2 = copy l2 in splice (prev l1) l2; l1
  and init = ni1
  and tail = ni1
  and to_list_backwards = ni1
  and of_list_backwards = ni1
  and last = ni1
  and mapi = ni2
  and iteri = ni2
  and of_backwards = ni1
  and map_right = ni2
  and iter_right = ni2
  and find f t = get (find f t)
  and find_right = ni2
  and is_empty = ni1
  and printer_delim = ("","")
  and set = ni3
  and insert = ni3
  and delete = ni2
  and delete_range = ni3
  and of_array = ni1
  and to_array = ni1
  and update = ni3
  and fold_lefti = ni3
  and build = ni2
  and mem = ni2
  and memq = ni2
  and partition = ni2
  and find_index = ni2
  and reduce_left = ni2
  and make = ni2
end

module ArrayContainer : Container = struct
  include Array
  include BatArray
  let map_right = ni2
  let iter_right = ni2
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  let last = ni1
  let of_list_backwards = ni1
  let to_list_backwards = ni1
  let hd = ni1
  let snoc = ni1
  let cons = ni1
  let take = head
  let drop = tail
  let tail = ni1
  let init = ni1
  and find_right = ni2
  and is_empty = ni1
  and printer_delim = ("[|", "|]")
  and invariants = ignore
  and set t i v = let t = Array.copy t in set t i v; t
  and insert = ni3
  and delete = ni2
  and delete_range = ni3
  and of_array = ni1
  and to_array = ni1
  and update = ni3
  and fold_lefti = ni3
  and build = init
  and mem t x = mem x t
  and memq t x = memq x t
  and partition = ni2
  and find_index = findi
  and reduce_left = ni2
end

module LazyListContainer : Container = struct
  include BatLazyList
  let map_right = ni2
  let iter_right = ni2
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) acc t
  let of_backwards = ni1
  let backwards = ni1
  let get = at
  let to_list_backwards = ni1
  let of_list_backwards = ni1
  let tail = ni1
  let snoc = ni1
  let cons t x = cons x t
  let init = ni1
  and find_right = ni2
  and printer_delim = ("","")
  and invariants = ignore
  and set = ni3
  and insert = ni3
  and delete = ni2
  and delete_range = ni3
  and of_array = ni1
  and to_array = ni1
  and update = ni3
  and fold_lefti = ni3
  and build = init
  and mem t x = mem x t
  and memq t x = memq x t
  and partition = ni2
  and find_index = ni2
  and reduce_left = ni2
end

module DynArrayContainer = struct
  include BatDynArray
  let of_backwards = ni1
  let backwards = ni1
  let iter_right = ni2
  let map_right = ni2
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  let exists = ni2
  let for_all = ni2
  let append t1 t2 = let t1 = copy t1 in append t2 t1; t1
  let to_list_backwards = ni1
  let of_list_backwards = ni1
  let hd = ni1
  let tail = ni1
  let snoc t x = let t = copy t in add t x; t
  let cons = ni2
  let init = ni1
  let find f t = get t (index_of f t)
  and find_right = ni2
  and is_empty = empty
  and printer_delim = ("[|", "|]")
  and set t i v = let t = copy t in set t i v; t
  and insert t i v = let t = copy t in insert t i v; t
  and delete t i = let t = copy t in delete t i; t
  and delete_range t i len = let t = copy t in delete_range t i len; t
  and update = ni3
  and fold_lefti = ni3
  and build = init
  and mem = ni2
  and memq = ni2
  and partition = ni2
  and find_index = ni2
  and reduce_left = ni2
  and make = ni2
end

module DynArrayContainerStepResizer : Container = struct
  include DynArrayContainer
  let of_enum e = (* much simpler to see what happens when resizing code with this
                     and what happens when not resizing with the previous module
                  *)
    let a = of_enum e in
    set_resizer a (step_resizer 1);
    a
end

module DynArrayContainerCrapResizer : Container = struct
  include DynArrayContainer
  let crap_resizer ~currslots:_ ~oldlength:_ ~newlength:_ = -1
  let of_enum e =
    let a = of_enum e in
    set_resizer a crap_resizer;
    a
end

module DequeContainer : Container = struct
  include BatDeque
  let length = size
  and of_backwards = ni1
  and backwards = ni1
  and iter_right = ni2
  and map_right = ni2
  and fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  and exists = ni2
  and for_all = ni2
  and filter = ni2
  and filter_map = ni2
  and get t n =
    let elt1 = at ~backwards:false t n in
    let elt2 = at ~backwards:true t (size t - 1 - n) in
    assert (elt1 = elt2);
    BatOption.get elt1
  and last q = match rear q with None -> raise Exit | Some (_, e) -> e
  and to_list_backwards = ni1
  and of_list_backwards = ni1
  and hd t = match front t with Some (hd, _) -> hd | None -> raise Exit
  and tail t = match front t with Some (_, tl) -> tl | None -> raise Exit
  and init t = match rear t with Some (tl, _) -> tl | None -> raise Exit
  and cons t x = cons x t
  and find f t =
    match find ~backwards:false f t with
    | None -> raise Not_found
    | Some (_, x) -> x
  and find_right f t =
    match find ~backwards:true f t with
    | None -> raise Not_found
    | Some (_, x) -> x
  and append t1 t2 =
    let res1 = append t1 t2 in
    let res2 = append_list t1 (to_list t2) in
    let res3 = prepend_list (to_list t1) t2 in
    assert_equal (to_list res1) (to_list res2);
    assert_equal (to_list res1) (to_list res3);
    res1
  and of_enum e =
    let orig = of_enum (BatEnum.clone e) in
    let n = BatEnum.count e in
    let l = BatList.of_enum e in
    let l1, l2 = BatList.split_at (n / 2) l in
    let q = prepend_list l1 (append_list empty l2) in
    assert_equal (to_list orig) (to_list q);
    q
  and printer_delim = ("[", "]")
  and set = ni3
  and insert = ni3
  and delete = ni2
  and delete_range = ni3
  and of_array = ni1
  and to_array = ni1
  and update = ni3
  and fold_lefti = ni3
  and build = ni2
  and mem = ni2
  and memq = ni2
  and partition = ni2
  and find_index = ni2
  and reduce_left = ni2
  and make = ni2
end

module ListContainer : Container = struct
  include BatList
  let map_right = ni2
  let iter_right = ni2
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  let get = at
  let to_list_backwards = ni1
  let of_list_backwards = ni1
  let of_list = ni1
  let to_list = ni1
  let tail = tl
  let snoc = ni2
  let cons t x = cons x t
  let init = ni1
  and find_right = ni2
  and printer_delim = ("[", "]")
  and invariants = ignore
  and set = ni3
  and insert = ni3
  and delete = ni2
  and delete_range = ni3
  and of_array = ni1
  and to_array = ni1
  and update = ni3
  and fold_lefti = ni3
  and build = init
  and mem t x = mem x t
  and memq t x = memq x t
  and find_index p l = fst (findi (fun _ v -> p v) l)
  and reduce_left = reduce
end

module RefListContainer : Container = struct
  include BatRefList
  let map_right = ni2
  let iter_right = ni2
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  let mapi = ni2
  let iteri = ni2
  let filter f l = let t = of_list (to_list l) in filter f t; t
  let filter_map = ni2
  let get = Index.at_index
  let append = ni2
  let to_list_backwards = ni1
  let of_list_backwards = ni1
  let cons t x = let t = of_list (to_list t) in push t x; t
  let snoc t x = let t = of_list (to_list t) in add t x; t
  let init = ni1
  let tail = tl
  and find_right = ni2
  and printer_delim = ("","")
  and print = ni_print
  and invariants = ignore
  and set = ni3
  and insert = ni3
  and delete = ni2
  and delete_range = ni3
  and of_array = ni1
  and to_array = ni1
  and update = ni3
  and fold_lefti = ni3
  and build = ni2
  and mem = ni2
  and memq = ni2
  and partition = ni2
  and find_index = ni2
  and reduce_left = ni2
  and make = ni2
end

module VectContainer : Container = struct
  include BatVect
  let map_right = ni2
  and iter_right = ni2
  and fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  and append = concat
  and to_list_backwards = ni1
  and of_list_backwards = ni1
  and cons t x = prepend x t
  and snoc t x = append x t
  and hd = first
  and tail t = snd (shift t)
  and init t = snd (pop t)
  and find_right = ni2
  and printer_delim = ("","")
  and insert t i v = insert i (singleton v) t
  and delete = ni2
  and delete_range t i len = remove i len t
  and invariants t = invariants t; invariants (balance t)
 (* so that balance is called without having to test it specifically *)
  and update = modify
  and fold_lefti = foldi
  and set t i v =
    try
      let t' = set t i v in
      let old_v = get t i in
      try
        destructive_set t i v;
        assert (BatEnum.equal (=) (enum t) (enum t'));
        destructive_set t i old_v;
        t'
      with Out_of_bounds -> assert false
    with Out_of_bounds ->
      ignore (destructive_set t i v); assert false
  and build = init
  and mem t x = mem x t
  and memq t x = memq x t
  and find_index = findi
  and reduce_left = reduce
end

module FunctorVectContainer : Container = struct
  include BatVect.Make(struct
    include BatDynArray
    let empty = Obj.magic (create ())
    let rev a =
      let module Array = BatDynArray in
      let n = length a in
      for i = 0 to (n / 2) - 1 do
        let tmp = a.(i) in
        a.(i) <- a.(n - 1 - i);
        a.(n - 1 - i) <- tmp;
      done
    let of_backwards e =
      let a = of_enum e in
      rev a;
      a
    let backwards a =
      rev a;
      let e = enum a in
      BatEnum.force e;
      rev a;
      e
    let concat = function
      | [] -> empty
      | h :: t ->
        let h = copy h in
        List.iter (fun elt -> append elt h) t;
        h
    let append a1 a2 = concat [a1;a2]
    let make i x = init i (fun _ -> x)
  end)(struct let max_height = 18 let leaf_size = 12 end)
  let map_right = ni2
  and iter_right = ni2
  and fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  and append = concat
  and to_list_backwards = ni1
  and of_list_backwards = ni1
  and cons t x = prepend x t
  and snoc t x = append x t
  and hd = first
  and tail t = snd (shift t)
  and init t = snd (pop t)
  and find_right = ni2
  and printer_delim = ("","")
  and insert t i v = insert i (singleton v) t
  and delete = ni2
  and delete_range t i len = remove i len t
  and invariants t = invariants t; invariants (balance t)
  and update = modify
  and fold_lefti = foldi
  and set t i v =
    try
      let t' = set t i v in
      let old_v = get t i in
      try
        destructive_set t i v;
        assert (BatEnum.equal (=) (enum t) (enum t'));
        destructive_set t i old_v;
        t'
      with Out_of_bounds -> assert false
    with Out_of_bounds ->
      ignore (destructive_set t i v); assert false
  and build = init
  and mem t x = mem x t
  and memq t x = memq x t
  and find_index = findi
  and reduce_left = reduce
end

module FingerTreeContainer : Container = struct
  include BatFingerTree
  let length = size
  let mapi = ni2
  let iteri = ni2
  let exists = ni2
  let for_all = ni2
  let filter = ni2
  let filter_map = ni2
  let last = last_exn
  let hd = head_exn
  let init = init_exn
  let tail = tail_exn
  let find = ni2
  and find_right = ni2
  and printer_delim = ("[", "]")
  and insert = ni3
  and delete = ni2
  and delete_range = ni3
  and of_array = ni1
  and to_array = ni1
  and fold_lefti = ni3
  and build = ni2
  and mem = ni2
  and memq = ni2
  and partition = ni2
  and find_index = ni2
  and reduce_left = ni2
  and make = ni2
end

module SeqContainer : Container = struct
  include BatSeq
  let iter_right = ni2
  let map_right = ni2
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  let backwards = ni1
  let rec of_enum e =
    fun () ->
      let e = BatEnum.clone e in
      match BatEnum.get e with
      | None -> nil ()
      | Some v -> Cons (v, of_enum e)
  let of_backwards = ni1
  let mapi = ni2
  let iteri = ni2
  let get = at
  let to_list_backwards = ni1
  let of_list_backwards = ni1
  let of_list = ni1
  let to_list = ni1
  let tail = tl
  let init = ni1
  let snoc = ni1
  let cons t x = cons x t
  let hd e =
    let x = try Some (hd e) with Assert_failure _ as e -> raise e | _ -> None in
    let y = try Some (first e) with Assert_failure _ as e -> raise e | _ -> None in
    assert (x = y);
    match x with None -> raise Exit | Some e -> e
  let find f t = BatOption.get (find f t)
  and find_right = ni2
  and printer_delim = ("[", "]")
  and invariants = ignore
  and set = ni3
  and insert = ni3
  and delete = ni2
  and delete_range = ni3
  and of_array = ni1
  and to_array = ni1
  and update = ni3
  and fold_lefti = ni3
  and build = init
  and memq = ni2
  and partition = ni2
  and find_index = ni2
  and reduce_left = reduce
  and mem t x = mem x t
end

module BatArray = struct
  include BatArray
  let not_countable_enum a =
    let e = enum a in
    BatEnum.from (fun () -> BatEnum.get_exn e)
end

module TestContainer(C : Container) : sig end = struct
  let n = 500
  let a = Array.init n (fun i -> i)
  let rev_a = Array.init n (fun i -> n - 1 - i)
  let c = C.of_enum (BatArray.enum a)
  let rev_c = C.of_enum (BatArray.enum rev_a)
  let inv = C.invariants
  let () = inv c; inv rev_c
  let empty : 'a C.t Lazy.t =
    try
      let s = C.of_enum (BatArray.enum [||]) in
      inv s;
      (* working around a caml bug: lazy [||] segfaults *)
      Obj.magic s
    with BatDllist.Empty ->
      lazy (raise BatDllist.Empty)

  let repeat_twice f =
    try
      (* repeating twice in case the structure mutates itself
         when doing operations on it *)
      f (); f ()
    with NotImplemented ->
      ()

  let () =
    repeat_twice (fun () ->
      assert (C.length c = n);
      try assert_equal 0 (C.length (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      C.iter (fun elt -> incr i; assert (!i = elt)) c;
      assert (!i = n - 1);
      try C.iter (fun _ -> assert false) (Lazy.force empty)
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      C.iteri (fun idx elt -> incr i; assert (!i = idx); assert (!i = elt)) c;
      assert (!i = n - 1);
      try C.iteri (fun _ -> assert false) (Lazy.force empty);
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref n in
      C.iter_right (fun elt -> decr i; assert (!i = elt)) c;
      assert (!i = 0);
      try C.iter_right (fun _ -> assert false) (Lazy.force empty)
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let c = C.build n (fun i -> i + 1) in
      inv c;
      let i = ref (-1) in
      (try C.iter (fun elt -> incr i; assert (!i + 1 = elt)) c;
      with NotImplemented -> failwith "build and not iter??");
      assert (!i = n - 1);
      assert_equal 0 (C.length (C.build 0 (fun _ -> assert false)));
      assert (
        try ignore (C.build (-1) (fun _ -> ())); false
        with Assert_failure _ as e -> raise e | _ -> true
      )
    )

  let () =
    repeat_twice (fun () ->
      let c = C.make n (-42) in
      inv c;
      let i = ref (-1) in
      (try C.iter (fun elt -> incr i; assert (elt = -42)) c;
      with NotImplemented -> failwith "make and not iter??");
      assert (!i = n - 1);
      assert_equal 0 (C.length (C.make 0 (-42)));
      assert (
        try ignore (C.make (-1) (-42)); false
        with Assert_failure _ as e -> raise e | _ -> true
      )
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let c = C.map (fun elt -> incr i; assert (!i = elt); elt + 1) c in
      inv c;
      let i = ref (-1) in
      (try C.iter (fun elt -> incr i; assert (!i + 1 = elt)) c;
      with NotImplemented -> failwith "map and not iter??");
      assert (!i = n - 1);
      try assert_equal 0 (C.length (C.map (fun _ -> assert false) (Lazy.force empty)))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let c = C.mapi (fun idx elt -> incr i; assert (!i = idx); assert (!i = elt); elt + 1) c in
      inv c;
      let i = ref (-1) in
      (try C.iteri (fun idx elt -> incr i; assert (!i = idx); assert (!i + 1 = elt)) c;
      with NotImplemented -> failwith "mapi and not iteri??");
      assert (!i = n - 1);
      try assert_equal 0 (C.length (C.mapi (fun _ -> assert false) (Lazy.force empty)))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref n in
      let c = C.map_right (fun elt -> decr i; assert (!i = elt); elt + 1) c in
      inv c;
      let i = ref n in
      (try C.iter_right (fun elt -> decr i; assert (!i + 1 = elt)) c;
      with NotImplemented -> failwith "map_right and not iter_right??");
      assert (!i = 0);
      try assert_equal 0 (C.length (C.map_right (fun _ -> assert false) (Lazy.force empty)))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let acc = 0 in
      let acc = C.fold_left (fun acc elt -> incr i; assert (!i = elt); acc + 1) acc c in
      assert (!i = n - 1);
      assert (acc = n);
      try ignore (C.fold_left (fun _ -> assert false) 0 (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref 0 in
      let acc = C.reduce_left (fun acc elt -> incr i; assert (acc = !i - 1); assert (!i = elt); acc + 1) c in
      assert (!i = n - 1);
      assert (acc = n - 1);
      try ignore (C.reduce_left (fun _ -> assert false) (Lazy.force empty));
          assert false
      with
      | Assert_failure _ as e -> raise e
      | _ -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let acc = 0 in
      let acc = C.fold_lefti (fun index acc elt -> incr i; assert (!i = elt); assert (!i = index); acc + 1) acc c in
      assert (!i = n - 1);
      assert (acc = n);
      try ignore (C.fold_lefti (fun _ -> assert false) 0 (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref n in
      let acc = 0 in
      let acc = C.fold_right (fun acc elt -> decr i; assert (!i = elt); acc + 1) acc c in
      assert (!i = 0);
      assert (acc = n);
      try ignore (C.fold_right (fun _ -> assert false) 0 (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let e = C.enum c in
      for i = 0 to n / 2 - 1 do
        assert (i = BatEnum.get_exn e)
      done;
      let e' = BatEnum.clone e in
      assert_equal (BatEnum.count e) (BatEnum.count e');
      for i = n / 2 to n - 1 do
        assert (i = BatEnum.get_exn e && i = BatEnum.get_exn e')
      done;
      assert (BatEnum.is_empty e && BatEnum.is_empty e');
      assert (BatEnum.get e = None);
      assert (BatEnum.get e' = None)
    )

  let () =
    repeat_twice (fun () ->
      let e = C.backwards c in
      for i = 0 to n / 2 - 1 do
        assert (n - 1 - i = BatEnum.get_exn e)
      done;
      let e' = BatEnum.clone e in
      assert (BatEnum.count e = BatEnum.count e');
      for i = n / 2 to n - 1 do
        assert (n - 1 - i = BatEnum.get_exn e && n - 1 - i = BatEnum.get_exn e')
      done;
      assert (BatEnum.is_empty e && BatEnum.is_empty e');
      assert (BatEnum.get e = None);
      assert (BatEnum.get e' = None)
    )

  let () =
    repeat_twice (fun () ->
      let c = C.of_enum (BatArray.not_countable_enum a) in
      inv c;
      repeat_twice (fun () -> assert (C.length c = n));
      repeat_twice (fun () ->
        let i = ref (-1) in
        C.iter (fun elt -> incr i; assert (!i = elt)) c;
        assert (!i = n - 1)
      )
    )

  let () =
    repeat_twice (fun () ->
      let c = C.of_backwards (BatArray.enum rev_a) in
      inv c;
      repeat_twice (fun () -> assert (C.length c = n));
      repeat_twice (fun () ->
        let i = ref (-1) in
        C.iter (fun elt -> incr i; assert (!i = elt)) c;
        assert (!i = n - 1)
      )
    )

  let () =
    repeat_twice (fun () ->
      let c = C.of_backwards (BatArray.not_countable_enum rev_a) in
      inv c;
      repeat_twice (fun () -> assert (C.length c = n));
      repeat_twice (fun () ->
        let i = ref (-1) in
        C.iter (fun elt -> incr i; assert (!i = elt)) c;
        assert (!i = n - 1)
      )
    )

  let () =
    repeat_twice (fun () ->
      assert (C.for_all (fun elt -> elt < n) c);
      let i = ref (-1) in
      assert (not (C.for_all (fun elt -> incr i; elt < 200) c));
      assert (!i = 200);
      try ignore (C.for_all (fun _ -> assert false) (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      assert (not (C.exists (fun elt -> not (elt < n)) c));
      let i = ref (-1) in
      assert (C.exists (fun elt -> incr i; not (elt < 200)) c);
      assert (!i = 200);
      try ignore (C.exists (fun _ -> assert false) (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let c2 = C.filter (fun elt -> incr i; if not (elt = !i) then assert false; elt mod 2 = 0) c in
      inv c2;
      let j = ref (-1) in
      C.iter (fun elt -> incr j; assert (!j * 2 = elt)) c2;
      assert (!i = n - 1);
      assert (!j = n / 2 - 1);
      (* iterating first to force the sequence of lazy
         sequence before checking the number of
         elements traversed *)
      try ignore (C.filter (fun _ -> assert false) (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let c2 = C.filter_map (fun elt -> incr i; assert (elt = !i); if elt mod 2 = 0 then Some (-(elt / 2)) else None) c in
      inv c2;
      let j = ref (-1) in
      C.iter (fun elt -> incr j; assert (!j = -elt)) c2;
      assert (!i = n - 1);
      assert (!j = n / 2 - 1);
      try ignore (C.filter_map (fun _ -> assert false) (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let c2, c3 = C.partition (fun elt -> incr i; if not (elt = !i) then assert false; elt mod 3 = 0) c in
      inv c2; inv c3;
      let j = ref (-1) in
      C.iter (fun elt -> incr j; assert (!j * 3 = elt)) c2;
      let k = ref (-1) in
      C.iter (fun elt -> incr k; assert (3 * (!k / 2) + (!k mod 2) + 1 = elt)) c3;
      assert_equal ~printer:string_of_int (n - 1) !i;
      assert_equal ~printer:string_of_int ((n + 2) / 3 - 1) !j;
      assert_equal ~printer:string_of_int n ((!j + 1) + (!k + 1));
      (* iterating first to force the sequence of lazy
         sequence before checking the number of
         elements traversed *)
      try ignore (C.partition (fun _ -> assert false) (Lazy.force empty))
      with BatDllist.Empty -> ()
    )

  let () =
    repeat_twice (fun () ->
      assert (C.last c = n - 1);
      assert (try ignore (C.last (C.of_enum (BatEnum.empty ()))); false with Assert_failure _ as e -> raise e | _ -> true)
    )

  let () =
    repeat_twice (fun () ->
      assert (C.hd c = 0);
      assert (try ignore (C.hd (C.of_enum (BatEnum.empty ()))); false with Assert_failure _ as e -> raise e | _ -> true)
    )

  let () =
    repeat_twice (fun () ->
      let c2 = C.append c rev_c in
      inv c2;
      assert (C.length c2 = n * 2);
      let i = ref (-1) in
      C.iter (fun elt -> incr i; assert (elt = min !i (2 * n - 1 - !i))) c2;
      assert (!i = 2 * n - 1);

      let c2 = C.append c (C.append c c) in
      inv c2;
      assert (C.length c2 = n * 3);
      let i = ref (-1) in
      C.iter (fun elt -> incr i; assert (elt = !i mod n)) c2;
      assert (!i = 3 * n - 1);

      let c2 = C.append (C.append c c) c in
      inv c2;
      assert (C.length c2 = n * 3);
      let i = ref (-1) in
      C.iter (fun elt -> incr i; assert (elt = !i mod n)) c2;
      assert (!i = 3 * n - 1);

      let c2 = C.append c (C.of_enum (BatList.enum [n; n+1])) in
      inv c2;
      let i = ref (-1) in
      C.iter (fun elt -> incr i; assert (elt = !i)) c2;
      assert (!i = n + 1);

      let c2 = C.append (C.of_enum (BatList.enum [-2; -1])) c in
      inv c2;
      let i = ref (-3) in
      C.iter (fun elt -> incr i; assert (elt = !i)) c2;
      assert (!i = n - 1);
    )

  let () =
    repeat_twice (fun () ->
      for i = 0 to n - 1 do
        assert_equal ~printer:string_of_int i (C.get c i)
      done;
      assert (try ignore (C.get c (-1)); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.get c n); false with Assert_failure _ as e -> raise e | _ -> true);
    )

  let () =
    repeat_twice (fun () ->
      for i = 0 to n - 1 do
        let c2 = C.set c i (-1) in
        inv c2;
        let idx = ref (-1) in
        C.iteri (fun j v ->
          incr idx;
          assert_equal j !idx;
          assert_equal ~printer:string_of_int
            (if i = j then (-1) else j) v
        ) c2;
        assert_equal ~printer:string_of_int (n - 1) !idx;
      done;
      assert (try ignore (C.set c (-1) (-1)); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.set c n (-1)); false with Assert_failure _ as e -> raise e | _ -> true);
      (try assert (
        try ignore (C.set (Lazy.force empty) (-1) (-1)); false
        with Assert_failure _ as e -> raise e | _ -> true
       ) with BatDllist.Empty -> ());
      (try assert (
        try ignore (C.set (Lazy.force empty) 0 (-1)); false
        with Assert_failure _ as e -> raise e | _ -> true
       ) with BatDllist.Empty -> ());
    )

  let () =
    repeat_twice (fun () ->
      for i = 0 to n - 1 do
        let c2 = C.update c i (fun i -> -i) in
        inv c2;
        let idx = ref (-1) in
        C.iteri (fun j v ->
          incr idx;
          assert_equal j !idx;
          assert_equal ~printer:string_of_int
            (if i = j then (-i) else j) v
        ) c2;
        assert_equal ~printer:string_of_int (n - 1) !idx;
      done;
      assert (try ignore (C.update c (-1) (fun _ -> -1)); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.update c n (fun _ -> -1)); false with Assert_failure _ as e -> raise e | _ -> true);
      (try assert (
        try ignore (C.update (Lazy.force empty) (-1) (fun _ -> -1)); false
        with Assert_failure _ as e -> raise e | _ -> true
       ) with BatDllist.Empty -> ());
      (try assert (
        try ignore (C.update (Lazy.force empty) 0 (fun _ -> -1)); false
        with Assert_failure _ as e -> raise e | _ -> true
       ) with BatDllist.Empty -> ());
    )

  let () =
    repeat_twice (fun () ->
      for i = 0 to n do
        let c2 = C.insert c i (-1) in
        inv c2;
        let idx = ref (-1) in
        C.iteri (fun j v ->
          incr idx;
          assert_equal j !idx;
          assert_equal ~printer:string_of_int
            (if i = j then (-1) else if i > j then j else j - 1) v
        ) c2;
        assert_equal ~printer:string_of_int n !idx;

        let c3 = C.insert c2 (i / 2) (-2) in
        inv c3;
        let idx = ref (-1) in
        C.iteri (fun j v ->
          incr idx;
          assert_equal j !idx;
          assert_equal ~printer:string_of_int
            (if j < i / 2 then j
             else if j = i / 2 then (-2)
             else if j - 1 < i then j - 1
             else if j - 1 = i then (-1)
             else j - 2
            ) v
        ) c3;
        assert_equal ~printer:string_of_int (n + 1) !idx;
      done;
      assert (try ignore (C.insert c (-1) (-1)); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.insert c (n + 1) (-1)); false with Assert_failure _ as e -> raise e | _ -> true);
    )

  let () =
    repeat_twice (fun () ->
      for i = 0 to n - 1 do
        let c2 = C.delete c i in
        inv c2;
        let idx = ref (-1) in
        C.iteri (fun j v ->
          incr idx;
          assert_equal j !idx;
          assert_equal ~printer:string_of_int
            (if i > j then j else j + 1) v
        ) c2;
        assert_equal ~printer:string_of_int (n - 2) !idx;

        let c3 = C.delete c2 (i / 2) in
        inv c3;
        let idx = ref (-1) in
        C.iteri (fun j v ->
          incr idx;
          assert_equal j !idx;
          assert_equal ~printer:string_of_int
            (if j < i / 2 then j
             else if j + 1 < i then j + 1
             else j + 2) v
        ) c3;
        assert_equal ~printer:string_of_int (n - 3) !idx;
      done;
      assert (try ignore (C.delete c (-1)); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.delete c n); false with Assert_failure _ as e -> raise e | _ -> true);
    )

  let () =
    repeat_twice (fun () ->
      assert (try ignore (C.delete_range c (-1) 1); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.delete_range c n 1); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.delete_range c 0 (-1)); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.delete_range c 1 n); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (C.is_empty (C.delete_range c 0 n));
      (* could check what happens with an empty range *)
      for i = 0 to n / 2 - 1 do
        let start = i in
        let len = min (1 + i * 2) (n - start) in
        let c2 = C.delete_range c start len in
        inv c2;
        let idx = ref (-1) in
        C.iteri (fun j v ->
          incr idx;
          assert_equal j !idx;
          assert_equal ~printer:string_of_int
            (if start > j then j else j + len) v
        ) c2;
        assert_equal ~printer:string_of_int (n - 1 - len) !idx;
      done;
    )

  let () =
    repeat_twice (fun () ->
      assert (C.to_list c = Array.to_list a);
      (try assert (C.to_list (Lazy.force empty) = [])
      with BatDllist.Empty -> ());
    )

  let () =
    repeat_twice (fun () ->
      let c = C.of_list (Array.to_list a) in
      inv c;
      assert (Array.of_list (C.to_list c) = a)
    )

  let () =
    repeat_twice (fun () ->
      let c = C.of_array a in
      inv c;
      assert (C.to_array c = a)
    )

  let () =
    repeat_twice (fun () ->
      assert (C.to_list_backwards c = List.rev (Array.to_list a))
    )

  let () =
    repeat_twice (fun () ->
      let c = C.of_list_backwards (Array.to_list a) in
      inv c;
      assert (Array.of_list (List.rev (C.to_list c)) = a)
    )

  let () =
    repeat_twice (fun () ->
      let c = C.snoc c n in
      inv c;
      assert (C.length c = n + 1);
      let i = ref (-1) in
      C.iter (fun elt -> incr i; assert (!i = elt)) c;
      assert (!i = n)
    )

  let () =
    repeat_twice (fun () ->
      let c = C.cons c (-1) in
      inv c;
      assert (C.length c = n + 1);
      let i = ref (-2) in
      C.iter (fun elt -> incr i; assert (!i = elt)) c;
      assert (!i = n - 1)
    )

  let () =
    repeat_twice (fun () ->
      let c = C.tail c in
      inv c;
      assert (C.length c = n - 1);
      let i = ref 0 in
      C.iter (fun elt -> incr i; assert (!i = elt)) c;
      assert (!i = n - 1);
      assert (
        try ignore (C.tail (C.of_enum (BatEnum.empty ()))); false
        with Assert_failure _ as e -> raise e | _ -> true
      )
    )

  let () =
    repeat_twice (fun () ->
      let c = C.init c in
      inv c;
      assert (C.length c = n - 1);
      let i = ref (-1) in
      C.iter (fun elt -> incr i; assert (!i = elt)) c;
      assert (!i = n - 2);
      assert (
        try ignore (C.init (C.of_enum (BatEnum.empty ()))); false
        with Assert_failure _ as e -> raise e | _ -> true
      )
    )

  let () =
    repeat_twice (fun () ->
      let a = Array.init n (fun i -> (i, i)) in
      let c = C.of_enum (BatArray.enum a) in
      assert (C.mem c (200, 200));

      assert (not (C.mem c (0,1)));
      (try assert (not (C.mem (Lazy.force empty) 0))
       with BatDllist.Empty -> ());
    )

  let () =
    repeat_twice (fun () ->
      let a = Array.init n (fun i -> ref i) in
      let c = C.of_enum (BatArray.enum a) in
      assert (C.memq c a.(200));
      assert (not (C.memq c (ref 200)));

      (try assert (not (C.memq (Lazy.force empty) 0))
       with BatDllist.Empty -> ());
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let elt = C.find (fun elt -> incr i; assert (!i = elt); elt = 200) c in
      assert (elt = 200);
      assert (!i = 200);
      assert (try ignore (C.find (fun _ -> false) c); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.find (fun _ -> assert false) (Lazy.force empty)); false with Assert_failure _ as e -> raise e | _ -> true);
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let index = C.find_index (fun elt -> incr i; assert (!i = elt); elt = 200) c in
      assert (index = 200);
      assert (!i = 200);
      assert (try ignore (C.find_index (fun _ -> false) c); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.find_index (fun _ -> assert false) (Lazy.force empty)); false with Assert_failure _ as e -> raise e | _ -> true);
    )

  let () =
    repeat_twice (fun () ->
      let i = ref n in
      let elt = C.find_right (fun elt -> decr i; assert (!i = elt); elt = 200) c in
      assert (elt = 200);
      assert (!i = 200);
      assert (try ignore (C.find_right (fun _ -> false) c); false with Assert_failure _ as e -> raise e | _ -> true);
      assert (try ignore (C.find_right (fun _ -> assert false) (Lazy.force empty)); false with Assert_failure _ as e -> raise e | _ -> true);
    )

  let () =
    try repeat_twice (fun () ->
      assert_equal true (C.is_empty (C.of_enum (BatList.enum [])));
      assert_equal false (C.is_empty (C.of_enum (BatList.enum [1])));
      assert_equal false (C.is_empty (C.of_enum (BatList.enum [1;2])));
      assert_equal false (C.is_empty (C.of_enum (BatList.enum [1;2;3])));
      assert_equal false (C.is_empty c);
    ) with BatDllist.Empty -> ()

  let () =
    repeat_twice (fun () ->
      let stringify l =
        try
          let c = C.of_enum (BatList.enum l) in
          inv c;
          BatIO.to_string (C.print ~sep:"," ~first:"<" ~last:">" BatInt.print) c
        with BatDllist.Empty -> "<>" in
      assert_equal "<2,4,66>" (stringify [2;4;66]);
      assert_equal "<2>" (stringify [2]);
      assert_equal "<>" (stringify []);
    )


end

let tests = "Container" >::: [
  "List" >:: (fun () -> let module M = TestContainer(ListContainer) in ());
  "RefList" >:: (fun () -> let module M = TestContainer(RefListContainer) in ());
  "Seq" >:: (fun () -> let module M = TestContainer(SeqContainer) in ());
  "Vect" >:: (fun () -> let module M = TestContainer(VectContainer) in ());
  "FunctorVect" >:: (fun () -> let module M = TestContainer(FunctorVectContainer) in ());
  "FingerTree" >:: (fun () -> let module M = TestContainer(FingerTreeContainer) in ());
  "Array" >:: (fun () -> let module M = TestContainer(ArrayContainer) in ());
  "DynArray" >:: (fun () -> let module M = TestContainer(DynArrayContainer) in ());
  "DynArrayStepResizer" >:: (fun () -> let module M = TestContainer(DynArrayContainerStepResizer) in ());
  "DynArrayCrapResizer" >:: (fun () -> let module M = TestContainer(DynArrayContainerCrapResizer) in ());
  "Deque" >:: (fun () -> let module M = TestContainer(DequeContainer) in ());
  "Lazylist" >:: (fun () -> let module M = TestContainer(LazyListContainer) in ());
  "Dllist" >:: (fun () -> let module M = TestContainer(DllistContainer) in ());
]
