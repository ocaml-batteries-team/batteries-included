open OUnit

module type Container = sig
  type 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val iter_right : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_right : ('a -> 'b) -> 'a t -> 'b t
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_right : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val enum : 'a t -> 'a BatEnum.t
  val backwards : 'a t -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> 'a t
  val of_backwards : 'a BatEnum.t -> 'a t
  val length : 'a t -> int
end

exception NotImplemented

module ArrayContainer : Container = struct
  include Array
  include BatArray
  let map_right _ _ = raise NotImplemented
  let iter_right _ _ = raise NotImplemented
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
end

module DynArrayContainer : Container = struct
  include BatDynArray
  let of_backwards _ = raise NotImplemented
  let backwards _ = raise NotImplemented
  let iter_right _ _ = raise NotImplemented
  let map_right _ _ = raise NotImplemented
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
end

module DequeContainer : Container = struct
  include BatDeque
  let length = size
  let of_backwards _ = raise NotImplemented
  let backwards _ = raise NotImplemented
  let iter_right _ _ = raise NotImplemented
  let map_right _ _ = raise NotImplemented
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
end

module ListContainer : Container = struct
  include BatList
  let map_right _ _ = raise NotImplemented
  let iter_right _ _ = raise NotImplemented
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
end

module RefListContainer : Container = struct
  include BatRefList
  let map_right _ _ = raise NotImplemented
  let iter_right _ _ = raise NotImplemented
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
end

module VectContainer : Container = struct
  include BatVect
  let map_right _ _ = raise NotImplemented
  let iter_right _ _ = raise NotImplemented
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
end

module FingerTreeContainer : Container = struct
  include BatFingerTree
  let length = size
end

module SeqContainer : Container = struct
  include BatSeq
  let iter_right _ _ = raise NotImplemented
  let map_right _ _ = raise NotImplemented
  let fold_right f acc t = fold_right (fun acc elt -> f elt acc) t acc
  let backwards _ = raise NotImplemented
  let rec of_enum e =
    fun () ->
      let e = BatEnum.clone e in
      match BatEnum.get e with
      | None -> Nil
      | Some v -> Cons (v, of_enum e)
  let of_backwards _ = raise NotImplemented
end

module TestContainer(C : Container) = struct
  let n = 1000
  let a = Array.init n (fun i -> i)
  let rev_a = Array.init n (fun i -> n - 1 - i)
  let c = C.of_enum (BatArray.enum a)

  let repeat_twice f =
    try
      (* repeating twice in case the structure mutates itself
         when doing operations on it *)
      f (); f ()
    with NotImplemented ->
      ()

  let () =
    repeat_twice (fun () -> assert (C.length c = n))

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      C.iter (fun elt -> incr i; assert (!i = elt)) c;
      assert (!i = n - 1)
    )

  let () =
    repeat_twice (fun () ->
      let i = ref n in
      C.iter_right (fun elt -> decr i; assert (!i = elt)) c;
      assert (!i = 0)
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let c = C.map (fun elt -> incr i; assert (!i = elt); elt + 1) c in
      let i = ref (-1) in
      (try C.iter (fun elt -> incr i; assert (!i + 1 = elt)) c;
      with NotImplemented -> failwith "map and not iter??");
      assert (!i = n - 1)
    )

  let () =
    repeat_twice (fun () ->
      let i = ref n in
      let c = C.map_right (fun elt -> decr i; assert (!i = elt); elt + 1) c in
      let i = ref n in
      (try C.iter_right (fun elt -> decr i; assert (!i + 1 = elt)) c;
      with NotImplemented -> failwith "map_right and not iter_right??");
      assert (!i = 0)
    )

  let () =
    repeat_twice (fun () ->
      let i = ref (-1) in
      let acc = 0 in
      let acc = C.fold_left (fun acc elt -> incr i; assert (!i = elt); acc + 1) acc c in
      assert (!i = n - 1);
      assert (acc = n)
    )

  let () =
    repeat_twice (fun () ->
      let i = ref n in
      let acc = 0 in
      let acc = C.fold_right (fun acc elt -> decr i; assert (!i = elt); acc + 1) acc c in
      assert (!i = 0);
      assert (acc = n)
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
      let c = C.of_backwards (BatArray.enum rev_a) in
      repeat_twice (fun () -> assert (C.length c = n));
      repeat_twice (fun () ->
        let i = ref (-1) in
        C.iter (fun elt -> incr i; assert (!i = elt)) c;
        assert (!i = n - 1)
      )
    )
end

let tests = "Container" >::: [
  "List" >:: (fun () -> let module M = TestContainer(ListContainer) in ());
  "RefList" >:: (fun () -> let module M = TestContainer(RefListContainer) in ());
  "Seq" >:: (fun () -> let module M = TestContainer(SeqContainer) in ());
  "Vect" >:: (fun () -> let module M = TestContainer(VectContainer) in ());
  "FingerTree" >:: (fun () -> let module M = TestContainer(FingerTreeContainer) in ());
  "Array" >:: (fun () -> let module M = TestContainer(ArrayContainer) in ());
  "DynArray" >:: (fun () -> let module M = TestContainer(DynArrayContainer) in ());
  "Deque" >:: (fun () -> let module M = TestContainer(DequeContainer) in ());
]
