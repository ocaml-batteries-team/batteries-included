(* ocamlbuild benchsuite/sequence.native -- snoc_front | tee >(./plot) *)

module type SIG = sig
  type 'a t
  val empty : 'a t
  val cons : 'a t -> 'a -> 'a t
  val front : 'a t -> ('a t * 'a) option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val snoc : 'a t -> 'a -> 'a t
  val rear : 'a t -> ('a t * 'a) option
  val of_enum : 'a BatEnum.t -> 'a t
  val enum : 'a t -> 'a BatEnum.t
  val of_backwards : 'a BatEnum.t -> 'a t
  val backwards : 'a t -> 'a BatEnum.t
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_right : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val reverse : 'a t -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val split_at : 'a t -> int -> 'a t * 'a t
  (* take, drop *)

  val generate_of_enum : 'a BatEnum.t -> 'a t
end

module Vect : SIG =
struct
  type 'a t = 'a BatVect.t
  let empty = BatVect.empty
  let cons t x = BatVect.prepend x t
  let snoc t x = BatVect.append x t
  let map = BatVect.map
  let front t =
    if BatVect.is_empty t then None
    else
      let n = BatVect.length t in
      Some (BatVect.sub t 0 (n - 1), BatVect.get t 0)
  let rear t =
    if BatVect.is_empty t then None
    else
      let n = BatVect.length t in
      Some (BatVect.sub t 1 (n - 1), BatVect.get t (n - 1))
  let of_enum = BatVect.of_enum
  let enum = BatVect.enum
  let of_backwards = BatVect.of_backwards
  let backwards = BatVect.backwards
  let fold_left = BatVect.fold_left
  let fold_right f acc t = BatVect.fold_right (fun acc elt -> f elt acc) t acc
  let reverse _ = assert false
  let get = BatVect.get
  let set = BatVect.set
  let append = BatVect.concat
  let split_at t n =
    (BatVect.sub t 0 n, BatVect.sub t n (BatVect.length t - n))
  let generate_of_enum = of_enum
end

module ListOverflow : SIG with type 'a t = 'a list =
struct
  type 'a t = 'a list
  let empty = []
  let length l =
    let rec aux acc = function
      | [] -> acc
      | _ :: t -> aux (acc + 1) t in
    aux 0 l
  let cons t x = x :: t
  let front = function
    | [] -> None
    | h :: t -> Some (t, h)
  let rec map f = function
    | [] -> []
    | h :: t ->
      let h = f h in
      let t = map f t in
      h :: t
  let rec rev_append l1 l2 =
    match l1 with
    | [] -> l2
    | h1 :: t1 -> rev_append t1 (h1 :: l2)
  let reverse l = rev_append l []
  let rec snoc t x =
    match t with
    | [] -> [x]
    | h :: t -> h :: snoc t x
  let rear = function
    | [] -> None
    | h :: t ->
      let rec aux acc prev = function
        | [] -> Some (reverse acc, prev)
        | h :: t -> aux (h :: acc) h t in
      aux [h] h t
  let rec of_enum e =
    match BatEnum.get e with
    | None -> []
    | Some h -> h :: of_enum e
  let generate_of_enum = of_enum
  let of_backwards e =
    let rec aux acc e =
      match BatEnum.get e with
      | None -> acc
      | Some h -> aux (h :: acc) e in
    aux [] e
  let enum l =
    let rec make lr count =
      BatEnum.make
        ~next:(fun () ->
          match !lr with
          | [] -> raise BatEnum.No_more_elements
          | h :: t ->
            decr count;
            lr := t;
            h
        )
        ~count:(fun () ->
          if !count < 0 then count := length !lr;
          !count
        )
        ~clone:(fun () ->
          make (ref !lr) (ref !count)
        ) in
    make (ref l) (ref (-1))
  let backwards l = enum (reverse l)
  let rec fold_left f acc = function
    | [] -> acc
    | h :: t -> fold_left f (f acc h) t
  let rec fold_right f acc l =
    match l with
    | [] -> acc
    | h :: t -> f (fold_right f acc t) h
  let rec get t i =
    match i, t with
    | _, [] -> invalid_arg "Index out of bounds"
    | 0, h :: _ -> h
    | _, _ :: t -> get t (i - 1)
  let rec set t i v =
    match i, t with
    | _, [] -> invalid_arg "Index out of bounds"
    | 0, h :: t -> v :: t
    | _, h :: t -> h :: set t (i - 1) v
  let rec append l1 l2 =
    match l1 with
    | [] -> l2
    | h :: t -> h :: append t l2
  let split_at l i =
    let rec aux acc i l =
      match i, l with
      | 0, _ -> reverse acc, l
      | _, [] -> invalid_arg "Index out of bounds"
      | _, h :: t -> aux (h :: acc) (i - 1) t in
    aux [] 0 l
end

module ListTail : sig
  include SIG with type 'a t = 'a list
  val map2 : ('a -> 'b) -> 'a list -> 'b list
end =
struct
  type 'a t = 'a list
  let empty = []
  let length l =
    let rec aux acc = function
      | [] -> acc
      | _ :: t -> aux (acc + 1) t in
    aux 0 l
  let cons t x = x :: t
  let front = function
    | [] -> None
    | h :: t -> Some (t, h)
  let rec rev_append l1 l2 =
    match l1 with
    | [] -> l2
    | h1 :: t1 -> rev_append t1 (h1 :: l2)
  let reverse l = rev_append l []
  let map f l =
    let rec aux f acc = function
    | [] -> reverse acc
    | h :: t -> aux f (f h :: acc) t in
    aux f [] l

  (* copy pasted from core lib *)
  let rec count_map ~f l ctr =
    match l with
    | [] -> []
    | [x1] ->
      let f1 = f x1 in
      [f1]
    | [x1; x2] ->
      let f1 = f x1 in
      let f2 = f x2 in
      [f1; f2]
    | [x1; x2; x3] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      [f1; f2; f3]
    | [x1; x2; x3; x4] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      let f5 = f x5 in
      f1 :: f2 :: f3 :: f4 :: f5 ::
        (if ctr > 1000
         then map f tl
         else count_map ~f tl (ctr + 1))
  let map2 f l = count_map ~f l 0

  let snoc t x =
    let rec aux x acc = function
      | [] -> reverse (x :: acc)
      | h :: t -> aux x (h :: acc) t in
    aux x [] t
  let rear = function
    | [] -> None
    | h :: t ->
      let rec aux acc prev = function
        | [] -> Some (reverse acc, prev)
        | h :: t -> aux (h :: acc) h t in
      aux [h] h t
  let of_backwards e =
    let rec aux acc e =
      match BatEnum.get e with
      | None -> acc
      | Some h -> aux (h :: acc) e in
    aux [] e
  let of_enum e = reverse (of_backwards e)
  let generate_of_enum = of_enum
  let enum l =
    let rec make lr count =
      BatEnum.make
        ~next:(fun () ->
          match !lr with
          | [] -> raise BatEnum.No_more_elements
          | h :: t ->
            decr count;
            lr := t;
            h
        )
        ~count:(fun () ->
          if !count < 0 then count := length !lr;
          !count
        )
        ~clone:(fun () ->
          make (ref !lr) (ref !count)
        ) in
    make (ref l) (ref (-1))
  let backwards l = enum (reverse l)
  let rec fold_left f acc = function
    | [] -> acc
    | h :: t -> fold_left f (f acc h) t
  let rec fold_right f acc l =
    fold_left f acc (reverse l)
  let rec get t i =
    match i, t with
    | _, [] -> invalid_arg "Index out of bounds"
    | 0, h :: _ -> h
    | _, _ :: t -> get t (i - 1)
  let set t i v =
    let rec aux i v acc t =
      match i, t with
      | _, [] -> invalid_arg "Index out of bounds"
      | 0, h :: t -> rev_append acc (v :: t)
      | _, h :: t -> aux (i - 1) v (h :: acc) t in
    aux i v [] t
  let append l1 l2 =
    rev_append (reverse l1) l2
  let split_at l i =
    let rec aux acc i l =
      match i, l with
      | 0, _ -> reverse acc, l
      | _, [] -> invalid_arg "Index out of bounds"
      | _, h :: t -> aux (h :: acc) (i - 1) t in
    aux [] 0 l
end

module ListTailModCons : sig
  include SIG with type 'a t = 'a list
  val map2 : ('a -> 'b) -> 'a list -> 'b list
end = struct
  type 'a t = 'a BatList.t
  let empty = []
  let cons t x = x :: t
  let snoc t x = BatList.append t [x]
  let map = BatList.map

  let set_tail (l : 'a list) (v : 'a list) =
    Obj.set_field (Obj.repr l) 1 (Obj.repr v)
  let map2 f = function
    | [] -> []
    | h :: t ->
      let rec loop f dst = function
        | [] -> ()
        | [a] ->
          let a = f a in
          set_tail dst (a :: [])
        | [a; b] ->
          let a = f a in
          let b = f b in
          set_tail dst (a :: b :: [])
        | [a; b; c] ->
          let a = f a in
          let b = f b in
          let c = f c in
          set_tail dst (a :: b :: c :: [])
        | [a; b; c; d] ->
          let a = f a in
          let b = f b in
          let c = f c in
          let d = f d in
          set_tail dst (a :: b :: c :: d :: [])
        | [a; b; c; d; e] ->
          let a = f a in
          let b = f b in
          let c = f c in
          let d = f d in
          let e = f e in
          set_tail dst (a :: b :: c :: d :: e :: [])
        | a :: b :: c :: d :: e :: t ->
          let a = f a in
          let b = f b in
          let c = f c in
          let d = f d in
          let e = f e in
          let last = e :: [] in
          set_tail dst (a :: b :: c :: d :: last);
          loop f last t in
      let r = f h :: [] in
      loop f r t;
      Obj.magic r

  let rec count_map ~f l ctr =
    match l with
    | [] -> []
    | [x1] ->
      let f1 = f x1 in
      [f1]
    | [x1; x2] ->
      let f1 = f x1 in
      let f2 = f x2 in
      [f1; f2]
    | [x1; x2; x3] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      [f1; f2; f3]
    | [x1; x2; x3; x4] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      let f5 = f x5 in
      f1 :: f2 :: f3 :: f4 :: f5 ::
        (if ctr > 1000
         then map2 f tl
         else count_map ~f tl (ctr + 1))

  let map2 f l =
    count_map ~f l 0

  let append = BatList.append
  let get = BatList.nth
  let split_at i t = BatList.split_at t i
  let set t i v = (* FIXME *)
    let l1, l2 = split_at t i in
    match l2 with
    | [] -> invalid_arg "aze"
    | _ :: t -> append l1 (v :: t)
  let reverse = BatList.rev
  let fold_left = BatList.fold_left
  let fold_right f init l =
    let rec tail_loop acc = function
      | [] -> acc
      | h :: t -> tail_loop (f acc h) t
    in
    let rec loop n = function
      | [] -> init
      | h :: t ->
        if n < 1000 then
          f (loop (n+1) t) h
        else
          f (tail_loop init (reverse t)) h
    in
    loop 0 l
  let enum = BatList.enum
  let backwards = BatList.backwards
  let of_enum = BatList.of_enum
  let generate_of_enum = of_enum
  let of_backwards = BatList.of_backwards
  let front = function
    | [] -> None
    | h :: t -> Some (t, h)
  let rear = function (*FIXME*)
    | [] -> None
    | h :: t ->
      let rec aux acc prev = function
        | [] -> Some (reverse acc, prev)
        | h :: t -> aux (h :: acc) h t in
      aux [h] h t
end

module Deque : SIG =
struct
  type 'a t = {front : 'a list; len : int; rear : 'a list}
  let empty = {front = []; rear = []; len = 0}
  let split_at _ _ = assert false
  let append _ _ = assert false
  let set _ _ _ = assert false
  let get _ _ = assert false
  let reverse {front; len; rear} = {front = rear; rear = front; len}
  let fold_left f acc {front; rear; len = _} =
    let acc = ListTailModCons.fold_left f acc front in
    ListTailModCons.fold_right f acc rear
  let fold_right f acc {front; rear; len = _} =
    let acc = ListTailModCons.fold_left f acc rear in
    ListTailModCons.fold_right f acc front
  let enum {front; rear} =
    BatEnum.append (ListTailModCons.enum front) (ListTailModCons.backwards rear)
  let backwards {front; rear} =
    BatEnum.append (ListTailModCons.enum rear) (ListTailModCons.backwards front)
  let of_enum e =
    let l = ListTailModCons.of_backwards e in
    {front = []; rear = l; len = List.length l}
  let of_backwards e =
    let l = ListTailModCons.of_backwards e in
    {front = l; rear = []; len = List.length l}

  let front q =
    match q with
    | {front = h :: front; len = len} ->
      Some ({ q with front = front ; len = len - 1 }, h)
    | {rear = rear; len = len} ->
      let rear, rev_front = BatList.split_at (len / 2) rear in
      let front = List.rev rev_front in
      match front with
      | [] -> None
      | h :: t ->
        Some ({ front = t ;
                len = len - 1 ;
                rear = rear ;
              }, h)

  let rear q =
    match q with
    | {rear = h :: rear; len = len} ->
      Some ({ q with rear = rear ; len = len - 1 }, h)
    | {front = front; len = len} ->
      let front, rev_rear = BatList.split_at (len / 2) front in
      let rear = List.rev rev_rear in
      match rear with
      | [] -> None
      | h :: t ->
        Some ({ rear = t ;
                len = len - 1 ;
                front = front ;
              }, h)

  let cons {front; len; rear} x =
    {front = x :: front; len = len + 1; rear = rear}
  let snoc {front; len; rear} x =
    {front = front; len = len + 1; rear = x :: rear}
  let map f {front; rear; len} =
    let front = ListTailModCons.map f front in
    let rear = List.rev (ListTailModCons.map f (List.rev rear)) in
    {front; rear; len}

  let generate_of_enum e =
    let l = of_enum e in
    match front l with
    | None -> l
    | Some (t, x) -> cons t x
end

module GenFingerTree = struct
  type 'a monoid = {
    zero : 'a;
    combine : 'a -> 'a -> 'a ;
  }
  exception Empty
  type ('a, 'm) node =
    | Node2 of 'm * 'a * 'a
    | Node3 of 'm * 'a * 'a * 'a
  type ('a, 'm) digit =
    | One of 'm * 'a
    | Two of 'm * 'a * 'a
    | Three of 'm * 'a * 'a * 'a
    | Four of 'm * 'a * 'a * 'a * 'a
  type ('a, 'm) fg =
    | Nil
    | Single of 'a
    | Deep of 'm * ('a, 'm) digit * (('a, 'm) node, 'm) fg * ('a, 'm) digit

  let empty = Nil
  let singleton a = Single a

  let is_empty = function
    | Nil -> true
    | Single _ | Deep _ -> false

  let fold_right_node f acc = function
    | Node2 (_, a, b) -> f (f acc b) a
    | Node3 (_, a, b, c) -> f (f (f acc c) b) a
  let fold_left_node f acc = function
    | Node2 (_, a, b) -> f (f acc a) b
    | Node3 (_, a, b, c) -> f (f (f acc a) b) c

  let fold_right_digit f acc = function
    | One (_, a) -> f acc a
    | Two (_, a, b) -> f (f acc b) a
    | Three (_, a, b, c) -> f (f (f acc c) b) a
    | Four (_, a, b, c, d) -> f (f (f (f acc d) c) b) a
  let fold_left_digit f acc = function
    | One (_, a) -> f acc a
    | Two (_, a, b) -> f (f acc a) b
    | Three (_, a, b, c) -> f (f (f acc a) b) c
    | Four (_, a, b, c, d) -> f (f (f (f acc a) b) c) d

  let rec fold_right : 'acc 'a 'm. ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc = fun f acc -> function
    | Nil -> acc
    | Single x -> f acc x
    | Deep (_, pr, m, sf) ->
      let acc = fold_right_digit f acc sf in
      let acc = fold_right (fun acc elt -> fold_right_node f acc elt) acc m in
      let acc = fold_right_digit f acc pr in
      acc
  let rec fold_left : 'acc 'a 'm. ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc = fun f acc -> function
    | Nil -> acc
    | Single x -> f acc x
    | Deep (_, pr, m, sf) ->
      let acc = fold_left_digit f acc pr in
      let acc = fold_left (fun acc elt -> fold_left_node f acc elt) acc m in
      let acc = fold_left_digit f acc sf in
      acc
  type ('wrapped_type, 'a, 'm) wrap = monoid:'m monoid -> measure:('a -> 'm) -> 'wrapped_type
  let measure_node = function
    | Node2 (v, _, _)
    | Node3 (v, _, _, _) -> v

  let measure_digit = function
    | One (v, _)
    | Two (v, _, _)
    | Three (v, _, _, _)
    | Four (v, _, _, _, _) -> v

  let measure_t_node ~monoid = function
    | Nil -> monoid.zero
    | Single x -> measure_node x
    | Deep (v, _, _, _) -> v
  let measure_t ~monoid ~measure = function
    | Nil -> monoid.zero
    | Single x -> measure x
    | Deep (v, _, _, _) -> v

  let node2 ~monoid ~measure a b =
    Node2 (monoid.combine (measure a) (measure b), a, b)
  let node2_node ~monoid a b =
    Node2 (monoid.combine (measure_node a) (measure_node b), a, b)

  let node3 ~monoid ~measure a b c =
    Node3 (monoid.combine (measure a) (monoid.combine (measure b) (measure c)), a, b, c)
  let node3_node ~monoid a b c =
    Node3 (monoid.combine (measure_node a) (monoid.combine (measure_node b) (measure_node c)), a, b, c)

  let deep ~monoid pr m sf =
    let v = measure_digit pr in
    let v = monoid.combine v (measure_t_node ~monoid m) in
    let v = monoid.combine v (measure_digit sf) in
    Deep (v, pr, m, sf)

  let one_node a =
    One (measure_node a, a)
  let one ~measure a =
    One (measure a, a)

  let two_node ~monoid a b =
    Two (monoid.combine (measure_node a) (measure_node b), a, b)
  let two ~monoid ~measure a b =
    Two (monoid.combine (measure a) (measure b), a, b)

  let three_node ~monoid a b c =
    Three (monoid.combine (monoid.combine (measure_node a) (measure_node b)) (measure_node c), a, b, c)
  let three ~monoid ~measure a b c =
    Three (monoid.combine (monoid.combine (measure a) (measure b)) (measure c), a, b, c)

  let four_node ~monoid a b c d =
    Four (monoid.combine (monoid.combine (measure_node a) (measure_node b)) (monoid.combine (measure_node c) (measure_node d)), a, b, c, d)
  let four ~monoid ~measure a b c d =
    Four (monoid.combine (monoid.combine (measure a) (measure b)) (monoid.combine (measure c) (measure d)), a, b, c, d)

  let cons_digit_node ~monoid d x =
    match d with
    | One (v, a) -> Two (monoid.combine (measure_node x) v, x, a)
    | Two (v, a, b) -> Three (monoid.combine (measure_node x) v, x, a, b)
    | Three (v, a, b, c) -> Four (monoid.combine (measure_node x) v, x, a, b, c)
    | Four _ -> assert false
  let cons_digit ~monoid ~measure d x =
    match d with
    | One (v, a) -> Two (monoid.combine (measure x) v, x, a)
    | Two (v, a, b) -> Three (monoid.combine (measure x) v, x, a, b)
    | Three (v, a, b, c) -> Four (monoid.combine (measure x) v, x, a, b, c)
    | Four _ -> assert false

  let snoc_digit_node ~monoid d x =
    match d with
    | One (v, a) -> Two (monoid.combine v (measure_node x), a, x)
    | Two (v, a, b) -> Three (monoid.combine v (measure_node x), a, b, x)
    | Three (v, a, b, c) -> Four (monoid.combine v (measure_node x), a, b, c, x)
    | Four _ -> assert false
  let snoc_digit ~monoid ~measure d x =
    match d with
    | One (v, a) -> Two (monoid.combine v (measure x), a, x)
    | Two (v, a, b) -> Three (monoid.combine v (measure x), a, b, x)
    | Three (v, a, b, c) -> Four (monoid.combine v (measure x), a, b, c, x)
    | Four _ -> assert false

  let rec cons_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> ('a, 'm) node -> (('a, 'm) node, 'm) fg =
    fun ~monoid t a ->
    match t with
    | Nil ->
      Single a
    | Single b ->
      deep ~monoid (one_node a) Nil (one_node b)
    | Deep (_, Four (_, b, c, d, e), m, sf) ->
      deep ~monoid (two_node ~monoid a b) (cons_aux ~monoid m (node3_node ~monoid c d e)) sf
    | Deep (v, pr, m, sf) ->
      Deep (monoid.combine (measure_node a) v, cons_digit_node ~monoid pr a, m, sf)
  let cons ~monoid ~measure t a =
    match t with
    | Nil ->
      Single a
    | Single b ->
      deep ~monoid (one measure a) Nil (one measure b)
    | Deep (_, Four (_, b, c, d, e), m, sf) ->
      deep ~monoid (two ~monoid ~measure a b) (cons_aux ~monoid m (node3 ~monoid ~measure c d e)) sf
    | Deep (v, pr, m, sf) ->
      Deep (monoid.combine (measure a) v, cons_digit ~monoid ~measure pr a, m, sf)

  let rec snoc_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> ('a, 'm) node -> (('a, 'm) node, 'm) fg =
    fun ~monoid t a ->
    match t with
    | Nil ->
      Single a
    | Single b ->
      deep ~monoid (one_node b) Nil (one_node a)
    | Deep (_, pr, m, Four (_, b, c, d, e)) ->
      deep ~monoid pr (snoc_aux ~monoid m (node3_node ~monoid b c d)) (two_node ~monoid e a)
    | Deep (v, pr, m, sf) ->
      Deep (monoid.combine v (measure_node a), pr, m, snoc_digit_node ~monoid sf a)
  let snoc ~monoid ~measure t a =
    match t with
    | Nil ->
      Single a
    | Single b ->
      deep ~monoid (one ~measure b) Nil (one ~measure a)
    | Deep (_, pr, m, Four (_, b, c, d, e)) ->
      deep ~monoid pr (snoc_aux ~monoid m (node3 ~monoid ~measure b c d)) (two ~measure ~monoid e a)
    | Deep (v, pr, m, sf) ->
      Deep (monoid.combine v (measure a), pr, m, snoc_digit ~monoid ~measure sf a)
  let to_tree_digit_node ~monoid d =
    match d with
    | One (_, a) -> Single a
    | Two (v, a, b) -> Deep (v, one_node a, Nil, one_node b)
    | Three (v, a, b, c) -> Deep (v, two_node ~monoid a b, Nil, one_node c)
    | Four (v, a, b, c, d) -> Deep (v, three_node ~monoid a b c, Nil, one_node d)
  let to_tree_digit ~monoid ~measure d =
    match d with
    | One (_, a) -> Single a
    | Two (v, a, b) -> Deep (v, one ~measure a, Nil, one ~measure b)
    | Three (v, a, b, c) -> Deep (v, two ~monoid ~measure a b, Nil, one ~measure c)
    | Four (v, a, b, c, d) -> Deep (v, three ~monoid ~measure a b c, Nil, one ~measure d)
  let to_tree_list ~monoid ~measure = function
    | [] -> Nil
    | [a] -> Single a
    | [a; b] -> deep ~monoid (one ~measure a) Nil (one ~measure b)
    | [a; b; c] -> deep ~monoid (two ~monoid ~measure a b) Nil (one ~measure c)
    | [a; b; c; d] -> deep ~monoid (three ~monoid ~measure a b c) Nil (one ~measure d)
    | _ -> assert false

  let to_digit_node = function
    | Node2 (v, a, b) -> Two (v, a, b)
    | Node3 (v, a, b, c) -> Three (v, a, b, c)
  let to_digit_list ~monoid ~measure = function
    | [a] -> one ~measure a
    | [a; b] -> two ~monoid ~measure a b
    | [a; b; c] -> three ~monoid ~measure a b c
    | [a; b; c; d] -> four ~monoid ~measure a b c d
    | _ -> assert false
  let to_digit_list_node ~monoid = function
    | [a] -> one_node a
    | [a; b] -> two_node ~monoid a b
    | [a; b; c] -> three_node ~monoid a b c
    | [a; b; c; d] -> four_node ~monoid a b c d
    | _ -> assert false

  let head_digit = function
    | One (_, a)
    | Two (_, a, _)
    | Three (_, a, _, _)
    | Four (_, a, _, _, _) -> a
  let last_digit = function
    | One (_, a)
    | Two (_, _, a)
    | Three (_, _, _, a)
    | Four (_, _, _, _, a) -> a
  let tail_digit_node ~monoid = function
    | One _ -> assert false
    | Two (_, _, a) -> one_node a
    | Three (_, _, a, b) -> two_node ~monoid a b
    | Four (_, _, a, b, c) -> three_node ~monoid a b c
  let tail_digit ~monoid ~measure = function
    | One _ -> assert false
    | Two (_, _, a) -> one ~measure a
    | Three (_, _, a, b) -> two ~monoid ~measure a b
    | Four (_, _, a, b, c) -> three ~monoid ~measure a b c
  let init_digit_node ~monoid = function
    | One _ -> assert false
    | Two (_, a, _) -> one_node a
    | Three (_, a, b, _) -> two_node ~monoid a b
    | Four (_, a, b, c, _) -> three_node ~monoid a b c
  let init_digit ~monoid ~measure = function
    | One _ -> assert false
    | Two (_, a, _) -> one ~measure a
    | Three (_, a, b, _) -> two ~monoid ~measure a b
    | Four (_, a, b, c, _) -> three ~monoid ~measure a b c

  type ('a, 'rest) view =
    | Vnil
    | Vcons of 'a * 'rest

  let rec view_left_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, (('a, 'm) node, 'm) fg) view =
    fun ~monoid -> function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match view_left_aux ~monoid m with
        | Vnil -> to_tree_digit_node ~monoid sf
        | Vcons (a, m') -> deep ~monoid (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid (tail_digit_node ~monoid pr) m sf in
      Vcons (head_digit pr, vcons)
  let view_left ~monoid ~measure = function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match view_left_aux ~monoid m with
        | Vnil -> to_tree_digit ~monoid ~measure sf
        | Vcons (a, m') -> deep ~monoid (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid (tail_digit ~monoid ~measure pr) m sf in
      Vcons (head_digit pr, vcons)

  let rec view_right_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, (('a, 'm) node, 'm) fg) view =
    fun ~monoid -> function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match view_right_aux ~monoid m with
        | Vnil -> to_tree_digit_node ~monoid pr
        | Vcons (a, m') -> deep ~monoid pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid pr m (init_digit_node ~monoid sf) in
      Vcons (last_digit sf, vcons)
  let view_right ~monoid ~measure = function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match view_right_aux ~monoid m with
        | Vnil -> to_tree_digit ~monoid ~measure pr
        | Vcons (a, m') -> deep ~monoid pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid pr m (init_digit ~monoid ~measure sf) in
      Vcons (last_digit sf, vcons)

  let head_exn = function
    | Nil -> raise Empty
    | Single a -> a
    | Deep (_, pr, _, _) -> head_digit pr
  let head = function
    | Nil -> None
    | Single a -> Some a
    | Deep (_, pr, _, _) -> Some (head_digit pr)

  let last_exn = function
    | Nil -> raise Empty
    | Single a -> a
    | Deep (_, _, _, sf) -> last_digit sf
  let last = function
    | Nil -> None
    | Single a -> Some a
    | Deep (_, _, _, sf) -> Some (last_digit sf)

  let tail ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let tail_exn ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> tl

  let front ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (tl, hd)
  let front_exn ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> (tl, hd)

  let init ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let init_exn ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> tl

  let rear ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (tl, hd)
  let rear_exn ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> (tl, hd)

  let nodes =
    let add_digit_to digit l =
      match digit with
      | One (_, a) -> a :: l
      | Two (_, a, b) -> a :: b :: l
      | Three (_, a, b, c) -> a :: b :: c :: l
      | Four (_, a, b, c, d) -> a :: b :: c :: d :: l in

    let rec nodes_aux ~monoid ~measure ts sf2 =
      match ts, sf2 with
      | [], One _ -> assert false
      | [], Two (_, a, b)
      | [a], One (_, b) -> [node2 ~monoid ~measure a b]
      | [], Three (_, a, b, c)
      | [a], Two (_, b, c)
      | [a; b], One (_, c) -> [node3 ~monoid ~measure a b c]
      | [], Four (_, a, b, c, d)
      | [a], Three (_, b, c, d)
      | [a; b], Two (_, c, d)
      | [a; b; c], One (_, d) -> [node2 ~monoid ~measure a b; node2 ~monoid ~measure c d]
      | a :: b :: c :: ts, _ -> node3 ~monoid ~measure a b c :: nodes_aux ~monoid ~measure ts sf2
      | [a], Four (_, b, c, d, e)
      | [a; b], Three (_, c, d, e) -> [node3 ~monoid ~measure a b c; node2 ~monoid ~measure d e]
      | [a; b], Four (_, c, d, e, f) -> [node3 ~monoid ~measure a b c; node3 ~monoid ~measure d e f] in

    fun ~monoid ~measure sf1 ts sf2 ->
      let ts = add_digit_to sf1 ts in
      nodes_aux ~monoid ~measure ts sf2

  let rec app3 : 'a 'm.
      monoid:'m monoid -> measure:('a -> 'm) -> ('a, 'm) fg -> 'a list -> ('a, 'm) fg -> ('a, 'm) fg =
    fun ~monoid ~measure t1 elts t2 ->
    match t1, t2 with
    | Nil, _ ->
      List.fold_right (fun elt acc -> cons ~monoid ~measure acc elt) elts t2
    | _, Nil ->
      List.fold_left (fun acc elt -> snoc ~monoid ~measure acc elt) t1 elts
    | Single x1, _ ->
      cons ~monoid ~measure (List.fold_right (fun elt acc -> cons ~monoid ~measure acc elt) elts t2) x1
    | _, Single x2 ->
      snoc ~monoid ~measure (List.fold_left (fun acc elt -> snoc ~monoid ~measure acc elt) t1 elts) x2
    | Deep (_, pr1, m1, sf1), Deep (_, pr2, m2, sf2) ->
      deep ~monoid pr1 (app3 ~monoid ~measure:measure_node m1 (nodes ~monoid ~measure sf1 elts pr2) m2) sf2

  let append ~monoid ~measure t1 t2 = app3 ~monoid ~measure t1 [] t2

  let reverse_digit_node ~monoid rev_a = function
    | One (_, a) -> one_node (rev_a a)
    | Two (_, a, b) -> two_node ~monoid (rev_a b) (rev_a a)
    | Three (_, a, b, c) -> three_node ~monoid (rev_a c) (rev_a b) (rev_a a)
    | Four (_, a, b, c, d) -> four_node ~monoid (rev_a d) (rev_a c) (rev_a b) (rev_a a)
  let reverse_digit ~monoid ~measure = function
    | One _ as d -> d
    | Two (_, a, b) -> two ~monoid ~measure b a
    | Three (_, a, b, c) -> three ~monoid ~measure c b a
    | Four (_, a, b, c, d) -> four ~monoid ~measure d c b a
  let reverse_node_node ~monoid rev_a = function
    | Node2 (_, a, b) -> node2_node ~monoid (rev_a b) (rev_a a)
    | Node3 (_, a, b, c) -> node3_node ~monoid (rev_a c) (rev_a b) (rev_a a)
  let reverse_node ~monoid ~measure = function
    | Node2 (_, a, b) -> node2 ~monoid ~measure b a
    | Node3 (_, a, b, c) -> node3 ~monoid ~measure c b a

  let rec reverse_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node -> ('a, 'm) node) -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, 'm) fg =
    fun ~monoid reverse_a -> function
    | Nil -> Nil
    | Single a -> Single (reverse_a a)
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit_node ~monoid reverse_a pr in
      let rev_sf = reverse_digit_node ~monoid reverse_a sf in
      let rev_m = reverse_aux ~monoid (reverse_node_node ~monoid (reverse_a)) m in
      deep ~monoid rev_sf rev_m rev_pr
  let reverse ~monoid ~measure = function
    | Nil
    | Single _ as t -> t
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit ~monoid ~measure pr in
      let rev_sf = reverse_digit ~monoid ~measure sf in
      let rev_m = reverse_aux ~monoid (reverse_node ~monoid ~measure) m in
      deep ~monoid rev_sf rev_m rev_pr

  type ('a, 'rest) split = Split of 'rest * 'a * 'rest
  let split_digit ~monoid ~measure p i = function
    | One (_, a) -> Split ([], a, [])
    | Two (_, a, b) ->
      let i' = monoid.combine i (measure a) in
      if p i' then Split ([], a, [b]) else
        Split ([a], b, [])
    | Three (_, a, b, c) ->
      let i' = monoid.combine i (measure a) in
      if p i' then Split ([], a, [b; c]) else
        let i'' = monoid.combine i' (measure b) in
        if p i'' then Split ([a], b, [c]) else
          Split ([a; b], c, [])
    | Four (_, a, b, c, d) ->
      let i' = monoid.combine i (measure a) in
      if p i' then Split ([], a, [b; c; d]) else
        let i'' = monoid.combine i' (measure b) in
        if p i'' then Split ([a], b, [c; d]) else
          let i''' = monoid.combine i'' (measure c) in
          if p i''' then Split ([a; b], c, [d]) else
            Split ([a; b; c], d, [])

  let deep_left ~monoid ~measure pr m sf =
    match pr with
    | [] -> (
      match view_left ~monoid ~measure:measure_node m with
      | Vnil -> to_tree_digit ~monoid ~measure sf
      | Vcons (a, m') -> deep ~monoid (to_digit_node a) m' sf
    )
    | _ ->
      deep ~monoid (to_digit_list ~monoid ~measure pr) m sf
  let deep_right ~monoid ~measure pr m sf =
    match sf with
    | [] -> (
      match view_right ~monoid ~measure:measure_node m with
      | Vnil -> to_tree_digit ~monoid ~measure pr
      | Vcons (a, m') -> deep ~monoid pr m' (to_digit_node a)
    )
    | _ ->
      deep ~monoid pr m (to_digit_list ~monoid ~measure sf)

  let rec split_tree : 'a 'm.
      monoid:'m monoid -> measure:('a -> 'm) -> ('m -> bool) -> 'm -> ('a, 'm) fg -> ('a, ('a, 'm) fg) split =
    fun ~monoid ~measure p i -> function
    | Nil -> raise Empty
    | Single x -> Split (Nil, x, Nil)
    | Deep (_, pr, m, sf) ->
      let vpr = monoid.combine i (measure_digit pr) in
      if p vpr then
        let Split (l, x, r) = split_digit ~monoid ~measure p i pr in
        Split (to_tree_list ~monoid ~measure l, x, deep_left ~monoid ~measure r m sf)
      else
        let vm = monoid.combine vpr (measure_t_node ~monoid m) in
        if p vm then
          let Split (ml, xs, mr) = split_tree ~monoid ~measure:measure_node p vpr m in
          let Split (l, x, r) = split_digit ~monoid ~measure p (monoid.combine vpr (measure_t_node ~monoid ml)) (to_digit_node xs) in
          Split (deep_right ~monoid ~measure pr ml l, x, deep_left ~monoid ~measure r mr sf)
        else
          let Split (l, x, r) = split_digit ~monoid ~measure p vm sf in
          Split (deep_right ~monoid ~measure pr m l, x, to_tree_list ~monoid ~measure r)

  let split ~monoid ~measure f t =
    match t with
    | Nil -> (Nil, Nil)
    | _ ->
      if f (measure_t ~monoid ~measure t) then
        let Split (l, x, r) = split_tree ~monoid ~measure f monoid.zero t in
        (l, cons ~monoid ~measure r x)
      else
        (t, Nil)

  let lookup_digit ~monoid ~measure p i = function
    | One (_, a) -> monoid.zero, a
    | Two (_, a, b) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else m_a, b
    | Three (_, a, b, c) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else
        let m_b = measure b in
        let i'' = monoid.combine i' m_b in
        if p i'' then m_a, b else monoid.combine m_a m_b, c
    | Four (_, a, b, c, d) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else
        let m_b = measure b in
        let i'' = monoid.combine i' m_b in
        if p i'' then m_a, b else
          let m_c = measure c in
          let i''' = monoid.combine i'' m_c in
          if p i''' then monoid.combine m_a m_b, c else monoid.combine (monoid.combine m_a m_b) m_c, d

  let lookup_node ~monoid ~measure p i = function
    | Node2 (_, a, b) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else m_a, b
    | Node3 (_, a, b, c) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else
        let m_b = measure b in
        let i'' = monoid.combine i' m_b in
        if p i'' then m_a, b else monoid.combine m_a m_b, c

  let rec lookup_tree : 'a 'm. monoid:'m monoid -> measure:('a -> 'm) -> ('m -> bool) -> 'm -> ('a, 'm) fg -> 'm * 'a =
    fun ~monoid ~measure p i -> function
    | Nil -> raise Empty
    | Single x -> monoid.zero, x
    | Deep (_, pr, m, sf) ->
      let m_pr = measure_digit pr in
      let vpr = monoid.combine i m_pr in
      if p vpr then lookup_digit ~monoid ~measure p i pr else
        let m_m = measure_t_node ~monoid m in
        let vm = monoid.combine vpr m_m in
        if p vm then
          let v_left, node = lookup_tree ~monoid ~measure:measure_node p vpr m in
          let v, x = lookup_node ~monoid ~measure p (monoid.combine vpr v_left) node in
          monoid.combine (monoid.combine m_pr v_left) v, x
        else
          let v, x = lookup_digit ~monoid ~measure p vm sf in
          monoid.combine (monoid.combine m_pr m_m) v, x

  let lookup ~monoid ~measure p t =
    snd (lookup_tree ~monoid ~measure p monoid.zero t)

  let enum_digit enum_a d k =
    match d with
    | One (_, a) ->
      enum_a a k
    | Two (_, a, b) ->
      enum_a a (fun () -> enum_a b k)
    | Three (_, a, b, c) ->
      enum_a a (fun () -> enum_a b (fun () -> enum_a c k))
    | Four (_, a, b, c, d) ->
      enum_a a (fun () -> enum_a b (fun () -> enum_a c (fun () -> enum_a d k)))
  let enum_digit_backwards enum_a d k =
    match d with
    | One (_, a) ->
      enum_a a k
    | Two (_, a, b) ->
      enum_a b (fun () -> enum_a a k)
    | Three (_, a, b, c) ->
      enum_a c (fun () -> enum_a b (fun () -> enum_a a k))
    | Four (_, a, b, c, d) ->
      enum_a d (fun () -> enum_a c (fun () -> enum_a b (fun () -> enum_a a k)))

  let enum_node enum_a n k =
    match n with
    | Node2 (_, a, b) ->
      enum_a a (fun () -> enum_a b k)
    | Node3 (_, a, b, c) ->
      enum_a a (fun () -> enum_a b (fun () -> enum_a c k))
  let enum_node_backwards enum_a n k =
    match n with
    | Node2 (_, a, b) ->
      enum_a b (fun () -> enum_a a k)
    | Node3 (_, a, b, c) ->
      enum_a c (fun () -> enum_a b (fun () -> enum_a a k))

  let enum_base a k = a, k

  type 'a iter = unit -> 'a ret
  and 'a ret = 'a * 'a iter
  type ('input, 'output) iter_into = 'input -> 'output iter -> 'output ret

  let rec enum_aux : 'v 'a 'm. ('a, 'v) iter_into -> (('a, 'm) fg, 'v) iter_into =
    fun enum_a t k ->
    match t with
    | Nil -> k ()
    | Single a -> enum_a a k
    | Deep (_, pr, m, sf) ->
      enum_digit enum_a pr (fun () ->
        enum_aux (enum_node enum_a) m (fun () ->
          enum_digit enum_a sf k
        )
      )
  let enum_cps t = enum_aux enum_base t (fun () -> raise BatEnum.No_more_elements)

  let rec enum_aux_backwards : 'v 'a 'm. ('a, 'v) iter_into -> (('a, 'm) fg, 'v) iter_into =
    fun enum_a t k ->
    match t with
    | Nil -> k ()
    | Single a -> enum_a a k
    | Deep (_, pr, m, sf) ->
      enum_digit_backwards enum_a sf (fun () ->
        enum_aux_backwards (enum_node_backwards enum_a) m (fun () ->
          enum_digit_backwards enum_a pr k
        )
      )
  let enum_cps_backwards t = enum_aux_backwards enum_base t (fun () -> raise BatEnum.No_more_elements)

  let enum t =
    BatEnum.from_loop
      (fun () -> enum_cps t)
      (fun k -> k ())
  let backwards t =
    BatEnum.from_loop
      (fun () -> enum_cps_backwards t)
      (fun k -> k ())

  let of_enum ~monoid ~measure enum =
    BatEnum.fold (fun t elt -> snoc ~monoid ~measure t elt) empty enum
  let of_backwards ~monoid ~measure enum =
    BatEnum.fold (fun t elt -> cons ~monoid ~measure t elt) empty enum

  let measure = measure_t
  let map ~monoid ~measure f t = (* suboptimal when the measure does not depend on 'a *)
    fold_left (fun acc elt -> snoc ~monoid ~measure acc (f elt)) empty t
end

module Sequence : sig
  include SIG

  val enum2 : 'a t -> 'a BatEnum.t
  val fold_left2 : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_right2 : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

  val reverse2 : 'a t -> 'a t
  val update2 : 'a t -> int -> ('a -> 'a) -> 'a t
  val set2 : 'a t -> int -> 'a -> 'a t
  val get2 : 'a t -> int -> 'a
  val of_enum2 : 'a BatEnum.t -> 'a t
  val map2 : ('a -> 'b) -> 'a t -> 'b t
end =
struct
  type nat = int
  let nat_plus_monoid = { GenFingerTree.
    zero = 0;
    combine = (+);
  }
  let size_measurer = fun _ -> 1

  type ('a, 'm) fg = ('a, nat) GenFingerTree.fg
  type 'a t = ('a, nat) fg
  let empty = GenFingerTree.empty
  let fold_left = GenFingerTree.fold_left
  let fold_right = GenFingerTree.fold_right
  let cons t x = GenFingerTree.cons ~monoid:nat_plus_monoid ~measure:size_measurer t x
  let snoc t x = GenFingerTree.snoc ~monoid:nat_plus_monoid ~measure:size_measurer t x
  let front t = GenFingerTree.front ~monoid:nat_plus_monoid ~measure:size_measurer t
  let rear t = GenFingerTree.rear ~monoid:nat_plus_monoid ~measure:size_measurer t
  let append t1 t2 = GenFingerTree.append ~monoid:nat_plus_monoid ~measure:size_measurer t1 t2
  let reverse t = GenFingerTree.reverse ~monoid:nat_plus_monoid ~measure:size_measurer t
  let measure t = GenFingerTree.measure ~monoid:nat_plus_monoid ~measure:size_measurer t
  let size = measure
  let split f t = GenFingerTree.split ~monoid:nat_plus_monoid ~measure:size_measurer f t
  let split_at t i =
    if i < 0 || i >= size t then invalid_arg "Index out of bounds";
    split (fun index -> i < index) t
  let lookup f t = GenFingerTree.lookup ~monoid:nat_plus_monoid ~measure:size_measurer f t
  let get t i =
    if i < 0 || i >= size t then invalid_arg "Index out of bounds";
    lookup (fun index -> i < index) t
  let tail_exn t = GenFingerTree.tail_exn ~monoid:nat_plus_monoid ~measure:size_measurer t
  let set t i v =
    if i < 0 || i >= size t then invalid_arg "Index out of bounds";
    let left, right = split_at t i in
    append (snoc left v) (tail_exn right)
  let update t i f =
    set t i (f (get t i))
  let of_enum e = GenFingerTree.of_enum ~monoid:nat_plus_monoid ~measure:size_measurer e
  let generate_of_enum = of_enum
  let of_backwards e = GenFingerTree.of_backwards ~monoid:nat_plus_monoid ~measure:size_measurer e
  let map f t = GenFingerTree.map ~monoid:nat_plus_monoid ~measure:size_measurer f t
  let enum = GenFingerTree.enum
  let backwards = GenFingerTree.backwards



  module Opt = (* optimized *)
  struct
    open GenFingerTree

    let rec height : 'a. int -> ('a, 'm) fg -> int = fun acc -> function
      | Nil
      | Single _ -> acc
      | Deep (_, _, m, _) -> height (acc + 1) m
    let height t = height 0 t

    let tdigit = 0
    let tfg = 1
    let telt = 2
    type 'a iter = int array

    let rec aux_elt stack index depth elt =
      if depth = 0 then (
        stack.(0) <- index - 2;
        stack.(index - 1) <- 42; (*gc*)
        Obj.magic elt
      ) else (
        match Obj.magic elt with
        | Node2 (_, a, b) ->
          stack.(index - 1) <- Obj.magic b;
          stack.(index + 0) <- telt lor ((depth - 1) lsl 2);
          aux_elt stack (index + 2) (depth - 1) a
        | Node3 (_, a, b, c) ->
          stack.(index - 1) <- Obj.magic c;
          stack.(index + 0) <- telt lor ((depth - 1) lsl 2);
          stack.(index + 1) <- Obj.magic b;
          stack.(index + 2) <- telt lor ((depth - 1) lsl 2);
          aux_elt stack (index + 4) (depth - 1) a
      )

    let aux_digit stack index depth = function
      | One (_, a) ->
        aux_elt stack index depth a
      | Two (_, a, b) ->
        stack.(index - 1) <- Obj.magic b;
        stack.(index + 0) <- telt lor (depth lsl 2);
        aux_elt stack (index + 2) depth a
      | Three (_, a, b, c) ->
        stack.(index - 1) <- Obj.magic c;
        stack.(index + 0) <- telt lor (depth lsl 2);
        stack.(index + 1) <- Obj.magic b;
        stack.(index + 2) <- telt lor (depth lsl 2);
        aux_elt stack (index + 4) depth a
      | Four (_, a, b, c, d) ->
        stack.(index - 1) <- Obj.magic d;
        stack.(index + 0) <- telt lor (depth lsl 2);
        stack.(index + 1) <- Obj.magic c;
        stack.(index + 2) <- telt lor (depth lsl 2);
        stack.(index + 3) <- Obj.magic b;
        stack.(index + 4) <- telt lor (depth lsl 2);
        aux_elt stack (index + 6) depth a

    let rec aux stack index =
      if index = 0 then (
        stack.(0) <- 0;
        raise BatEnum.No_more_elements
      );
      let type_ = stack.(index) land 3 in
      let depth = stack.(index) lsr 2 in
      let value = Obj.magic stack.(index - 1) in
      if type_ = telt then (* this test comes first because it is
                            * the one most likely to be true
                            * making it last results in a 20% slow down *)
        aux_elt stack index depth value
      else if type_ = tfg then
        match value with
        | Nil -> stack.(index - 1) <- 0(*gc*); aux stack (index - 2)
        | Single x -> aux_elt stack index depth x
        | Deep (_, pr, m, sf) ->
          stack.(index - 1) <- Obj.magic sf;
          stack.(index + 0) <- tdigit lor (depth lsl 2);
          stack.(index + 1) <- Obj.magic m;
          stack.(index + 2) <- tfg lor ((depth + 1) lsl 2);
          aux_digit stack (index + 4) depth pr
      else
        aux_digit stack index depth value

    let enum_next (stack : int array) =
      aux stack stack.(0)

    let enum_stack t : _ array =
      let stack = Obj.obj (Obj.new_block 0 ((3 * height t + 3 + 1) * 2 + 1)) in
      stack.(0) <- 2;
      stack.(1) <- Obj.magic t;
      stack.(2) <- tfg;
      stack

    let enum t =
      let stack = enum_stack t in
      BatEnum.make
        ~next:(fun () -> enum_next stack)
        ~count:(fun _ -> assert false)
        ~clone:(fun () -> assert false)

    let rec fold_left_a f depth acc a =
      if depth = 0 then
        f acc a
      else
        Obj.magic (
          match Obj.magic a with
          | Node2 (_, a, b) ->
            let acc = fold_left_a f (depth - 1) acc a in
            let acc = fold_left_a f (depth - 1) acc b in
            acc
          | Node3 (_, a, b, c) ->
            let acc = fold_left_a f (depth - 1) acc a in
            let acc = fold_left_a f (depth - 1) acc b in
            let acc = fold_left_a f (depth - 1) acc c in
            acc
        )

    let fold_left_digit f depth acc = function
      | One (_, a) ->
        fold_left_a f depth acc a
      | Two (_, a, b) ->
        let acc = fold_left_a f depth acc a in
        let acc = fold_left_a f depth acc b in
        acc
      | Three (_, a, b, c) ->
        let acc = fold_left_a f depth acc a in
        let acc = fold_left_a f depth acc b in
        let acc = fold_left_a f depth acc c in
        acc
      | Four (_, a, b, c, d) ->
        let acc = fold_left_a f depth acc a in
        let acc = fold_left_a f depth acc b in
        let acc = fold_left_a f depth acc c in
        let acc = fold_left_a f depth acc d in
        acc

    let rec fold_left f depth acc = function
      | Nil ->
        acc
      | Single a ->
        fold_left_a f depth acc a
      | Deep (_, pr, m, sf) ->
        let acc = fold_left_digit f depth acc pr in
        let acc = fold_left f (depth + 1) acc (Obj.magic m) in
        let acc = fold_left_digit f depth acc sf in
        acc
    let fold_left f acc t =
      fold_left f 0 acc t

    let rec fold_right_a f depth acc a =
      if depth = 0 then
        f acc a
      else
        Obj.magic (
          match Obj.magic a with
          | Node2 (_, a, b) ->
            let acc = fold_right_a f (depth - 1) acc b in
            let acc = fold_right_a f (depth - 1) acc a in
            acc
          | Node3 (_, a, b, c) ->
            let acc = fold_right_a f (depth - 1) acc c in
            let acc = fold_right_a f (depth - 1) acc b in
            let acc = fold_right_a f (depth - 1) acc a in
            acc
        )

    let fold_right_digit f depth acc = function
      | One (_, a) ->
        fold_right_a f depth acc a
      | Two (_, a, b) ->
        let acc = fold_right_a f depth acc b in
        let acc = fold_right_a f depth acc a in
        acc
      | Three (_, a, b, c) ->
        let acc = fold_right_a f depth acc c in
        let acc = fold_right_a f depth acc b in
        let acc = fold_right_a f depth acc a in
        acc
      | Four (_, a, b, c, d) ->
        let acc = fold_right_a f depth acc d in
        let acc = fold_right_a f depth acc c in
        let acc = fold_right_a f depth acc b in
        let acc = fold_right_a f depth acc a in
        acc

    let rec fold_right f depth acc = function
      | Nil ->
        acc
      | Single a ->
        fold_right_a f depth acc a
      | Deep (_, pr, m, sf) ->
        let acc = fold_right_digit f depth acc sf in
        let acc = fold_right f (depth + 1) acc (Obj.magic m) in
        let acc = fold_right_digit f depth acc pr in
        acc
    let fold_right f acc t =
      fold_right f 0 acc t
  end

  let enum2 = Opt.enum
  let fold_left2 = Opt.fold_left
  let fold_right2 = Opt.fold_right

  module Spec = (* specialized for int annots *)
  struct
    open GenFingerTree

    let measure_t_node = function
      | Nil -> 0
      | Single x -> measure_node x
      | Deep (v, _, _, _) -> v
    let measure_t = function
      | Nil -> 0
      | Single _ -> 1
      | Deep (v, _, _, _) -> v

    let node2 a b =
      Node2 (2, a, b)
    let node2_node a b =
      Node2 (measure_node a + measure_node b, a, b)

    let node3 a b c =
      Node3 (3, a, b, c)
    let node3_node a b c =
      Node3 (measure_node a + measure_node b + measure_node c, a, b, c)

    let deep pr m sf =
      Deep (measure_digit pr + measure_t_node m + measure_digit sf, pr, m, sf)

    let one a =
      One (1, a)
    let one_node a =
      One (measure_node a, a)

    let two a b =
      Two (2, a, b)
    let two_node a b =
      Two (measure_node a + measure_node b, a, b)

    let three a b c =
      Three (3, a, b, c)
    let three_node a b c =
      Three (measure_node a + measure_node b + measure_node c, a, b, c)

    let four a b c d =
      Four (4, a, b, c, d)
    let four_node a b c d =
      Four (measure_node a + measure_node b + measure_node c + measure_node d, a, b, c, d)


    let rec reverse_a depth a =
      if depth = 0 then a else
        Obj.magic (
          match Obj.magic a with
          | Node2 (v, a, b) ->
            Node2 (v, reverse_a (depth - 1) b, reverse_a (depth - 1) a)
          | Node3 (v, a, b, c) ->
            Node3 (v, reverse_a (depth - 1) c, reverse_a (depth - 1) b, reverse_a (depth - 1) a)
        )

    let reverse_digit depth = function
      | One (v, a) ->
        One (v, reverse_a depth a)
      | Two (v, a, b) ->
        Two (v, reverse_a depth b, reverse_a depth a)
      | Three (v, a, b, c) ->
        Three (v, reverse_a depth c, reverse_a depth b, reverse_a depth a)
      | Four (v, a, b, c, d) ->
        Four (v, reverse_a depth d, reverse_a depth c, reverse_a depth b, reverse_a depth a)

    let rec reverse depth = function
      | Nil -> Nil
      | Single a -> Single (reverse_a depth a)
      | Deep (v, pr, m, sf) ->
        let rev_pr = reverse_digit depth pr in
        let rev_sf = reverse_digit depth sf in
        let rev_m = Obj.magic (reverse (depth + 1) (Obj.magic m)) in
        Deep (v, rev_sf, rev_m, rev_pr)
    let reverse t = reverse 0 t

    let get_digit d i =
      match d with
      | One (_, a) -> a
      | Two (_, a, b) -> if i = 0 then a else b
      | Three (_, a, b, c) -> if i = 0 then a else if i = 1 then b else c
      | Four (_, a, b, c, d) -> if i < 2 then (if i = 0 then a else b) else (if i = 2 then c else d)

    let rec get_a depth a i =
      if depth = 1 then (
        match Obj.magic a with
        | Node2 (_, a, b) -> if i = 0 then a else b
        | Node3 (_, a, b, c) -> if i = 0 then a else if i = 1 then b else c
      ) else (
        match Obj.magic a with
        | Node2 (_, a, b) ->
          if i < measure_node a then get_a (depth - 1) a i else
            let i = i - measure_node a in
            get_a (depth - 1) b i
        | Node3 (_, a, b, c) ->
          if i < measure_node a then get_a (depth - 1) a i else
            let i = i - measure_node a in
            if i < measure_node b then get_a (depth - 1) b i else
              let i = i - measure_node b in
              get_a (depth - 1) c i
      )

    let get_digit_node depth d i =
      match d with
      | One (_, a) ->
        get_a depth a i
      | Two (_, a, b) ->
        if i < measure_node a then get_a depth a i else
          let i = i - measure_node a in
          get_a depth b i
      | Three (_, a, b, c) ->
        if i < measure_node a then get_a depth a i else
          let i = i - measure_node a in
          if i < measure_node b then get_a depth b i else
            let i = i - measure_node b in
            get_a depth c i
      | Four (_, a, b, c, d) ->
        if i < measure_node a then get_a depth a i else
          let i = i - measure_node a in
          if i < measure_node b then get_a depth b i else
            let i = i - measure_node b in
            if i < measure_node c then get_a depth c i else
              let i = i - measure_node c in
              get_a depth d i

    let rec get_aux depth t i =
      match t with
      | Nil -> assert false
      | Single v -> get_a depth v i
      | Deep (_, pr, m, sf) ->
        if i < measure_digit pr then
          get_digit_node depth pr i
        else
          let i = i - measure_digit pr in
          if i < measure_t_node m then
            get_aux (depth + 1) (Obj.magic m) i
          else
            let i = i - measure_t_node m in
            get_digit_node depth sf i

    let check_bounds t i =
      if i < 0 || i >= size t then invalid_arg "Index out of bounds"

    let get t i =
      check_bounds t i;
      match t with
      | Nil -> assert false
      | Single v -> v
      | Deep (_, pr, m, sf) ->
        if i < measure_digit pr then
          get_digit pr i
        else
          let i = i - measure_digit pr in
          if i < measure_t_node m then
            get_aux 1 m i
          else
            let i = i - measure_t_node m in
            get_digit sf i

    let update_digit d i f =
      match d with
      | One (v, a) ->
        One (v, f a)
      | Two (v, a, b) ->
        if i = 0 then Two (v, f a, b) else
          Two (v, a, f b)
      | Three (v, a, b, c) ->
        if i = 0 then Three (v, f a, b, c) else
          if i = 1 then Three (v, a, f b, c) else
            Three (v, a, b, f c)
      | Four (v, a, b, c, d) ->
        if i < 2 then (
          if i = 0 then Four (v, f a, b, c, d) else Four (v, a, f b, c, d)
        ) else (
          if i = 2 then Four (v, a, b, f c, d) else Four (v, a, b, c, f d)
        )

    let rec update_a depth a i f =
      if depth = 1 then
        Obj.magic (
          match Obj.magic a with
          | Node2 (v, a, b) ->
            if i = 0 then Node2 (v, f a, b) else
              Node2 (v, a, f b)
          | Node3 (v, a, b, c) ->
            if i = 0 then Node3 (v, f a, b, c) else
              if i = 1 then Node3 (v, a, f b, c) else
                Node3 (v, a, b, f c)
        )
      else
        Obj.magic (
          match Obj.magic a with
          | Node2 (v, a, b) ->
            if i < measure_node a then Node2 (v, update_a (depth - 1) a i f, b) else
              let i = i - measure_node a in
              Node2 (v, a, update_a (depth - 1) b i f)
          | Node3 (v, a, b, c) ->
            if i < measure_node a then Node3 (v, update_a (depth - 1) a i f, b, c) else
              let i = i - measure_node a in
              if i < measure_node b then Node3 (v, a, update_a (depth - 1) b i f, c) else
                let i = i - measure_node b in
                Node3 (v, a, b, update_a (depth - 1) c i f)
        )

    let update_digit_node depth d i f =
      match d with
      | One (v, a) ->
        One (v, update_a depth a i f)
      | Two (v, a, b) ->
        if i < measure_node a then Two (v, update_a depth a i f, b) else
          let i = i - measure_node a in
          Two (v, a, update_a depth b i f)
      | Three (v, a, b, c) ->
        if i < measure_node a then Three (v, update_a depth a i f, b, c) else
          let i = i - measure_node a in
          if i < measure_node b then Three (v, a, update_a depth b i f, c) else
            let i = i - measure_node b in
            Three (v, a, b, update_a depth c i f)
      | Four (v, a, b, c, d) ->
        if i < measure_node a then Four (v, update_a depth a i f, b, c, d) else
          let i = i - measure_node a in
          if i < measure_node b then Four (v, a, update_a depth b i f, c, d) else
            let i = i - measure_node b in
            if i < measure_node c then Four (v, a, b, update_a depth c i f, d) else
              let i = i - measure_node c in
              Four (v, a, b, c, update_a depth d i f)

    let rec update_aux depth t i f =
      match t with
      | Nil -> assert false
      | Single v -> Single (update_a depth v i f)
      | Deep (v, pr, m, sf) ->
        if i < measure_digit pr then
          Deep (v, update_digit_node depth pr i f, m, sf)
        else
          let i = i - measure_digit pr in
          if i < measure_t_node m then
            Deep (v, pr, Obj.magic (update_aux (depth + 1) (Obj.magic m) i f), sf)
          else
            let i = i - measure_t_node m in
            Deep (v, pr, m, update_digit_node depth sf i f)

    let update t i f =
      check_bounds t i;
      match t with
      | Nil -> assert false
      | Single v -> Single (f v)
      | Deep (v, pr, m, sf) ->
        if i < measure_digit pr then
          Deep (v, update_digit pr i f, m, sf)
        else
          let i = i - measure_digit pr in
          if i < measure_t_node m then
            Deep (v, pr, update_aux 1 m i f, sf)
          else
            let i = i - measure_t_node m in
            Deep (v, pr, m, update_digit sf i f)

    let set t i v =
      update t i (fun _ -> v)

    let rec get_node depth enum =
      if depth = 1 then
        let v1 = BatEnum.get_exn enum in
        let v2 = BatEnum.get_exn enum in
        let v3 = BatEnum.get_exn enum in
        Obj.magic (node3 v1 v2 v3)
      else
        let v1 = get_node (depth - 1) enum in
        let v2 = get_node (depth - 1) enum in
        let v3 = get_node (depth - 1) enum in
        Obj.magic (node3_node v1 v2 v3)

    let rec get_digit_node depth enum n =
      match n with
      | 1 ->
        let v1 = get_node depth enum in
        one_node v1
      | 2 ->
        let v1 = get_node depth enum in
        let v2 = get_node depth enum in
        two_node v1 v2
      | 3 ->
        let v1 = get_node depth enum in
        let v2 = get_node depth enum in
        let v3 = get_node depth enum in
        three_node v1 v2 v3
      | 4 ->
        let v1 = get_node depth enum in
        let v2 = get_node depth enum in
        let v3 = get_node depth enum in
        let v4 = get_node depth enum in
        four_node v1 v2 v3 v4
      | _ -> assert false

    let rec fast_of_enum_aux depth enum n =
      if n = 0 then Nil else
        if n = 1 then Single (get_node depth enum) else
          let n_rec = if n <= 8 then 0 else (n - 8 + 3 - 1) / 3 in
          let n_left = (n - n_rec * 3) / 2 in
          let n_right = (n - n_rec * 3 + 1) / 2 in
          let pr = get_digit_node depth enum n_left in
          let m = Obj.magic (fast_of_enum_aux (depth + 1) enum n_rec) in
          let sf = get_digit_node depth enum n_right in
          deep pr m sf

    let rec get_digit enum n =
      match n with
      | 1 ->
        let v1 = BatEnum.get_exn enum in
        one v1
      | 2 ->
        let v1 = BatEnum.get_exn enum in
        let v2 = BatEnum.get_exn enum in
        two v1 v2
      | 3 ->
        let v1 = BatEnum.get_exn enum in
        let v2 = BatEnum.get_exn enum in
        let v3 = BatEnum.get_exn enum in
        three v1 v2 v3
      | 4 ->
        let v1 = BatEnum.get_exn enum in
        let v2 = BatEnum.get_exn enum in
        let v3 = BatEnum.get_exn enum in
        let v4 = BatEnum.get_exn enum in
        four v1 v2 v3 v4
      | _ -> assert false

    let fast_of_enum enum n =
      if n = 0 then Nil else
        if n = 1 then Single (BatEnum.get_exn enum) else
          let n_rec = if n <= 8 then 0 else (n - 8 + 3 - 1) / 3 in
          let n_left = (n - n_rec * 3) / 2 in
          let n_right = (n - n_rec * 3 + 1) / 2 in
          let pr = get_digit enum n_left in
          let m = fast_of_enum_aux 1 enum n_rec in
          let sf = get_digit enum n_right in
          Deep (n, pr, m, sf)

    let rec get_node depth a i =
      if depth = 1 then
        let v1 = BatDynArray.unsafe_get a !i in
        let v2 = BatDynArray.unsafe_get a (!i + 1) in
        let v3 = BatDynArray.unsafe_get a (!i + 2) in
        i := !i + 3;
        Obj.magic (node3 v1 v2 v3)
      else
        let v1 = get_node (depth - 1) a i in
        let v2 = get_node (depth - 1) a i in
        let v3 = get_node (depth - 1) a i in
        Obj.magic (node3_node v1 v2 v3)

    let rec get_digit_node depth a i n =
      match n with
      | 1 ->
        let v1 = get_node depth a i in
        one_node v1
      | 2 ->
        let v1 = get_node depth a i in
        let v2 = get_node depth a i in
        two_node v1 v2
      | 3 ->
        let v1 = get_node depth a i in
        let v2 = get_node depth a i in
        let v3 = get_node depth a i in
        three_node v1 v2 v3
      | 4 ->
        let v1 = get_node depth a i in
        let v2 = get_node depth a i in
        let v3 = get_node depth a i in
        let v4 = get_node depth a i in
        four_node v1 v2 v3 v4
      | _ -> assert false

    let rec fast_of_enum_aux depth a i n =
      if n = 0 then Nil else
        if n = 1 then Single (get_node depth a i) else
          let n_rec = if n <= 8 then 0 else (n - 8 + 3 - 1) / 3 in
          let n_left = (n - n_rec * 3) / 2 in
          let n_right = (n - n_rec * 3 + 1) / 2 in
          let pr = get_digit_node depth a i n_left in
          let m = Obj.magic (fast_of_enum_aux (depth + 1) a i n_rec) in
          let sf = get_digit_node depth a i n_right in
          deep pr m sf

    let rec get_digit a i n =
      match n with
      | 1 ->
        let v1 = BatDynArray.unsafe_get a !i in
        i := !i + 1;
        one v1
      | 2 ->
        let v1 = BatDynArray.unsafe_get a !i in
        let v2 = BatDynArray.unsafe_get a (!i + 1) in
        i := !i + 2;
        two v1 v2
      | 3 ->
        let v1 = BatDynArray.unsafe_get a !i in
        let v2 = BatDynArray.unsafe_get a (!i + 1) in
        let v3 = BatDynArray.unsafe_get a (!i + 2) in
        i := !i + 3;
        three v1 v2 v3
      | 4 ->
        let v1 = BatDynArray.unsafe_get a !i in
        let v2 = BatDynArray.unsafe_get a (!i + 1) in
        let v3 = BatDynArray.unsafe_get a (!i + 2) in
        let v4 = BatDynArray.unsafe_get a (!i + 3) in
        i := !i + 4;
        four v1 v2 v3 v4
      | _ -> assert false

    let fast_of_enum_array a i n =
      if n = 0 then Nil else
        if n = 1 then Single (BatDynArray.unsafe_get a 0) else
          let n_rec = if n <= 8 then 0 else (n - 8 + 3 - 1) / 3 in
          let n_left = (n - n_rec * 3) / 2 in
          let n_right = (n - n_rec * 3 + 1) / 2 in
          let pr = get_digit a i n_left in
          let m = fast_of_enum_aux 1 a i n_rec in
          let sf = get_digit a i n_right in
          Deep (n, pr, m, sf)

    let of_enum enum =
      if BatEnum.fast_count enum then
        fast_of_enum enum (BatEnum.count enum)
      else
        let a = BatDynArray.make 10 in
        try while true do BatDynArray.add a (BatEnum.get_exn enum) done;
            assert false
        with BatEnum.No_more_elements ->
          fast_of_enum_array a (ref 0) (BatDynArray.length a)

    let rec map_a f depth a =
      if depth = 0 then
        f a
      else
        Obj.magic (
          match Obj.magic a with
          | Node2 (v, a, b) ->
            let a = map_a f (depth - 1) a in
            let b = map_a f (depth - 1) b in
            Node2 (v, a, b)
          | Node3 (v, a, b, c) ->
            let a = map_a f (depth - 1) a in
            let b = map_a f (depth - 1) b in
            let c = map_a f (depth - 1) c in
            Node3 (v, a, b, c)
        )

    let map_digit f depth = function
      | One (v, a) ->
        let a = map_a f depth a in
        One (v, a)
      | Two (v, a, b) ->
        let a = map_a f depth a in
        let b = map_a f depth b in
        Two (v, a, b)
      | Three (v, a, b, c) ->
        let a = map_a f depth a in
        let b = map_a f depth b in
        let c = map_a f depth c in
        Three (v, a, b, c)
      | Four (v, a, b, c, d) ->
        let a = map_a f depth a in
        let b = map_a f depth b in
        let c = map_a f depth c in
        let d = map_a f depth d in
        Four (v, a, b, c, d)

    let rec map f depth = function
      | Nil ->
        Nil
      | Single a ->
        let a = map_a f depth a in
        Single a
      | Deep (v, pr, m, sf) ->
        let pr = map_digit f depth pr in
        let m = Obj.magic (map f (depth + 1) (Obj.magic m)) in
        let sf = map_digit f depth sf in
        Deep (v, pr, m, sf)

    let map f t =
      map f 0 t
  end

  let reverse2 = Spec.reverse
  let update2 = Spec.update
  let set2 = Spec.set
  let get2 = Spec.get
  let of_enum2 = Spec.of_enum
  let map2 = Spec.map
end














(*SEARCHME*)

let rec memory_size acc t =
  let tag = Obj.tag t in
  if tag = Obj.int_tag then acc
  else if tag < Obj.no_scan_tag &&
      tag <> Obj.lazy_tag &&
      tag <> Obj.closure_tag &&
      tag <> Obj.object_tag &&
      tag <> Obj.infix_tag &&
      tag <> Obj.forward_tag &&
      tag <> Obj.abstract_tag &&
      tag <> Obj.string_tag &&
      tag <> Obj.double_tag &&
      tag <> Obj.double_array_tag &&
      tag <> Obj.custom_tag &&
      tag <> Obj.final_tag &&
      tag <> Obj.out_of_heap_tag &&
      tag <> Obj.unaligned_tag then
    let size = Obj.size t in
    let acc = ref (acc + size + 1) in
    for i = 0 to size - 2 do
      acc := memory_size !acc (Obj.field t i)
    done;
    memory_size !acc (Obj.field t (size - 1))
  else
    assert false
let memory_size a = memory_size 0 (Obj.repr a)

let bench_size size s =
  let module M = (val s : SIG) in
  let rec aux stack = function
    | 0 -> stack
    | n -> aux (M.cons stack n) (n - 1) in
  let s = aux M.empty size in
  memory_size s

let bench_cons_front size s n =
  for i = 0 to n do
    let module M = (val s : SIG) in
    let rec aux stack = function
      | 0 -> stack
      | n -> aux (M.cons stack n) (n - 1) in
    let s = aux M.empty size in
    let rec aux stack =
      match M.front stack with
      | None -> ()
      | Some (stack, _) -> aux stack in
    aux s
  done

let bench_map size s =
  (* not benching the construction time, just the mapping time *)
  let module M = (val s : SIG) in
  let rec aux stack = function
    | 0 -> stack
    | n -> aux (M.cons stack n) (n - 1) in
  let s = aux M.empty size in
  fun n ->
    for i = 0 to n do
      ignore (M.map (fun x -> x + 1) s)
    done

let bench_snoc_front size s n =
  for i = 0 to n do
    let module M = (val s : SIG) in
    let rec aux stack = function
      | 0 -> stack
      | n -> aux (M.snoc stack n) (n - 1) in
    let s = aux M.empty size in
    let rec aux stack =
      match M.front stack with
      | None -> ()
      | Some (stack, _) -> aux stack in
    aux s
  done

let bench_snoc_front_rear size s n =
  for i = 0 to n do
    let module M = (val s : SIG) in
    let rec aux stack = function
      | 0 -> stack
      | n -> aux (M.snoc stack n) (n - 1) in
    let s = aux M.empty size in
    let rec aux stack =
      match M.front stack with
      | None -> ()
      | Some (stack, _) ->
        match M.rear stack with
        | None -> ()
        | Some (stack, _) -> aux stack in
    aux s
  done

let bench_enum1 size s =
  let a = BatArray.Labels.init size ~f:(fun i -> i) in
  let e = BatArray.enum a in
  let module M = (val s : SIG) in
  let t = M.generate_of_enum e in
  fun n ->
    for i = 0 to n do
      let e = M.enum t in
      try while true; do ignore (BatEnum.get_exn e); done
      with BatEnum.No_more_elements -> ()
    done

let bench_of_enum1 size s n =
  let a = BatArray.Labels.init size ~f:(fun i -> i) in
  for i = 0 to n do
    let e = BatArray.enum a in
    let module M = (val s : SIG) in
    ignore (M.of_enum e)
  done

let bench_fold_left size s =
  let a = BatArray.Labels.init size ~f:(fun i -> i) in
  let e = BatArray.enum a in
  let module M = (val s : SIG) in
  let t = M.generate_of_enum e in
  fun n ->
    for i = 0 to n do
      M.fold_left (fun () _ -> ()) () t;
    done

let bench_fold_right size s =
  let a = BatArray.Labels.init size ~f:(fun i -> i) in
  let e = BatArray.enum a in
  let module M = (val s : SIG) in
  let t = M.generate_of_enum e in
  fun n ->
    for i = 0 to n do
      M.fold_right (fun () _ -> ()) () t;
    done

let bench_reverse size s =
  let a = BatArray.Labels.init size ~f:(fun i -> i) in
  let e = BatArray.enum a in
  let module M = (val s : SIG) in
  let t = M.generate_of_enum e in
  fun n ->
    for i = 0 to n do
      ignore (M.reverse t)
    done

let bench_append size s =
  let a = BatArray.Labels.init size ~f:(fun i -> i) in
  let e = BatArray.enum a in
  let module M = (val s : SIG) in
  let t = M.generate_of_enum e in
  fun n ->
    for i = 0 to n do
      ignore (M.append t t)
    done

let bench_get size s =
  let a = BatArray.Labels.init size ~f:(fun i -> i) in
  let e = BatArray.enum a in
  let module M = (val s : SIG) in
  let t = M.generate_of_enum e in
  fun n ->
    for i = 0 to n do
      for i = 0 to size - 1 do
        ignore (M.get t i)
      done
    done

let bench_set size s =
  let a = BatArray.Labels.init size ~f:(fun i -> i) in
  let e = BatArray.enum a in
  let module M = (val s : SIG) in
  let t = M.generate_of_enum e in
  fun n ->
    for i = 0 to n do
      let t = ref t in
      for i = 0 to size - 1 do
        t := M.set !t i 0
      done
    done

module ListTailCore : SIG = struct include ListTail let map = map2 end
module ListTailModConsOpt : SIG = struct include ListTailModCons let map = map2 end
module FgGen : SIG = Sequence
module FgGenOpt : SIG = struct include Sequence let enum = enum2 let fold_left = fold_left2 let fold_right = fold_right2 end
module FgSpec : SIG = struct include Sequence let reverse = reverse2 let update = update2 let set = set2 let get = get2 let of_enum = of_enum2 let map = map2 end


let sizes = [
  1; 10; 100; 1_000; 10_000; 100_000;
]

let print_readings ~title size l =
  if size = BatList.hd sizes then (
    Printf.printf "#%s size" title;
    BatList.iter (fun r ->
      Printf.printf "\t%s" r.Bench.desc;
    ) l;
    Printf.printf "\n"
  );
  Printf.printf "%d" size;
  BatList.iter (fun r ->
    Printf.printf "\t%.3f" (1_000_000_000. *. r.Bench.mean.Bench.Bootstrap.point /. float size)
  ) l;
  Printf.printf "\n"

let bench ~title ?(deque=false) ?(list=false) ?(map=false) bench =
  fun size ->
    let core = if map then [
      "ListTailModConsOpt", bench size (module ListTailModConsOpt : SIG);
      "ListTailCore", bench size (module ListTailCore : SIG);
    ] else [] in
    let lists = if list then [
      "ListOverflow", bench size (module ListOverflow : SIG);
      "ListTail", bench size (module ListTail : SIG);
      "ListTailModCons", bench size (module ListTailModCons : SIG);
    ] @ core else [
    ] in
    let deque = if deque then [
      "Deque", bench size (module Deque : SIG);
    ] else [] in
    let readings =
      Bench.bench_n (lists @ deque @ [
        "FgGen", bench size (module FgGen : SIG);
        "FgGenOpt", bench size (module FgGenOpt : SIG);
        "FgSpec", bench size (module FgSpec : SIG);
        "Vect", bench size (module Vect : SIG);
      ]) in
    fun () -> print_readings ~title size readings

let heap_size ~title size =
  let assoc = [
    "ListOverflow", bench_size size (module ListOverflow : SIG);
    "Deque", bench_size size (module Deque : SIG);
    "FgGen", bench_size size (module FgGen : SIG);
    "Vect", bench_size size (module Vect : SIG);
  ] in
  fun () ->
    if size = BatList.hd sizes then (
      Printf.printf "#%s size" title;
      BatList.iter (fun (name,_) -> Printf.printf "\t%s" name) assoc;
      Printf.printf "\n"
    );
    Printf.printf "%d" size;
    BatList.iter (fun (_,size) -> Printf.printf "\t%d" size) assoc;
    Printf.printf "\n"

let benches = [
  "cons_front", bench ~list:true ~deque:true bench_cons_front;
  "snoc_front", bench ~deque:true bench_snoc_front;
  "snoc_front_rear", bench ~deque:true bench_snoc_front_rear;
  "size", heap_size;
  "map", bench ~deque:true ~list:true ~map:true bench_map;
  "of_enum", bench ~list:true ~deque:true bench_of_enum1;
  "enum", bench ~list:true ~deque:true bench_enum1;
  "fold_left", bench ~list:true ~deque:true bench_fold_left;
  "fold_right", bench ~list:true ~deque:true bench_fold_right;
  "reverse", bench ~list:true bench_reverse;
  "append", bench bench_append;
  "set", bench bench_set;
  "get", bench bench_get;
]

let () =
  Bench.config.Bench.samples <- 100;
  Array.iter (fun s ->
    try
      let f = BatList.assoc s benches in
      let printers = BatList.map (f ~title:s) sizes in
      BatList.iter (fun f -> f ()) printers;
      Printf.printf "\n"
    with Not_found ->
      Printf.printf "`%s' is not a valid bench name\nThe possibilities are: " s;
      BatList.iter (fun (name,_) -> Printf.printf "%s, " name) benches;
      Printf.printf "\n";
      exit 1
  ) (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
