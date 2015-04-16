(*
 * BatEnum - Enumeration over abstract collection of elements.
 * Copyright (C) 2003 Nicolas Cannasse
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** {6 Representation} *)

type 'a t = {
  mutable count : unit -> int; (** Return the number of remaining elements in the enumeration. *)
  mutable next  : unit -> 'a;  (** Return the next element of the enumeration or raise [No_more_elements].*)
  mutable clone : unit -> 'a t;(** Return a copy of the enumeration. *)
  mutable fast  : bool;        (** [true] if [count] can be done without reading all elements, [false] otherwise.*)
}

type 'a enumerable = 'a t
type 'a mappable = 'a t

external enum : 'a t -> 'a t = "%identity"
external of_enum : 'a t -> 'a t = "%identity"

(* raised by 'next' functions, should NOT go outside the API *)
exception No_more_elements

let make ~next ~count ~clone =
  {
    count = count;
    next  = next;
    clone = clone;
    fast  = true;
  }

(** {6 Internal utilities}*)
let _dummy () = assert false (*BISECT-VISIT*)

(* raised by 'count' functions, may go outside the API *)
exception Infinite_enum

let return_no_more_elements () = raise No_more_elements
let return_no_more_count    () = 0
let return_infinite_count   () = raise Infinite_enum

(* Inlined from ExtList to avoid circular dependencies. *)
type 'a _mut_list = {
  hd : 'a;
  mutable tl : 'a _mut_list;
}

let rec empty () =
  {
    count = return_no_more_count;
    next  = return_no_more_elements;
    clone = empty;
    fast  = true;
  }

let close e =
  e.next <- return_no_more_elements;
  e.count<- return_no_more_count;
  e.clone<- empty

let force t =(** Transform [t] into a list *)
  let rec clone enum count =
    let enum = ref !enum
    and count = ref !count in
    {
      count = (fun () -> !count);
      next  = (fun () ->
        match !enum with
        | []     -> raise No_more_elements
        | h :: t -> decr count; enum := t; h);
      clone = (fun () ->
        let enum = ref !enum
        and count = ref !count in
        clone enum count);
      fast  = true;
    }
  in
  let count = ref 0 in
  let _empty = Obj.magic [] in
  let rec loop dst =
    let x = { hd = t.next(); tl = _empty } in
    incr count;
    dst.tl <- x;
    loop x
  in
  let enum = ref _empty  in
  (try
    enum := { hd = t.next(); tl = _empty };
    incr count;
    loop !enum;
  with No_more_elements -> ());
  let tc = clone (Obj.magic enum) count in
  t.clone <- tc.clone;
  t.next <- tc.next;
  t.count <- tc.count;
  t.fast <- true

(* Inlined from {!LazyList}.

   This lazy list permits cloning enumerations constructed with {!from}
   without having to actually force them.*)
module MicroLazyList = struct
  type 'a ll_t      = ('a node_t) Lazy.t
  and  'a node_t =
    | Nil
    | Cons of 'a * 'a ll_t

  let nil = lazy Nil

  let enum l =
    let rec aux (l:'a ll_t) : 'a t=
      let reference = ref l in
      let e = make
          ~next:(fun () -> match Lazy.force !reference with
            | Cons(x,t) -> reference := t; x
            | Nil       -> raise No_more_elements )
          ~count:_dummy
          ~clone:(fun () -> aux !reference)
      in e.count <- (fun () -> force e; e.count());
      e.fast  <- false;
      e
    in aux l

  let from f =
    let rec aux () =
      lazy (
        let item = try  Some (f ())
        with No_more_elements -> None
        in match item with
        | Some x -> Cons (x, aux () )
        | _      -> Nil
      )
    in
    aux ()
end

let from f =
  let e = {
    next = _dummy;
    count = _dummy;
    clone = _dummy;
    fast = false;
  } in
  e.next  <- (fun () -> try f () with No_more_elements -> close e ; raise No_more_elements);
  e.count <- (fun () -> force e; e.count());
  e.clone <- (fun () ->
    let e' =  MicroLazyList.enum(MicroLazyList.from f) in
    e.next <- e'.next;
    e.clone<- e'.clone;
    e.count<- (fun () -> force e; e.count());
    (* we can't use [e'.count] because that would force [e'],
       which doesn't update [e].  That would for example,
       cause e.fast to not be updated to true.  A simple test
       to see the problem with [e'.count] is to do the
       following: (1) create a enum using this [from]
       function, (2) clone that enum, (3) grab the count of
       the original enum and then iterate over it.  A
       discrepancy between the count and the elements will
       result. *)
    e.fast <- e'.fast;
    e.clone () );
  e


let from2 next clone =
  let e = {
    next = next;
    count = _dummy;
    clone = clone;
    fast = false;
  } in
  e.count <- (fun () -> force e; e.count());
  e

let init n f = (*Experimental fix for init*)
  if n < 0 then invalid_arg "BatEnum.init";
  let count = ref n in
  let f' () =
    match !count with
    | 0 -> raise No_more_elements
    | _ -> decr count;
      f ( n - 1 - !count)
  in let e = from f' in
  e.fast  <- true;
  e.count <- (fun () -> !count);
  e


let get t =
  try   Some (t.next())
  with  No_more_elements -> None

let get_exn t = t.next ()

let push t e =
  let rec make t =
    let fnext = t.next in
    let fcount = t.count in
    let fclone = t.clone in
    let next_called = ref false in
    t.next <- (fun () ->
      next_called := true;
      t.next <- fnext;
      t.count <- fcount;
      t.clone <- fclone;
      e);
    t.count <- (fun () ->
      let n = fcount() in
      if !next_called then n else n+1);
    t.clone <- (fun () ->
      let tc = fclone() in
      if not !next_called then make tc;
      tc);
  in
  make t

let peek t =
  match get t with
  | None -> None
  | Some x ->
    push t x;
    Some x

module MicroList = (*Inlined from ExtList to avoid circular dependencies*)
struct
  let enum l =
    let rec aux lr count =
      make
        ~next:(fun () ->
          match !lr with
          | [] -> raise No_more_elements
          | h :: t ->
            decr count;
            lr := t;
            h
        )
        ~count:(fun () ->
          if !count < 0 then count := List.length !lr;
          !count
        )
        ~clone:(fun () ->
          aux (ref !lr) (ref !count)
        )
    in
    aux (ref l) (ref (-1))
end

let take n e =
  let r = ref [] in
  begin
    try
      for i = 1 to n do
        r := e.next () :: !r
      done
    with No_more_elements -> ()
  end;
  MicroList.enum (List.rev !r)

(*let take n e = (*Er... that looks quite weird.*)
  let remaining = ref n in
  let f () =
    if !remaining >= 0 then
      let result = e.next () in
    decr remaining;
    result
    else raise No_more_elements
  in let e = make
       ~next: f
       ~count:(fun () -> !remaining)
       ~clone:_dummy
  in e.clone <- (fun () -> force e; e.clone ());
    e*)

let junk t =
  try
    ignore(t.next())
  with
    No_more_elements -> ()

let is_empty t =
  if t.fast then
    t.count() = 0
  else
    peek t = None

let count t =
  t.count()

let fast_count t =
  t.fast



let clone t =
  t.clone()

let iter f t =
  let rec loop () =
    f (t.next());
    loop();
  in
  try
    loop();
  with
    No_more_elements -> ()

let iteri f t =
  let rec loop idx =
    f idx (t.next());
    loop (idx+1);
  in
  try
    loop 0;
  with
    No_more_elements -> ()

let iter2 f t u =
  let push_t = ref None in
  let rec loop () =
    push_t := None;
    let e = t.next() in
    push_t := Some e;
    f e (u.next());
    loop ()
  in
  try
    loop ()
  with
    No_more_elements ->
    match !push_t with
    | None -> ()
    | Some e ->
      push t e

let iter2i f t u =
  let push_t = ref None in
  let rec loop idx =
    push_t := None;
    let e = t.next() in
    push_t := Some e;
    f idx e (u.next());
    loop (idx + 1)
  in
  try
    loop 0
  with
    No_more_elements ->
    match !push_t with
    | None -> ()
    | Some e -> push t e

let fold f init t =
  let acc = ref init in
  let rec loop() =
    acc := f !acc (t.next());
    loop()
  in
  try
    loop()
  with
    No_more_elements -> !acc

let reduce f t =
  match get t
  with None -> raise Not_found
     |  Some init -> fold f init t

let sum t =
  match get t with
  | None -> 0
  | Some i -> fold (+) i t

(* Kahan summing.  [Enum.reduce (+.)] is 20% faster, but has
   cumulative error O(n) instead of O(1) *)
let fsum t =
  match get t with
  | None -> 0.
  | Some i ->
    let sum = ref i in
    let c = ref 0. in
    iter (fun x ->
      let y = x -. !c in
      let t = !sum +. y in
      c := (t -. !sum) -. y;
      sum := t
    ) t;
    !sum

let kahan_sum = fsum

(* NEED A PROPER TEST OF ROUNDING ERROR *)
(*$T fsum
   let arr = Array.make 10001 1e-10 in arr.(0) <- 1e10; \
      Float.approx_equal (fsum (Array.enum arr)) (1e10 +. 1e-5)
*)

(*$T kahan_sum
   kahan_sum (Array.enum [| |]) = 0.
   kahan_sum (Array.enum [| 1.; 2. |]) = 3.
   let n, x = 1_000, 1.1 in \
     Float.approx_equal (float n *. x) \
                        (kahan_sum (Array.enum (Array.make n x)))
*)


let exists f t =
  try let rec aux () = f (t.next()) || aux ()
    in aux ()
  with No_more_elements -> false

let for_all f t =
  try let rec aux () = f (t.next()) && aux ()
    in aux ()
  with No_more_elements -> true

(* test paired elements, ignore any extra elements from one enum *)
let for_all2 f t1 t2 =
  try
    let rec aux () = f (t1.next()) (t2.next()) && aux () in
    aux ()
  with No_more_elements -> true

let scanl f init t =
  let acc = ref init in
  let gen () =
    acc := f !acc (t.next());
    !acc
  in
  let e = from gen in
  push e init;
  e

let scan f t =
  match get t with
  | Some x -> scanl f x t
  | None   -> empty ()

let foldi f init t =
  let acc = ref init in
  let rec loop idx =
    acc := f idx (t.next()) !acc;
    loop (idx + 1)
  in
  try
    loop 0
  with
    No_more_elements -> !acc

let fold2 f init t u =
  let acc = ref init in
  let push_t = ref None in
  let rec loop() =
    push_t := None;
    let e = t.next() in
    push_t := Some e;
    acc := f e (u.next()) !acc;
    loop()
  in
  try
    loop()
  with
    No_more_elements ->
    match !push_t with
    | None -> !acc
    | Some e ->
      push t e;
      !acc

let fold2i f init t u =
  let acc = ref init in
  let push_t = ref None in
  let rec loop idx =
    push_t := None;
    let e = t.next() in
    push_t := Some e;
    acc := f idx e (u.next()) !acc;
    loop (idx + 1)
  in
  try
    loop 0
  with
    No_more_elements ->
    match !push_t with
    | None -> !acc
    | Some e ->
      push t e;
      !acc



let find f t =
  let rec loop () =
    let x = t.next() in
    if f x then x else loop()
  in
  try
    loop()
  with
    No_more_elements -> raise Not_found

(*$T
  find ((=) 5) (1 -- 10) = 5
  try ignore (find ((=) 11) (1 -- 10) = 5); false with Not_found -> true
*)

let find_map f t =
  let rec loop () =
    match f (t.next ()) with
    | Some x -> x
    | None -> loop ()
  in
  try
    loop ()
  with No_more_elements -> raise Not_found

(*$T find_map
   try let _ = empty () |> find_map (const (Some 1)) in false with Not_found -> true
   singleton 0 |> find_map (const (Some 1)) = 1
   1 -- 5 |> find_map (function 2 -> Some 0 | _ -> None) = 0
   1 -- 5 |> find_map (function 5 -> Some 0 | _ -> None) = 0
   try let _ = 1 -- 5 |> find_map (function 6 -> Some 0 | _ -> None) in \
      false with Not_found -> true
*)
(*qtest TODO: migrate try into an exception test *)

let rec map f t =
  {
    count = t.count;
    next = (fun () -> f (t.next()));
    clone = (fun () -> map f (t.clone()));
    fast = t.fast;
  }

let rec mapi f t =
  let idx = ref (-1) in
  {
    count = t.count;
    next = (fun () -> incr idx; f !idx (t.next()));
    clone = (fun () -> mapi f (t.clone()));
    fast = t.fast;
  }

let rec filter f t =
  let rec next() =
    let x = t.next() in
    if f x then x else next()
  in
  from2 next (fun () -> filter f (t.clone()))

let rec filter_map f t =
  let rec next () =
    match f (t.next()) with
    | None -> next()
    | Some x -> x
  in
  from2 next (fun () -> filter_map f (t.clone()))

let rec append ta tb =
  let t = {
    count = (fun () -> ta.count() + tb.count());
    next = _dummy;
    clone = (fun () -> append (ta.clone()) (tb.clone()));
    fast = ta.fast && tb.fast;
  } in
  t.next <- (fun () ->
    try
      ta.next()
    with
      No_more_elements ->
      (* add one indirection because tb can mute *)
      t.next <- (fun () -> tb.next());
      t.count <- (fun () -> tb.count());
      t.clone <- (fun () -> tb.clone());
      t.fast <- tb.fast;
      t.next()
  );
  t

(*$T
  append (List.enum [1;2;3]) (List.enum [4;5]) |> List.of_enum = [1;2;3;4;5]
  append (List.enum [1;2;3]) (List.enum [4;5]) |> \
    mapi (Tuple2.curry identity) |> List.of_enum = [0,1;1,2;2,3;3,4;4,5]

*)

let prefix_action f t =
  let full_action e =
    e.count <- (fun () -> t.count());
    e.next  <- (fun () -> t.next ());
    e.clone <- (fun () -> t.clone());
    f ()
  in
  let rec t' =
    {
      count = (fun () -> full_action t'; t.count() );
      next  = (fun () -> full_action t'; t.next()  );
      clone = (fun () -> full_action t'; t.clone() );
      fast  = t.fast
    } in t'


let suffix_action_without_raise (f:unit -> 'a) (t:'a t) =
  {
    count = t.count;
    next  = (fun () ->
      try  t.next ()
      with No_more_elements -> f() );
    clone = (fun () -> t.clone());  (* needs to be delayed because [t] may
                                       mutate and we want the newest clone
                                       function *)
    fast  = t.fast
  }

let suffix_action f t =
  let f' () = f (); close t; raise No_more_elements in
  suffix_action_without_raise f' t


let rec concat t =
  let tn = ref (empty ()) in
  let rec next () =
    try (!tn).next ()
    with No_more_elements -> tn := t.next(); next()
  in
  let clone () = append ((!tn).clone()) (concat (t.clone())) in
  from2 next clone

(*$T concat
  let e = List.enum [ [| 1; 2; 3; 4|]; [| 5; 6 |] ] |> map Array.enum \
    |> concat in drop 1 e; (count e) = (count (clone e))
*)


let singleton x =
  init 1 (fun _ -> x)

let switchn n f e =
  let queues = ArrayLabels.init n ~f:(fun _ -> Queue.create ())   in
  let gen i () = (*Generate the next value for the i^th enum*)
    let my_queue  = queues.(i) in
    if Queue.is_empty my_queue then (*Need to fetch next*)
      let rec aux () =     (*Keep fetching until an appropriate
                                 item has been found*)
        let next_item = e.next()    in
        let position  = f next_item in
        if  i = position then next_item
        else
          (
            Queue.push next_item queues.(position);
            aux ()
          )
      in aux ()
    else Queue.take my_queue
  in ArrayLabels.init ~f:(fun i -> from (gen i)) n

let switch f e =
  let a = switchn 2 (fun x -> if f x then 0 else 1) e in
  (a.(0), a.(1))

(*$T
  List.enum [1;2;3;4] |> switch (fun x -> x mod 2 = 0) |> \
    Tuple2.mapn List.of_enum = ([2;4], [1;3])
*)

let partition = switch

(*$T partition
   let a,b = partition (fun x -> x > 3) (List.enum [1;2;3;4;5;1;5;0]) in \
      List.of_enum a = [4;5;5] && List.of_enum b = [1;2;3;1;0]
*)

(*$Q partition
   (Q.list Q.small_int) (fun l -> let f x = x mod 2 = 1 in List.partition f l \
      = (partition f (List.enum l) |> Tuple.Tuple2.mapn List.of_enum))
*)

let seq init f cond =
  let acc = ref init in
  let aux () = if cond !acc then begin
      let result = !acc in
      acc := f !acc;
      result
    end
    else raise No_more_elements
  in from aux

let repeat ?times x = match times with
  | None ->
    let rec aux =
      {
        count = return_infinite_count;
        next  = (fun () -> x);
        clone = (fun () -> aux);
        fast  = true;
      } in aux
  | Some n ->
    init n (fun _ -> x)

(*$T
  repeat ~times:5 0 |> List.of_enum = [0;0;0;0;0]
  repeat 1 |> take 3 |> List.of_enum = [1;1;1]
*)

let cycle ?times x =
  let enum   =
    match times with
    | None   -> from (fun () -> clone x)
    | Some n -> init n (fun _ -> clone x)
  in concat enum

(*$T
  cycle ~times:5 (singleton 1) |> List.of_enum = [1;1;1;1;1]
  cycle (List.enum [1;2]) |> take 5 |> List.of_enum = [1;2;1;2;1]
*)

let range ?until x =
  let cond =  match until with
    | None   -> ( fun _ -> true   )
    | Some n -> ( fun m -> m <= n )
  in seq x ( ( + ) 1 ) cond

(*$T
  range 1 ~until:5 |> List.of_enum = [1;2;3;4;5]
*)

let drop n e =
  for i = 1 to n do
    junk e
  done

let skip n e =
  drop n e; e

let drop_while p e =
  let rec aux () =
    match get e with
    | Some x when p x -> aux ()
    | Some x          -> push e x
    | None            -> ()
  in prefix_action aux e

(*let drop_while p e =
  let rec aux () =
    let x = e.next () in
      print_string "filtering\n";
      if p x then (aux ())
      else (push e x;
        raise No_more_elements)
  in
    append (from aux) e*)


let take_while f t =
  let next () =
    let x = t.next () in
    if f x then x
    else
      (push t x;
       raise No_more_elements)
  in from next


let span f t =
  (*Two possibilities: either the tail has been read
    already -- in which case all head data has been
    copied onto the queue -- or the tail hasn't been
    read -- in which case, stuff should be read from
    [t] *)
  let queue           = Queue.create ()
  and read_from_queue = ref false in
  let head () =
    if !read_from_queue then (*Everything from the head has been copied *)
      try  Queue.take queue  (*to the queue already                     *)
      with Queue.Empty -> raise No_more_elements
    else let x = t.next () in
      if f x then x
      else (push t x;
        raise No_more_elements)
  and tail () =
    if not !read_from_queue then (*Copy everything to the queue         *)
      begin
        read_from_queue := true;
        let rec aux () =
          match get t with
          | None            -> raise No_more_elements
          | Some x when f x -> Queue.push x queue; aux ()
          | Some x          -> x
        in aux ()
      end
    else t.next()
  in
  (from head, from tail)

(*$T span
  List.enum [1;2;3;4;5] |> span (fun x-> x<4) |> Tuple2.mapn List.of_enum = \
    ([1;2;3], [4;5])
  *)

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let avg = List.fold_left (+) 0 l / (max 1 @@ List.length l) in \
    let l' = List.sort Int.compare l in \
    let f x = x < avg in \
    Tuple2.mapn List.of_enum (span f @@ List.enum l' ) = \
    (List.of_enum @@ take_while f @@ List.enum l', \
     List.of_enum @@ drop_while f @@ List.enum l'))
*)

let while_do cont f e =
  let (head, tail) = span cont e in
  append (f head) tail

let break test e = span (fun x -> not (test x)) e

let uniq_by cmp e =
  match peek e with
    None -> empty ()
  | Some first ->
    let prev = ref first in
    let not_last x = not (cmp (BatRef.post prev (fun _ -> x)) x) in
    let result = filter not_last e in
    push result first;
    result

let uniq e =
  uniq_by (=) e

let uniqq e =
  uniq_by (==) e


(*$T
  List.enum [1;1;2;3;3;2] |> uniq |> List.of_enum = [1;2;3;2]
  List.enum [1;1;2;3;3;2] |> uniqq |> List.of_enum = [1;2;3;2]
  List.enum ["a";"a";"b";"c";"c";"b"] |> uniq |> List.of_enum = ["a";"b";"c";"b"]
  List.enum ["a";"A";"b";"c";"C";"b"] \
    |> uniq_by (fun a b -> String.lowercase a = String.lowercase b) \
    |> List.of_enum = ["a";"b";"c";"b"]
*)

let dup t      = (t, t.clone())

(*$Q
  (Q.list Q.small_int) (fun l -> \
    List.enum l |> dup |> Tuple2.mapn List.of_enum |> Tuple2.uncurry (=))
*)

let min_count x y =
  let count x = try Some (x.count ()) with Infinite_enum -> None in
  match count x, count y with
    | None, None -> raise Infinite_enum
    | Some c, None | None, Some c -> c
    | Some c1, Some c2 -> min c1 c2

let combine (x,y) =
  if x.fast && y.fast then (*Optimized case*)
    let rec aux (x,y) =
      {
        count = (fun () -> min_count x y)                 ;
        next  = (fun () -> (x.next(), y.next()))          ;
        clone = (fun () -> aux (x.clone(), y.clone()))    ;
        fast  = true
      }
    in aux (x,y)
  else from (fun () -> (x.next(), y.next()))

(*$T
  combine (List.enum [1;2;3], List.enum ["a";"b"]) \
    |> List.of_enum = [1, "a"; 2, "b"]
  combine (List.enum [1;2;3], repeat "a") \
    |> List.of_enum = [1,"a"; 2,"a"; 3,"a"]
  combine (List.enum [1;2;3], repeat "a") \
    |> Enum.count = 3
*)

let uncombine e =
  let advance    = ref `first
  and queue_snd  = Queue.create ()
  and queue_fst  = Queue.create () in
  let first () = match !advance with
    | `first ->
      let (x,y) = e.next() in
      Queue.push y queue_snd;
      x
    | `second-> (*Second element has been read further*)
      try  Queue.pop queue_fst
      with Queue.Empty ->
        let (x,y) = e.next()  in
        Queue.push y queue_snd;
        advance := `first;
        x
  and second() = match !advance with
    | `second ->
      let (x,y) = e.next() in
      Queue.push x queue_fst;
      y
    | `first  -> (*Second element has been read further*)
      try Queue.pop queue_snd
      with Queue.Empty ->
        let (x,y) = e.next()  in
        Queue.push x queue_fst;
        advance := `second;
        y
  in (from first, from second)

(*$R uncombine
  let pair_list = [1,2;3,4;5,6;7,8;9,0] in
  let a,b = uncombine (BatList.enum pair_list) in
  let a = BatArray.of_enum a in
  let b = BatArray.of_enum b in
  let c,d = uncombine (BatList.enum pair_list) in
  let d = BatArray.of_enum d in
  let c = BatArray.of_enum c in
  let aeq = assert_equal ~printer:(BatIO.to_string (BatArray.print BatInt.print)) in
  aeq a [|1;3;5;7;9|];
  aeq b [|2;4;6;8;0|];
  aeq a c;
  aeq b d
*)

let group_aux test eq e =
  let prev_group = ref (empty ()) in
  let f () =
    (* Make sure elements belonging to prev group are consumed from e *)
    force !prev_group;
    let grp =
      let last_test = ref None in
      let check_test t =
        let ok =
          match !last_test with
          | None -> true
          | Some t' -> eq t' t
        in
        if ok then
          last_test := Some t;
        ok
      in
      take_while (fun x -> check_test (test x)) e
    in
    if is_empty grp then
      raise No_more_elements;
    prev_group := grp;
    grp
  in
  let clone () =
    failwith "Grouped enumerations cannot be cloned safely"
  in
  from2 f clone

let group test e =
  group_aux test (=) e

let group_by eq e =
  group_aux (fun x -> x) eq e

(*$T group
   empty () |> group (const ()) |> is_empty
   List.enum [1;2;3;4] |> group identity |> map List.of_enum \
    |> List.of_enum = [[1];[2];[3];[4]]
   List.enum [1;2;3;4] |> group (const true) |> List.of_enum \
    |> List.map List.of_enum = [[1;2;3;4]]
   List.enum [1;2;3;5;6;7;9;10;4;5] |> group (fun x -> x mod 2) |> List.of_enum \
    |> List.map List.of_enum = [[1];[2];[3;5];[6];[7;9];[10;4];[5]]
*)

let clump clump_size add get e = (* convert a uchar enum into a ustring enum *)
  let next () =
    match peek e with
    | None   -> raise No_more_elements
    | Some x ->
      add x;
      junk e; (* don't get [x] twice *)
      (try
        for i = 2 to clump_size do
          add (e.next ())
        done
      with No_more_elements -> ());
      get ()
  in
  from next

(*$T clump
  let l = RefList.empty() in \
  Char.range 'a' ~until:'k' |> \
  clump 4 (RefList.push l) \
    (fun()-> String.implode \
      (RefList.to_list l |> tap (fun _ -> RefList.clear l) |> List.rev)) \
  |> List.of_enum = ["abcd"; "efgh"; "ijk"]
*)

(* mutable state used for {!cartesian_product}. Use a module to have a private namespace. *)
module ProductState = struct
  type ('a, 'b) current_state =
    | GetLeft
    | GetRight
    | GetRightOrStop
    | Stop
    | ProdLeft of 'a * 'b list
    | ProdRight of 'b * 'a list

  type ('a,'b) t = {
    e1 : 'a enumerable;
    e2 : 'b enumerable;
    mutable all1 : 'a list;
    mutable all2 : 'b list;
    mutable cur : ('a,'b) current_state;
  }
end

let cartesian_product e1 e2 =
  let open ProductState in
  (* sketch of the algo: state machine that alternates between taking a
     new element from [e1] and yield its product with [state.all2], and
     taking a new element from [e2] and make its product with [state.all1]

     [state.cur]: current state of automaton, i.e., what we have to do next.
     Can be `Stop,
     `GetLeft/`GetRight (to obtain next element from first/second generator),
     or `ProdLeft/`ProdRIght to compute the product of an element with a list
     of already met elements *)
  let rec next state () =
    match state.cur with
    | Stop -> raise No_more_elements
    | GetLeft ->
      let x1 = try Some (state.e1.next()) with No_more_elements -> None in
      begin match x1 with
        | None -> state.cur <- GetRightOrStop
        | Some x ->
          state.all1 <- x :: state.all1;
          state.cur <- ProdLeft (x, state.all2)
      end;
      next state ()
    | GetRight | GetRightOrStop ->
      let x2 = try Some (state.e2.next()) with No_more_elements -> None in
      begin match x2, state.cur with
        | None, GetRightOrStop -> state.cur <- Stop; raise No_more_elements
        | None, GetRight -> state.cur <- GetLeft
        | Some y, _ ->
          state.all2 <- y::state.all2;
          state.cur <- ProdRight (y, state.all1)
        | None, _ -> assert false
      end;
      next state ()
    | ProdLeft (_, []) ->
      state.cur <- GetRight;
      next state ()
    | ProdLeft (x, y::l) ->
      state.cur <- ProdLeft (x, l);
      x, y
    | ProdRight (_, []) ->
      state.cur <- GetLeft;
      next state()
    | ProdRight (y, x::l) ->
      state.cur <- ProdRight (y, l);
      x, y
  and clone state () =
    let state' = {state with e1=state.e1.clone(); e2=state.e2.clone();} in
    _make state'
  and count state () =
    let n1 = state.e1.count ()
    and n2 = state.e2.count () in
    (* 3 products to make: e1 with e2, and ei with all{2-i} for i in {1,2} *)
    let n = n1 * n2 + n1 * List.length state.all2 + n2 * List.length state.all1 in
    match state.cur with
    | ProdRight (_, l) -> n + List.length l
    | ProdLeft (_, l) -> n + List.length l
    | Stop -> 0
    | GetLeft | GetRight | GetRightOrStop -> n
  (* build enum from the state *)
  and _make state = {
    next = next state;
    clone = clone state;
    count = count state;
    fast = state.e1.fast && state.e2.fast;
  }
  in
  let state = {e1; e2; cur=GetLeft; all1=[]; all2=[]} in
  _make state

(*$T cartesian_product
  cartesian_product (List.enum [1;2;3]) (List.enum ["a";"b"]) \
    |> List.of_enum |> List.sort Pervasives.compare = \
    [1,"a"; 1,"b"; 2,"a"; 2,"b"; 3,"a"; 3,"b"]
  let e = cartesian_product (List.enum [1;2;3]) (List.enum [1]) in \
    e |> List.of_enum |> List.sort Pervasives.compare = [1,1; 2,1; 3,1]
  let e = cartesian_product (List.enum [1]) (List.enum [1;2;3]) in \
    e |> List.of_enum |> List.sort Pervasives.compare = [1,1; 1,2; 1,3]
  let e = cartesian_product (List.enum [1;2;3]) (List.enum [1;2;3]) in \
    ignore (Enum.get e); Enum.count e = 8
  let e = cartesian_product (List.enum [1;2]) (Enum.repeat 3) in\
    e |> Enum.take 4 |> Enum.map fst |> List.of_enum \
      |> List.sort Pervasives.compare = [1; 1; 2; 2]
  let e = cartesian_product (Enum.repeat 3) (List.enum [1;2]) in\
    e |> Enum.take 4 |> Enum.map snd |> List.of_enum \
      |> List.sort Pervasives.compare = [1; 1; 2; 2]
  let e = cartesian_product (Enum.repeat 3) (Enum.repeat "a") in\
    e |> Enum.take 3 |> List.of_enum \
      |> List.sort Pervasives.compare = [3, "a"; 3, "a"; 3, "a"]
*)

(*$Q cartesian_product
  Q.(pair (list small_int) (list small_int)) \
    (fun (l1,l2) -> \
      let l1 = List.take 5 l1 in \
      let l2 = List.take 4 l2 in \
      cartesian_product (List.enum l1) (List.enum l2) |> count = \
      List.length l1 * List.length l2)
  Q.(pair (list small_int) (list small_int)) \
    (fun (l1,l2) -> \
      let l1 = List.take 5 l1 in \
      let l2 = List.take 4 l2 in \
      cartesian_product (List.enum l1) (List.enum l2) \
      |> List.of_enum |> List.length = List.length l1 * List.length l2)
*)

let from_while f =
  from (fun () -> match f () with
    | None   -> raise No_more_elements
    | Some x -> x )


let from_loop data next =
  let r = ref data in
  from(fun () -> let (a,b) = next !r in
    r := b;
    a)

let unfold data next =
  from_loop data (fun data -> match next data with
    | None   -> raise No_more_elements
    | Some x -> x )

let arg_min f enum =
  match get enum with
    None -> invalid_arg "arg_min: Empty enum"
  | Some v ->
    let item, eval = ref v, ref (f v) in
    iter (fun v -> let fv = f v in
      if fv < !eval then (item := v; eval := fv)) enum;
    !item

let arg_max f enum =
  match get enum with
    None -> invalid_arg "arg_max: Empty enum"
  | Some v ->
    let item, eval = ref v, ref (f v) in
    iter (fun v -> let fv = f v in
      if fv > !eval then (item := v; eval := fv)) enum;
    !item

(*$T arg_max
   List.enum ["cat"; "canary"; "dog"; "dodo"; "ant"; "cow"] \
      |> arg_max String.length = "canary"
*)
(*$T arg_min
   -5 -- 5 |> arg_min (fun x -> x * x + 6 * x - 5) = -3
 *)

module Infix = struct
  let ( -- ) x y = range x ~until:y

  let ( --. ) (a, step) b =
    let n = int_of_float ((b -. a) /. step) + 1 in
    if n < 0 then
      empty ()
    else
      init n (fun i -> float_of_int i *. step +. a)

  let ( --^ ) x y = range x ~until:(y-1)

  let ( --- ) x y =
    if x <= y then x -- y
    else          seq x ((+) (-1)) ( (<=) y )

  let ( --~ ) a b = map Char.chr (range (Char.code a) ~until:(Char.code b))

  let ( // ) e f = filter f e

  let ( /@ ) e f        = map f e
  let ( @/ )            = map
  let ( //@ ) e f       = filter_map f e
  let ( @// )           = filter_map
end
include Infix

(* -----------
   Concurrency
*)

let append_from a b =
  let t    = from (fun () -> a.next())       in
  let f () = let result = b.next ()          in
    t.next <- (fun () -> b.next ());
    result
  in
  suffix_action_without_raise f t



let merge test a b =
  if   is_empty a    then b
  else if is_empty b then a
  else let next_a = ref (a.next())
  and next_b = ref (b.next()) in
    let aux () =
      let (n, na, nb) =
        if test !next_a !next_b then
          try (!next_a, a.next(), !next_b)
          with No_more_elements -> (*a is exhausted, b probably not*)
            push b !next_b;
            push b !next_a;
            raise No_more_elements
        else
          try (!next_b, !next_a, b.next())
          with No_more_elements -> (*b is exhausted, a probably not*)
            push a !next_a;
            push a !next_b;
            raise No_more_elements
      in next_a := na;
      next_b := nb;
      n
    in append_from (append_from (from aux) a) b

(*$T
  let a=List.enum [1;3;5] and b = List.enum[2;4] in \
  let test = let r = ref false in (fun _ _ -> r:= not !r; !r) in \
  merge test a b |> List.of_enum = [1;2;3;4;5]
*)

(*let mergen test a =
  ArrayLabels.fold_left ~init:[]
    ~f:(fun x ->
  let Array.of_list a
  let next = Array.map
  let rec aux =
    if Array.length !next = 1 then (*we're done*)
    if *)


let interleave enums =
  let enums_len = Array.length enums in
  if not (enums_len > 0) then empty () else begin
    let available = Array.make enums_len true
    and next_idx = Array.init enums_len ((+) 1) in
    next_idx.((Array.length next_idx) - 1) <- 0 ;
    let rec next_elem idx =
      match get enums.(idx) with
       | Some x -> x , next_idx.(idx)
       | None -> begin
           available.(idx) <- false ;
           let rec loop k =
             let l = next_idx.(k) in
             if l = idx then raise No_more_elements else
             if available.(l) then (next_idx.(idx) <- l ; next_elem l) else loop l
           in loop idx
         end
    in
    from_loop 0 next_elem
  end

(*$T interleave
  let e1 = List.enum [ 8 ; 2 ; 5 ; 2 ] and e2 = List.enum [ -5 ; -7 ; -6 ; 2 ; 1 ; -9 ; 2 ] in \
  let e = interleave [| e1 ; e2 |] in \
  List.of_enum e = [ 8 ; -5 ; 2 ; -7 ; 5 ; -6 ; 2 ; 2 ; 1 ; -9 ; 2 ]
*)

(*$R interleave
  let e1 = Enum.empty ()
  and e2 = List.enum [ 8 ; 2 ; 5 ; 2 ]
  and e3 = List.enum [ -5 ; -7 ; -6 ; 2 ; 1 ; -9 ; 2 ] in
  let e = interleave [| e1; e2 ; e3 |] in
  assert_equal (List.of_enum e) [ 8 ; -5 ; 2 ; -7 ; 5 ; -6 ; 2 ; 2 ; 1 ; -9 ; 2 ]
*)

(*$R interleave
  let e1 = Enum.empty ()
  and e2 = Enum.empty ()
  and e3 = Enum.empty () in
  let e = interleave [| e1; e2 ; e3 |] in
  assert_equal (List.of_enum e) [ ]
*)

let slazy f =
  let constructor = lazy (f ()) in
  make ~next: (fun () -> (Lazy.force constructor).next ())
    ~count:   (fun () -> (Lazy.force constructor).count())
    ~clone:   (fun () -> (Lazy.force constructor).clone())

let delay = slazy

let lsing f =
  init 1 (fun _ -> f ())



let lcons f e = append (lsing f) e
let lapp  f e = append (slazy f) e

let ising     = singleton
let icons f e = append (ising f) e
let iapp      = append

let hard_count t =
  if t.fast then
    let result = t.count () in
    close t;
    result
  else (*Counting would cache stuff, which we don't want here.*)
    let length = ref 0 in
    try while true do ignore (t.next()); incr length done; assert false
    with No_more_elements -> !length

(* common hidden function for print and print_at_most *)
let _print_common ~first ~last ~sep ~limit print_a out e =
  BatInnerIO.nwrite out first;
  match get e with
  | None    -> BatInnerIO.nwrite out last
  | Some x  ->
    print_a out x;
    let rec aux limit =
      match get e, limit with
      | None, _ -> BatInnerIO.nwrite out last
      | Some _, 0 ->
        BatInnerIO.nwrite out "...";
        BatInnerIO.nwrite out last
      | Some x, _ ->
        BatInnerIO.nwrite out sep;
        print_a out x;
        aux (limit-1)
    in aux (limit-1)

let print ?(first="") ?(last="") ?(sep=" ") print_a  out e =
  _print_common ~first ~last ~sep ~limit:max_int print_a out e

let print_at_most ?(first="") ?(last="") ?(sep=" ") ~limit print_a out e =
  if limit <= 0 then raise (Invalid_argument "enum.print_at_most");
  _print_common ~first ~last ~sep ~limit print_a out e

(*$T print_at_most
  Printf.sprintf2 "yolo %a" (print_at_most ~limit:3 Int.print) \
    (range 0 ~until:10) = "yolo 0 1 2..."
*)

let t_printer a_printer _paren out e =
  print ~first:"[" ~sep:"; " ~last:"]" (a_printer false) out e

let compare cmp t u =
  let rec aux () =
    match (get t, get u) with
    | (None, None)     -> 0
    | (None, _)        -> -1
    | (_, None)        -> 1
    | (Some x, Some y) -> match cmp x y with
      | 0 -> aux ()
      | n -> n
  in aux ()

let ord ord_val t u =
  let cmp_val = BatOrd.comp ord_val in
  BatOrd.ord0 (compare cmp_val t u)

let equal eq t u =
  let rec aux () =
    match (get t, get u) with
    | (None, None)     -> true
    | (Some x, Some y) -> eq x y && aux ()
    | _ -> false
  in aux ()

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let e = List.enum l in equal Int.equal e (clone e))
*)

let rec to_object t =
  object
    method next = t.next ()
    method count= count t
    method clone = to_object (clone t)
  end

let rec of_object o =
  make ~next:(fun () -> o#next)
    ~count:(fun () -> o#count)
    ~clone:(fun () -> of_object (o#clone))

let flatten = concat

(*$T
  flatten (map singleton @@ List.enum [1;2;3]) |> List.of_enum = [1;2;3]
*)

let rec concat_map f t =
  let tn = ref (empty ()) in
  let rec next () =
    try (!tn).next ()
    with No_more_elements -> tn := f (t.next()); next()
  in
  let clone () = append ((!tn).clone()) (concat_map f (t.clone())) in
  from2 next clone

(*$T concat_map
  (1 -- 10 |> concat_map (fun x -> List.enum [x;-x]) |> sum) = 0
  let e = (1 -- 10 |> concat_map (fun x -> List.enum [x;-x])) in \
    let n = Enum.count e in \
    n = (List.of_enum e |> List.length)
  let e = (1 -- 10 |> concat_map (fun x -> List.enum [x;-x])) in \
    Enum.count e = 20
*)

(*$Q concat_map
  Q.small_int (fun i -> \
    let i = abs i in \
    equal (=) (0 -- i) (concat_map singleton (0 -- i)))
 *)

module Exceptionless = struct
  let find f e =
    try  Some (find f e)
    with Not_found -> None
end

module Labels = struct
  let iter ~f x     = iter f x
  let iter2 ~f x y  = iter2 f x y
  let iteri ~f x    = iteri f x
  let iter2i ~f x y = iter2i f x y
  let for_all ~f t  = for_all f t
  let exists ~f t   = exists f t
  let fold ~f ~init x    = fold f init x
  let fold2 ~f ~init x y = fold2 f init x y
  let foldi ~f ~init x   = foldi f init x
  let fold2i ~f ~init x y= fold2i f init x y
  let find ~f x    = find f x
  let map ~f x     = map f x
  let mapi ~f x    = mapi f x
  let filter ~f x  = filter f x
  let filter_map ~f x= filter_map f x
  let init x ~f    = init x f
  let switch ~f    = switch f
  let take_while ~f  = take_while f
  let drop_while ~f  = drop_while f
  let from ~f        = from f
  let from_loop ~init ~f = from_loop init f
  let from_while ~f  = from_while f
  let seq ~init ~f ~cnd  = seq init f cnd
  let unfold ~init ~f = unfold init f
  let compare ?(cmp=Pervasives.compare) t u = compare cmp t u
  let uniq ?(cmp=(=)) x = uniq_by cmp x
  module LExceptionless = struct
    include Exceptionless
    let find ~f e = find f e
  end
end

module type Enumerable = sig
  type 'a enumerable
  val enum : 'a enumerable -> 'a t
  val of_enum : 'a t -> 'a enumerable
end

module WithMonad (Mon : BatInterfaces.Monad) =
struct
  type 'a m = 'a Mon.m

  let sequence enum =
    let (>>=) = Mon.bind and return = Mon.return in
    (* We use a list as an accumulator for the result sequence
       computed under the monad. A previous version of this code used
       a Queue instead, which was problematic for backtracking
       monads. Due to the destructive nature of Enums, the current
       version will still be problematic but at least the result will
       be consistent. *)
    let of_acc acc =
      (* we don't use List functions to avoid creating a cyclic
         dependency *)
      let li = ref (List.rev acc) in
      from (fun () ->
        match !li with
        | [] -> raise No_more_elements
        | hd::tl ->
          li := tl;
          hd)
    in
    let rec loop acc = match get enum with
      | None -> return (of_acc acc)
      | Some elem -> elem >>= (fun x -> loop (x :: acc))
    in
    loop []

  let fold_monad f init enum =
    let (>>=) = Mon.bind and return = Mon.return in
    let rec fold m = match get enum with
      | None -> m
      | Some x -> m >>= fun acc -> fold (f acc x)
    in
    fold (return init)
end

module Monad =
struct
  type 'a m = 'a t
  let return x = singleton x
  let bind m f = concat (map f m)
end

(*$T
  equal (=) (Monad.return 1) (singleton 1)
  equal (=) (Monad.bind (List.enum [1;2]) (fun x-> List.enum [x+1;x])) \
    (List.enum [2;1;3;2])
*)

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let id l = Monad.bind l Monad.return in  \
    List.enum l |> id |> List.of_enum = l)
*)

module Incubator = struct
  open BatOrd

  let int_eq (x:int) y = x = y
  let int_ord (x:int) y =
    if x > y then Gt
    else if y > x then Lt
    else Eq

  let eq_elements eq_elt a1 a2 = for_all2 eq_elt a1 a2

  let rec ord_elements ord_elt t u =
    match (get t, get u) with
    | (None, None)     -> Eq
    | (None, _)        -> Lt
    | (_, None)        -> Gt
    | (Some x, Some y) -> match ord_elt x y with
      | Eq -> ord_elements ord_elt t u
      | (Gt|Lt) as n -> n

  let eq eq_elt t1 t2 =
    bin_eq
      int_eq (count t1) (count t2)
      (eq_elements eq_elt) t1 t2

  let ord ord_elt t1 t2 =
    bin_ord
      int_ord (count t1) (count t2)
      (ord_elements ord_elt) t1 t2

  module Eq (T : Eq) = struct
    type 'a enum = 'a t
    type t = T.t enum
    let eq = eq T.eq
  end

  module Ord (T : Ord) = struct
    type 'a enum = 'a t
    type t = T.t enum
    let ord = ord T.ord
  end

end
