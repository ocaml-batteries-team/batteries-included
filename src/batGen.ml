(*
 * BatInt32 - Extended 32-bit integers
 * Copyright (C) 2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
 *               2008 David Teller
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

(** Internal definition of generators *)

type 'a t = unit -> 'a option
(** Consumable generator of values of type 'a. Returns [None]
    constantly once it is exhausted. *)

type 'a gen = 'a t

type 'a restart_gen = unit -> 'a t
(** Restartable generator. Every time it is invoked with [()], a new
    generator is created and can be consumed. All created generators
    {b must} be equivalent. *)

module type Enumerable = sig
  type 'a enumerable

  val gen : 'a enumerable -> 'a gen
  val of_gen : 'a gen -> 'a enumerable
end

(** {2 Common signature for transient and restartable generators} *)

module type S = sig
  type 'a t

  val empty : 'a t
    (** Empty generator, with no elements *)

  val singleton : 'a -> 'a t
    (** One-element generator *)

  val repeat : 'a -> 'a t
    (** Repeat same element endlessly *)

  val iterate : 'a -> ('a -> 'a) -> 'a t
    (** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]] *)

  val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
    (** Dual of {!fold}, with a deconstructing operation. It keeps on
        unfolding the ['b] value into a new ['b], and a ['a] which is yielded,
        until [None] is returned. *)

  val init : ?limit:int -> (int -> 'a) -> 'a t
    (** Calls the function, starting from 0, on increasing indices.
        If [limit] is provided and is a positive int, iteration will
        stop at the limit (excluded).
        For instance [init ~limit:4 id] will yield 0, 1, 2, and 3. *)

  val seq : 'a -> ('a -> 'a) -> ('a -> bool) -> 'a t
  (** [seq init step cond] creates a sequence of data, which starts
      from [init],  extends by [step],  until the condition [cond]
      fails. E.g. [seq 1 ((+) 1) ((>) 100)] returns [1, 2, ... 99]. If [cond
      init] is false, the result is empty. *)

  (** {2 Basic combinators} *)

  val is_empty : _ t -> bool
    (** Check whether the enum is empty. *)

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** Fold on the generator, tail-recursively *)

  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
    (** Fold on non-empty sequences (otherwise raise Invalid_argument) *)

  val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
    (** Like {!fold}, but keeping successive values of the accumulator *)

  val iter : ('a -> unit) -> 'a t -> unit
    (** Iterate on the enum *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
    (** Iterate on elements with their index in the enum, from 0 *)

  val length : _ t -> int
    (** Length of an enum (linear time) *)

  val map : ('a -> 'b) -> 'a t -> 'b t
    (** Lazy map. No iteration is performed now, the function will be called
        when the result is traversed. *)

  val append : 'a t -> 'a t -> 'a t
    (** Append the two enums; the result contains the elements of the first,
        then the elements of the second enum. *)

  val flatten : 'a gen t -> 'a t
    (** Flatten the enumeration of generators *)

  val flat_map : ('a -> 'b gen) -> 'a t -> 'b t
    (** Monadic bind; each element is transformed to a sub-enum
        which is then iterated on, before the next element is processed,
        and so on. *)

  val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
    (** Is the given element, member of the enum? *)

  val take : int -> 'a t -> 'a t
    (** Take at most n elements *)

  val drop : int -> 'a t -> 'a t
    (** Drop n elements *)

  val nth : int -> 'a t -> 'a
    (** n-th element, or Not_found
        @raise Not_found if the generator contains less than [n] arguments *)

  val take_nth : int -> 'a t -> 'a t
    (** [take_nth n g] returns every element of [g] whose index
        is a multiple of [n]. For instance [take_nth 2 (1--10) |> to_list]
        will return [1;3;5;7;9] *)

  val filter : ('a -> bool) -> 'a t -> 'a t
    (** Filter out elements that do not satisfy the predicate.  *)

  val take_while : ('a -> bool) -> 'a t -> 'a t
    (** Take elements while they satisfy the predicate *)

  val drop_while : ('a -> bool) -> 'a t -> 'a t
    (** Drop elements while they satisfy the predicate *)

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
    (** Maps some elements to 'b, drop the other ones *)

  val zip_index : 'a t -> (int * 'a) t
    (** Zip elements with their index in the enum *)

  val unzip : ('a * 'b) t -> 'a t * 'b t
    (** Unzip into two sequences, splitting each pair *)

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p l] returns the elements that satisfy [p],
        and the elements that do not satisfy [p] *)

  val for_all : ('a -> bool) -> 'a t -> bool
    (** Is the predicate true for all elements? *)

  val exists : ('a -> bool) -> 'a t -> bool
    (** Is the predicate true for at least one element? *)

  val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
    (** Minimum element, according to the given comparison function.
        @raise Invalid_argument if the generator is empty *)

  val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
    (** Maximum element, see {!min}
        @raise Invalid_argument if the generator is empty *)

  val eq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** Equality of generators. *)

  val lexico : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Lexicographic comparison of generators. If a generator is a prefix
        of the other one, it is considered smaller. *)

  val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Synonym for {! lexico} *)

  val find : ('a -> bool) -> 'a t -> 'a option
    (** [find p e] returns the first element of [e] to satisfy [p],
        or None. *)

  val sum : int t -> int
    (** Sum of all elements *)

  (** {2 Multiple iterators} *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** Map on the two sequences. Stops once one of them is exhausted.*)

  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
    (** Iterate on the two sequences. Stops once one of them is exhausted.*)

  val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
    (** Fold the common prefix of the two iterators *)

  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** Succeeds if all pairs of elements satisfy the predicate.
        Ignores elements of an iterator if the other runs dry. *)

  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** Succeeds if some pair of elements satisfy the predicate.
        Ignores elements of an iterator if the other runs dry. *)

  val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** Combine common part of the enums (stops when one is exhausted) *)

  val zip : 'a t -> 'b t -> ('a * 'b) t
    (** Zip together the common part of the enums *)

  (** {2 Complex combinators} *)

  val merge : 'a gen t -> 'a t
    (** Pick elements fairly in each sub-generator. The merge of enums
        [e1, e2, ... ] picks elements in [e1], [e2],
        in [e3], [e1], [e2] .... Once a generator is empty, it is skipped;
        when they are all empty, and none remains in the input,
        their merge is also empty. 
        For instance, [merge [1;3;5] [2;4;6]] will be, in disorder, [1;2;3;4;5;6]. *)

  val intersection : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
    (** Intersection of two sorted sequences. Only elements that occur in both
        inputs appear in the output *)

  val sorted_merge : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
    (** Merge two sorted sequences into a sorted sequence *)

  val sorted_merge_n : ?cmp:('a -> 'a -> int) -> 'a t list -> 'a t
    (** Sorted merge of multiple sorted sequences *)

  val tee : ?n:int -> 'a t -> 'a gen list
    (** Duplicate the enum into [n] generators (default 2). The generators
        share the same underlying instance of the enum, so the optimal case is
        when they are consumed evenly *)

  val round_robin : ?n:int -> 'a t -> 'a gen list
    (** Split the enum into [n] generators in a fair way. Elements with
        [index = k mod n] with go to the k-th enum. [n] default value
        is 2. *)

  val interleave : 'a t -> 'a t -> 'a t
    (** [interleave a b] yields an element of [a], then an element of [b],
        and so on. When a generator is exhausted, this behaves like the
        other generator. *)

  val intersperse : 'a -> 'a t -> 'a t
    (** Put the separator element between all elements of the given enum *)

  val product : 'a t -> 'b t -> ('a * 'b) t
    (** Cartesian product, in no predictable order. Works even if some of the
        arguments are infinite. *)

  val group : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
    (** Group equal consecutive elements together. *)

  val group_by : ('a -> 'b) -> 'a t -> 'a gen t
    (** group together consecutive elements that have the same
        image by the given function *)

  val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
    (** Remove consecutive duplicate elements. Basically this is
        like [fun e -> map List.hd (group e)]. *)

  val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
    (** Sort according to the given comparison function. The enum must be finite. *)

  val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
    (** Sort and remove duplicates. The enum must be finite. *)

  val chunks : int -> 'a t -> 'a array t
    (** [chunks n e] returns a generator of arrays of length [n], composed
        of successive elements of [e]. The last array may be smaller
        than [n] *)

  (* TODO later
  val permutations : 'a t -> 'a gen t
    (** Permutations of the enum. Each permutation becomes unavailable once
        the next one is produced. *)

  val combinations : int -> 'a t -> 'a t t
    (** Combinations of given length. *)

  val powerSet : 'a t -> 'a t t
    (** All subsets of the enum (in no particular order) *)
  *)

  (** {2 Basic conversion functions} *)

  val of_list : 'a list -> 'a t
    (** Enumerate elements of the list *)

  val to_list : 'a t -> 'a list
    (** non tail-call trasnformation to list, in the same order *)

  val to_rev_list : 'a t -> 'a list
    (** Tail call conversion to list, in reverse order (more efficient) *)

  val to_array : 'a t -> 'a array
    (** Convert the enum to an array (not very efficient) *)

  val of_array : ?start:int -> ?len:int -> 'a array -> 'a t
    (** Iterate on (a slice of) the given array *)

  val rand_int : int -> int t
    (** Random ints in the given range. *)

  val int_range : int -> int -> int t
    (** [int_range a b] enumerates integers between [a] and [b], included. [a]
        is assumed to be smaller than [b]. *)

  module Infix : sig
    val (--) : int -> int -> int t
      (** Synonym for {! int_range} *)

    val (>>=) : 'a t -> ('a -> 'b gen) -> 'b t
      (** Monadic bind operator *)
  end

  val (--) : int -> int -> int t
    (** Synonym for {! int_range} *)

  val (>>=) : 'a t -> ('a -> 'b gen) -> 'b t
    (** Monadic bind operator *)

  val pp : ?start:string -> ?stop:string -> ?sep:string -> ?horizontal:bool ->
           (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    (** Pretty print the content of the generator on a formatter. *)
end

(* inlined here *)
let __of_list l =
  let l = ref l in
  fun () ->
    match !l with
    | [] -> None
    | x::l' -> l := l'; Some x

let print ?(first="") ?(last="") ?(sep=", ") f oc g =
  let rec pp i = match g () with
    | None -> BatInnerIO.write_string oc last
    | Some x ->
      if i > 0 then BatInnerIO.write_string oc sep;
      f oc x;
      pp (i+1)
   in
   BatInnerIO.write_string oc first; pp 0

(* unfold: f returns an option *)
let from_loop init f =
  let stop = ref false in
  let data = ref init in
  fun () ->
    if !stop then None else match f !data with
    | None -> stop:=true; data:=Obj.magic 0; None  (* free data*)
    | Some (x, data') -> data:= data'; Some x

(** {2 Transient generators} *)

let empty () = None

(*$T empty
  empty |> to_list = []
*)

let singleton x =
  let first = ref true in
  fun () ->
    if !first then (first := false; Some x) else None

(*T singleton
  singleton 1 |> to_list = [1]
  singleton "foo" |> to_list = ["foo"]
*)

let rec repeat x () = Some x

(*$T repeat
  repeat 42 |> take 3 |> to_list = [42; 42; 42]
*)

let repeatedly f () = Some (f ())

(*$T repeatedly
  repeatedly (let r = ref 0 in fun () -> incr r; !r) \
    |> take 5 |> to_list = [1;2;3;4;5]
*)

let iterate x f =
  let cur = ref x in
  fun () ->
    let x = !cur in
    cur := f !cur;
    Some x

(*$T iterate
  iterate 0 ((+)1) |> take 5 |> to_list = [0;1;2;3;4]
*)

let next gen = gen ()

let get gen = gen ()

let get_exn gen =
  match gen () with
  | Some x -> x
  | None -> raise (Invalid_argument "Gen.get_exn")

(*$R get_exn
  let g = of_list [1;2;3] in
  assert_equal 1 (get_exn g);
  assert_equal 2 (get_exn g);
  assert_equal 3 (get_exn g);
  assert_raises (Invalid_argument "Gen.get_exn") (fun () -> get_exn g)
*)

let junk gen = ignore (gen ())

let rec fold f acc gen =
  match gen () with
  | None -> acc
  | Some x -> fold f (f acc x) gen

(*$Q
  (Q.list Q.small_int) (fun l -> \
    of_list l |> fold (fun l x->x::l) [] = List.rev l)
*)

let reduce f g =
  let acc = match g () with
    | None -> raise (Invalid_argument "reduce")
    | Some x -> x
  in 
  fold f acc g

(* Dual of {!fold}, with a deconstructing operation *)
let unfold f acc =
  let acc = ref acc in
  fun () ->
    match f !acc with
    | None -> None
    | Some (x, acc') ->
      acc := acc';
      Some x

(*$T unfold
  unfold (fun (prev,cur) -> Some (prev, (cur,prev+cur))) (0,1) \
    |> take 7 |> to_list = [0; 1; 1; 2; 3; 5; 8]
*)

let init ?(limit=max_int) f =
  let r = ref 0 in
  fun () ->
    if !r >= limit
    then None
    else
      let x = f !r in
      let _ = incr r in
      Some x

(*$T init
  init ~limit:5 (fun i->i) |> to_list = [0;1;2;3;4]
*)

let seq start f cond =
  let cur = ref start in
  fun () ->
    if not (cond !cur) then None
    else (let x = !cur in cur := f !cur; Some x)

(*$T seq
  seq 0 ((+) 1) (fun x -> x>= 5) |> to_list = [0;1;2;3;4]
*)

let rec iter f gen =
  match gen() with
  | None -> ()
  | Some x -> f x; iter f gen

let iteri f gen =
  let rec iteri i = match gen() with
  | None -> ()
  | Some x -> f i x; iteri (i+1)
  in
  iteri 0

let is_empty gen = match gen () with
  | None -> true
  | Some _ -> false

(*$T
  is_empty empty
  not (is_empty (singleton 2))
*)

let length gen =
  fold (fun acc _ -> acc + 1) 0 gen

(*$Q
  (Q.list Q.small_int) (fun l -> \
    of_list l |> length = List.length l)
*)

(* useful state *)
type 'a run_state =
  | Init
  | Run of 'a
  | Stop

let scan f acc g =
  let state = ref Init in
  fun () ->
    match !state with
    | Init ->
        state := Run acc;
        Some acc
    | Stop -> None
    | Run acc ->
        match g() with
        | None -> state := Stop; None
        | Some x ->
            let acc' = f acc x in
            state := Run acc';
            Some acc'

(*$T scan
  scan (fun acc x -> x+1::acc) [] (1--5) |> to_list \
    = [[]; [2]; [3;2]; [4;3;2]; [5;4;3;2]; [6;5;4;3;2]]
*)

let rec iter2 f gen1 gen2 =
  match gen1(), gen2() with
  | Some x, Some y -> f x y; iter2 f gen1 gen2
  | _ -> ()

(*$T iter2
  let r = ref 0 in iter2 (fun _ _ -> incr r) (1--10) (4--6); !r = 3
*)

(** {3 Lazy} *)

let map f gen =
  let stop = ref false in
  fun () ->
    if !stop then None
    else match gen() with
    | None -> stop:= true; None
    | Some x -> Some (f x)

(*$Q map
  (Q.list Q.small_int) (fun l -> \
    let f x = x*2 in \
    of_list l |> map f |> to_list = List.map f l)
*)

let append gen1 gen2 =
  let first = ref true in
  let rec next() =
    if !first
    then match gen1() with
    | (Some _) as x -> x
    | None -> first:=false; next()
    else gen2()
  in next

(*$Q
  (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
    append (of_list l1) (of_list l2) |> to_list = l1 @ l2)
*)

let flatten next_gen =
  let state = ref Init in
  (* get next element *)
  let rec next () =
    match !state with
    | Init -> get_next_gen()
    | Run gen ->
      begin match gen () with
      | None -> get_next_gen ()
      | (Some _) as x -> x
      end
    | Stop -> None
  and get_next_gen() = match next_gen() with
    | None -> state := Stop; None
    | Some gen -> state := Run gen; next()
  in
  next

let flat_map f next_elem =
  let state = ref Init in
  let rec next() =
    match !state with
    | Init -> get_next_gen()
    | Run gen ->
      begin match gen () with
      | None -> get_next_gen ()
      | (Some _) as x -> x
      end
    | Stop -> None
  and get_next_gen() = match next_elem() with
    | None -> state:=Stop; None
    | Some x ->
        try state := Run (f x); next()
        with e -> state := Stop; raise e
  in
  next

(*$Q flat_map
  (Q.list Q.small_int) (fun l -> \
    let f x = of_list [x;x*2] in \
    eq (map f (of_list l) |> flatten) (flat_map f (of_list l)))
*)

let mem ?(eq=(=)) x gen =
  let rec mem eq x gen =
    match gen() with
    | Some y -> eq x y || mem eq x gen
    | None -> false
  in mem eq x gen

let take n gen =
  assert (n >= 0);
  let count = ref 0 in  (* how many yielded elements *)
  fun () ->
    if !count = n || !count = ~-1
    then None
    else match gen() with
      | None -> count := ~-1; None   (* indicate stop *)
      | (Some _) as x -> incr count; x

(*$Q
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (n,l) -> \
    of_list l |> take n |> length = Pervasives.min n (List.length l))
*)

(* call [gen] at most [n] times, and stop *)
let rec __drop n gen =
  if n = 0 then ()
  else match gen() with
    | Some _ -> __drop (n-1) gen
    | None -> ()

let drop n gen =
  assert (n >= 0);
  let dropped = ref false in
  fun () ->
    if !dropped
      then gen()
      else begin
        (* drop [n] elements and yield the next element *)
        dropped := true;
        __drop n gen;
        gen()
      end

(*$Q
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (n,l) -> \
    let g1,g2 = take n (of_list l), drop n (of_list l) in \
    append g1 g2 |> to_list = l)
*)

let nth n gen =
  assert (n>=0);
  __drop n gen;
  match gen () with
  | None -> raise Not_found
  | Some x -> x

(*$= nth & ~printer:string_of_int
  4 (nth 4 (0--10))
  8 (nth 8 (0--10))
*)

(*$T
  (try ignore (nth 11 (1--10)); false with Not_found -> true)
*)

let take_nth n gen =
  assert (n>=1);
  let i = ref n in
  let rec next() =
    match gen() with
    | None -> None
    | (Some _) as res when !i = n -> i:=1; res
    | Some _ -> incr i; next()
  in next

let filter p gen =
  let rec next () =
    (* wrap exception into option, for next to be tailrec *)
    match gen() with
    | None -> None
    | (Some x) as res ->
      if p x
        then res (* yield element *)
        else next ()  (* discard element *)
  in next

(*$T
  filter (fun x ->x mod 2 = 0) (1--10) |> to_list = [2;4;6;8;10]
*)

let take_while p gen =
  let stop = ref false in
  let rec next () =
    if !stop
    then None
    else match gen() with
    | (Some x) as res ->
        if p x then res else (stop := true; None)
    | None -> stop:=true; None
  in next

(*$T
  take_while (fun x ->x<10) (1--1000) |> eq (1--9)
*)

module Drop_whileState = struct
  type t =
    | Stop
    | Drop
    | Yield
end

let drop_while p gen =
  let open Drop_whileState in
  let state = ref Drop in
  let rec next () =
    match !state with
    | Stop -> None
    | Drop ->
        begin match gen () with
        | None -> state := Stop; None
        | (Some x) as res ->
            if p x then next() else (state:=Yield; res)
        end
    | Yield ->
        begin match gen () with
        | None -> state := Stop; None
        | (Some x) as res -> res
        end
  in next

(*$T
  drop_while (fun x-> x<10) (1--20) |> eq (10--20)
*)

let filter_map f gen =
  (* tailrec *)
  let rec next () =
    match gen() with
    | None -> None
    | Some x ->
        match f x with
        | None -> next()
        | (Some _) as res -> res
  in next

(*$T
  filter_map (fun x-> if x mod 2 = 0 then Some (string_of_int x) else None) (1--10) \
    |> to_list = List.map string_of_int [2;4;6;8;10]
*)

let zip_index gen =
  let r = ref ~-1 in
  fun () ->
    match gen() with
    | None -> None
    | Some x ->
        incr r;
        Some (!r, x)

(*$T
  zip_index (1--5) |> to_list = [0,1; 1,2; 2,3; 3,4; 4,5]
*)

let unzip gen =
  let stop = ref false in
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let next_left () =
    if Queue.is_empty q1
      then if !stop then None
      else match gen() with
      | Some (x,y) ->
        Queue.push y q2;
        Some x
      | None -> stop := true; None
    else Some (Queue.pop q1)
  in
  let next_right () =
    if Queue.is_empty q2
      then if !stop then None
      else match gen() with
      | Some (x,y) ->
        Queue.push x q1;
        Some y
      | None -> stop := true; None
    else Some (Queue.pop q2)
  in
  next_left, next_right

(*$T
  unzip (of_list [1,2;3,4]) |> (fun (x,y)-> to_list x, to_list y) \
    = ([1;3], [2;4])
*)

(*$Q
  (Q.list (Q.pair Q.small_int Q.small_int)) (fun l -> \
    of_list l |> unzip |> (fun (x,y) -> to_list x,to_list y) = \
    List.split l)
*)

(* [partition p l] returns the elements that satisfy [p],
   and the elements that do not satisfy [p] *)
let partition p gen =
  let qtrue = Queue.create () in
  let qfalse = Queue.create () in
  let stop = ref false in
  let rec nexttrue () =
    if Queue.is_empty qtrue
      then if !stop then None
      else match gen() with
      | (Some x) as res ->
        if p x then res else (Queue.push x qfalse; nexttrue())
      | None -> stop:=true; None
    else Some (Queue.pop qtrue)
  and nextfalse() =
    if Queue.is_empty qfalse
      then if !stop then None
      else match gen() with
      | (Some x) as res ->
        if p x then (Queue.push x qtrue; nextfalse()) else res
      | None -> stop:= true; None
    else Some (Queue.pop qfalse)
  in
  nexttrue, nextfalse

(*$T
  partition (fun x -> x mod 2 = 0) (1--10) |> \
    (fun (x,y)->to_list x, to_list y) = ([2;4;6;8;10], [1;3;5;7;9])
*)

let rec for_all p gen =
  match gen() with
  | None -> true
  | Some x -> p x && for_all p gen

let rec exists p gen =
  match gen() with
  | None -> false
  | Some x -> p x || exists p gen

let min ?(lt=fun x y -> x < y) gen =
  let first = match gen () with
    | Some x -> x
    | None -> raise (Invalid_argument "min")
  in
  fold (fun min x -> if lt x min then x else min) first gen

(*$T
  min (of_list [1;4;6;0;11; -2]) = ~-2
  (try ignore (min empty); false with Invalid_argument _ -> true)
*)

let max ?(lt=fun x y -> x < y) gen =
  let first = match gen () with
    | Some x -> x
    | None -> raise (Invalid_argument "max")
  in
  fold (fun max x -> if lt max x then x else max) first gen

(*$T
  max (of_list [1;4;6;0;11; -2]) = 11
  (try ignore (max empty); false with Invalid_argument _ -> true)
*)

let eq ?(eq=(=)) gen1 gen2 =
  let rec check () =
    match gen1(), gen2() with
    | None, None -> true
    | Some x1, Some x2 when eq x1 x2 -> check ()
    | _ -> false
  in
  check ()

(*$Q
  (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
    eq (of_list l1)(of_list l2) = (l1 = l2))
*)

let lexico ?(cmp=Pervasives.compare) gen1 gen2 =
  let rec lexico () =
    match gen1(), gen2() with
    | None, None -> 0
    | Some x1, Some x2 ->
      let c = cmp x1 x2 in
      if c <> 0 then c else lexico ()
    | Some _, None -> 1
    | None, Some _ -> -1
  in lexico ()

let compare ?cmp gen1 gen2 = lexico ?cmp gen1 gen2

(*$Q
  (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
    let sign x = if x < 0 then -1 else if x=0 then 0 else 1 in \
    sign (compare (of_list l1)(of_list l2)) = sign (Pervasives.compare l1 l2))
*)

let rec find p e = match e () with
  | None -> None
  | Some x when p x -> Some x
  | Some _ -> find p e

(*$T
   find (fun x -> x>=5) (1--10) = Some 5
   find (fun x -> x>5) (1--4) = None
*)

let sum e =
  let rec sum acc = match e() with
  | None -> acc
  | Some x -> sum (x+acc)
  in sum 0

(*$T
  sum (1--10) = 55
*)

(** {2 Multiple Iterators} *)

let map2 f e1 e2 =
  fun () -> match e1(), e2() with
  | Some x, Some y -> Some (f x y)
  | _ -> None

(*$T
  map2 (+) (1--5) (1--4) |> eq (of_list [2;4;6;8])
  map2 (+) (1--5) (repeat 0) |> eq (1--5)
*)

let rec iter2 f e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> f x y; iter2 f e1 e2
  | _ -> ()

let rec fold2 f acc e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> fold2 f (f acc x y) e1 e2
  | _ -> acc

let rec for_all2 p e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> p x y && for_all2 p e1 e2
  | _ -> true

let rec exists2 p e1 e2 =
  match e1(), e2() with
  | Some x, Some y -> p x y || exists2 p e1 e2
  | _ -> false

let zip_with f a b =
  let stop = ref false in
  fun () ->
    if !stop then None
    else match a(), b() with
    | Some xa, Some xb -> Some (f xa xb)
    | _ -> stop:=true; None

let zip a b = zip_with (fun x y -> x,y) a b

(*$Q
  (Q.list Q.small_int) (fun l -> \
    zip_with (fun x y->x,y) (of_list l) (of_list l) \
      |> unzip |> fst |> to_list = l)
*)

(** {3 Complex combinators} *)

module MergeState = struct
  type 'a t = {
    gens : 'a gen Queue.t;
    mutable state : my_state;
  }

  and my_state =
    | NewGen
    | YieldAndNew
    | Yield
    | Stop
end

(* state machine:
    (NewGen -> YieldAndNew)* // then no more generators in next_gen, so
    -> Yield* -> Stop *)
let merge next_gen =
  let open MergeState in
  let state = {gens = Queue.create(); state=NewGen;}in
  (* recursive function to get next element *)
  let rec next () =
    match state.state with
    | Stop -> None
    | Yield ->  (* only yield from generators in state.gens *)
        if Queue.is_empty state.gens
        then (state.state <- Stop; None)
        else
          let gen = Queue.pop state.gens in
          begin match gen () with
          | None -> next()
          | (Some _) as res ->
              Queue.push gen state.gens;  (* put gen back in queue *)
              res
        end
    | NewGen ->
        begin match next_gen() with
        | None ->
            state.state <- Yield;  (* exhausted *)
            next()
        | Some gen ->
            Queue.push gen state.gens;
            state.state <- YieldAndNew;
            next()
        end
    | YieldAndNew -> (* yield element from queue, then get a new generator *)
        if Queue.is_empty state.gens
        then (state.state <- NewGen; next())
        else
          let gen = Queue.pop state.gens in
          begin match gen () with
          | None -> state.state <- NewGen; next()
          | (Some _) as res ->
              Queue.push gen state.gens;
              state.state <- NewGen;
              res
          end
  in next

(*$T
  merge (of_list [of_list [1;3;5]; of_list [2;4;6]; of_list [7;8;9]]) \
    |> to_list |> List.sort Pervasives.compare = [1;2;3;4;5;6;7;8;9]
*)

let intersection ?(cmp=Pervasives.compare) gen1 gen2 =
  let x1 = ref (gen1 ()) in
  let x2 = ref (gen2 ()) in
  let rec next () =
    match !x1, !x2 with
    | Some y1, Some y2 ->
      let c = cmp y1 y2 in
      if c = 0  (* equal elements, yield! *)
        then (x1 := gen1(); x2 := gen2(); Some y1)
      else if c < 0 (* drop y1 *)
        then (x1 := gen1 (); next ())
      else (* drop y2 *)
        (x2 := gen2(); next ())
    | _ -> None
  in next

(*$T
  intersection (of_list [1;1;2;3;4;8]) (of_list [1;2;4;5;6;7;8;9]) \
    |> to_list = [1;2;4;8]
*)

let sorted_merge ?(cmp=Pervasives.compare) gen1 gen2 =
  let x1 = ref (gen1 ()) in
  let x2 = ref (gen2 ()) in
  fun () ->
    match !x1, !x2 with
    | None, None -> None
    | (Some y1)as r1, ((Some y2) as r2) ->
      if cmp y1 y2 <= 0
        then (x1 := gen1 (); r1)
        else (x2 := gen2 (); r2)
    | (Some _)as r, None ->
      x1 := gen1 ();
      r
    | None, ((Some _)as r) ->
      x2 := gen2 ();
      r

(*$T
  sorted_merge (of_list [1;2;2;3;5;10;100]) (of_list [2;4;5;6;11]) \
    |> to_list = [1;2;2;2;3;4;5;5;6;10;11;100]
*)

(** {4 Mutable heap (taken from heap.ml to avoid dependencies)} *)
module Heap = struct
  type 'a t = {
    mutable tree : 'a tree;
    cmp : 'a -> 'a -> int;
  } (** A pairing tree heap with the given comparison function *)
  and 'a tree =
    | Empty
    | Node of 'a * 'a tree * 'a tree

  let empty ~cmp = {
    tree = Empty;
    cmp;
  }

  let is_empty h =
    match h.tree with
    | Empty -> true
    | Node _ -> false

  let rec union ~cmp t1 t2 = match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | Node (x1, l1, r1), Node (x2, l2, r2) ->
    if cmp x1 x2 <= 0
      then Node (x1, union ~cmp t2 r1, l1)
      else Node (x2, union ~cmp t1 r2, l2)

  let insert h x =
    h.tree <- union ~cmp:h.cmp (Node (x, Empty, Empty)) h.tree

  let pop h = match h.tree with
    | Empty -> raise Not_found
    | Node (x, l, r) ->
      h.tree <- union ~cmp:h.cmp l r;
      x
end

let sorted_merge_n ?(cmp=Pervasives.compare) l =
  (* make a heap of (value, generator) *)
  let cmp (v1,_) (v2,_) = cmp v1 v2 in
  let heap = Heap.empty ~cmp in
  (* add initial values *)
  List.iter
    (fun gen' -> match gen'() with
    | Some x -> Heap.insert heap (x, gen')
    | None -> ())
    l;
  fun () ->
    if Heap.is_empty heap then None
    else begin
      let x, gen = Heap.pop heap in
      match gen() with
      | Some y ->
        Heap.insert heap (y, gen);  (* insert next value *)
        Some x
      | None -> Some x (* gen empty, drop it *)
    end

(*$T
  sorted_merge_n [of_list [1;2;2;3;5;10;100]; of_list [2;4;5;6;11]; (6--10)] \
    |> to_list = [1;2;2;2;3;4;5;5;6;6;7;8;9;10;10;11;100]
*)

let round_robin ?(n=2) gen =
  (* array of queues, together with their index *)
  let qs = Array.init n (fun i -> Queue.create ()) in
  let cur = ref 0 in
  (* get next element for the i-th queue *)
  let rec next i =
    let q = qs.(i) in
    if Queue.is_empty q
      then update_to_i i  (* consume generator *)
      else Some(Queue.pop q)
  (* consume [gen] until some element for [i]-th generator is
     available. *)
  and update_to_i i =
    match gen() with
    | None -> None
    | Some x ->
      let j = !cur in
      cur := (j+1) mod n;  (* move cursor to next generator *)
      let q = qs.(j) in
      if j = i
        then begin
          assert (Queue.is_empty q);
          Some x  (* return the element *)
        end else begin
          Queue.push x q;
          update_to_i i  (* continue consuming [gen] *)
        end
  in
  (* generators *)
  let l = Array.mapi (fun i _ -> (fun () -> next i)) qs in
  Array.to_list l

(*$T
  round_robin ~n:3 (1--12) |> List.map to_list = \
    [[1;4;7;10]; [2;5;8;11]; [3;6;9;12]]
*)

(* Duplicate the enum into [n] generators (default 2). The generators
   share the same underlying instance of the enum, so the optimal case is
   when they are consumed evenly *)
let tee ?(n=2) gen =
  (* array of queues, together with their index *)
  let qs = Array.init n (fun i -> Queue.create ()) in
  let finished = ref false in (* is [gen] exhausted? *)
  (* get next element for the i-th queue *)
  let rec next i =
    if Queue.is_empty qs.(i)
      then
        if !finished then None
        else get_next i  (* consume generator *)
      else Queue.pop qs.(i)
  (* consume one more element *)
  and get_next i = match gen() with
    | (Some x) as res ->
      for j = 0 to n-1 do
        if j <> i then Queue.push res qs.(j)
      done;
      res
    | None -> finished := true; None
  in
  (* generators *)
  let l = Array.mapi (fun i _ -> (fun () -> next i)) qs in
  Array.to_list l

(*$T
  tee ~n:3 (1--12) |> List.map to_list = \
    [to_list (1--12); to_list (1--12); to_list (1--12)]
*)


module InterleaveState = struct
  type 'a t =
    | Only of 'a gen 
    | Both of 'a gen * 'a gen * bool ref
    | Stop
end

(* Yield elements from a and b alternatively *)
let interleave gen_a gen_b =
  let open InterleaveState in
  let state = ref (Both (gen_a, gen_b, ref true)) in
  let rec next() = match !state with
  | Stop -> None
  | Only g ->
      begin match g() with
        | None -> state := Stop; None
        | (Some _) as res -> res
      end
  | Both (g1, g2, r) ->
      match (if !r then g1() else g2()) with
      | None ->
          state := if !r then Only g2 else Only g1;
          next()
      | (Some _) as res ->
          r := not !r; (* swap *)
          res
  in next

(*$T
  interleave (repeat 0) (1--5) |> take 10 |> to_list = \
    [0;1;0;2;0;3;0;4;0;5]
*)

module IntersperseState = struct
  type 'a t =
    | Start
    | YieldElem of 'a option
    | YieldSep of 'a option  (* next val *)
    | Stop
end

(* Put [x] between elements of [enum] *)
let intersperse x gen =
  let open IntersperseState in
  let state = ref Start in
  let rec next() = match !state with
    | Stop -> None
    | YieldElem res ->
        begin match gen() with
        | None -> state := Stop
        | Some _ as res' -> state := YieldSep res'
        end;
        res
    | YieldSep res ->
        state := YieldElem res;
        Some x
    | Start ->
        match gen() with
        | None -> state := Stop; None
        | Some _ as res -> state := YieldElem res; next()
  in next

(*$T
  intersperse 0 (1--5) |> to_list = [1;0;2;0;3;0;4;0;5]
*)

(* Cartesian product *)
let product gena genb =
  let all_a = ref [] in
  let all_b = ref [] in
  (* cur: current state, i.e., what we have to do next. Can be stop,
    getLeft/getRight (to obtain next element from first/second generator),
    or prodLeft/prodRIght to compute the product of an element with a list
    of already met elements *)
  let cur = ref `GetLeft in
  let rec next () =
    match !cur with
    | `Stop -> None
    | `GetLeft ->
      begin match gena() with
        | None -> cur := `GetRightOrStop
        | Some a -> all_a := a :: !all_a; cur := `ProdLeft (a, !all_b)
      end;
      next ()
    | `GetRight | `GetRightOrStop ->  (* TODO: test *)
      begin match genb() with
        | None when !cur = `GetRightOrStop -> cur := `Stop
        | None -> cur := `GetLeft
        | Some b -> all_b := b::!all_b; cur := `ProdRight (b, !all_a)
      end;
      next ()
    | `ProdLeft (_, []) ->
      cur := `GetRight;
      next()
    | `ProdLeft (x, y::l) ->
      cur := `ProdLeft (x, l);
      Some (x, y)
    | `ProdRight (_, []) ->
      cur := `GetLeft;
      next()
    | `ProdRight (y, x::l) ->
      cur := `ProdRight (y, l);
      Some (x, y)
  in
  next

(*$T
  product (1--3) (of_list ["a"; "b"]) |> to_list \
    |> List.sort Pervasives.compare = \
      [1, "a"; 1, "b"; 2, "a"; 2, "b"; 3, "a"; 3, "b"]
*)

(* Group equal consecutive elements together. *)
let group ?(eq=(=)) gen =
  match gen() with
  | None -> empty
  | Some x ->
    let cur = ref [x] in
    let rec next () =
      (* try to get an element *)
      let next_x = if !cur = [] then None else gen() in
      match next_x, !cur with
      | None, [] -> None
      | None, l ->
        cur := [];  (* stop *)
        Some l
      | Some x, y::_ when eq x y ->
        cur := x::!cur;
        next ()  (* same group *)
      | Some x, l ->
        cur := [x];
        Some l
    in next

(*$T
  group (of_list [0;0;0;1;0;2;2;3;4;5;5;5;5;10]) |> to_list = \
    [[0;0;0];[1];[0];[2;2];[3];[4];[5;5;5;5];[10]]
*)

let group_by f gen =
  match gen() with
  | None -> empty
  | Some x ->
    let cur = ref [x] in
    let rec next () =
      (* try to get an element *)
      let next_x = if !cur = [] then None else gen() in
      match next_x, !cur with
      | None, [] -> None
      | None, l ->
        cur := [];  (* stop *)
        Some (__of_list l)
      | Some x, y::_ when f x = f y ->
        cur := x::!cur;
        next ()  (* same group *)
      | Some x, l ->
        cur := [x];
        Some (__of_list l)
    in next
(*$T
  group_by (fun x -> x mod 2=0) \
    (of_list [0;0;0;1;0;2;2;3;4;6;5;5;5;5;10]) |> map to_list |> to_list = \
    [[0;0;0];[1];[0;2;2];[3];[4;6];[5;5;5;5];[10]]
*)

let uniq ?(eq=(=)) gen =
  let state = ref Init in
  let rec next() = match !state with
    | Stop -> None
    | Init ->
        begin match gen() with
        | None -> state:= Stop; None
        | (Some x) as res -> state := Run x; res
        end
    | Run x ->
        begin match gen() with
        | None -> state:= Stop; None
        | (Some y) as res ->
            if eq x y
            then next()   (* ignore duplicate *)
            else (state := Run y; res)
        end
  in next

(*$T
  uniq (of_list [0;0;0;1;0;2;2;3;4;5;5;5;5;10]) |> to_list = \
    [0;1;0;2;3;4;5;10]
*)

let sort ?(cmp=Pervasives.compare) gen =
  (* build heap *)
  let h = Heap.empty ~cmp in
  iter (Heap.insert h) gen;
  fun () ->
    if Heap.is_empty h
      then None
      else Some (Heap.pop h)
(*$T
  sort (of_list [0;0;0;1;0;2;2;3;4;5;5;5;-42;5;10]) |> to_list = \
    [-42;0;0;0;0;1;2;2;3;4;5;5;5;5;10]
*)


(* NOTE: using a set is not really possible, because once we have built the
  set there is no simple way to iterate on it *)
let sort_uniq ?(cmp=Pervasives.compare) gen =
  uniq ~eq:(fun x y -> cmp x y = 0) (sort ~cmp gen)

(*$T
  sort_uniq (of_list [0;0;0;1;0;2;2;3;4;5;42;5;5;42;5;10]) |> to_list = \
    [0;1;2;3;4;5;10;42]
*)

let chunks n e =
  let rec next () =
    match e() with
    | None -> None
    | Some x ->
        let a = Array.make n x in
        fill a 1

  and fill a i =
    (* fill the array. [i]: current index to fill *)
    if i = n
    then Some a
    else match e() with
    | None -> Some (Array.sub a 0 i)  (* last array is not full *)
    | Some x ->
        a.(i) <- x;
        fill a (i+1)
  in
  next

(*$T
  chunks 25 (0--100) |> map Array.to_list |> to_list = \
    List.map to_list [(0--24); (25--49);(50--74);(75--99);(100--100)]
*)

(*
let permutations enum =
  failwith "not implemented" (* TODO *)

let combinations n enum =
  assert (n >= 0);
  failwith "not implemented" (* TODO *)

let powerSet enum =
  failwith "not implemented"
*)

(** {3 Conversion} *)

let of_list l = __of_list l

let to_rev_list gen =
  fold (fun acc x -> x :: acc) [] gen

(*$Q
  (Q.list Q.small_int) (fun l -> \
    to_rev_list (of_list l) = List.rev l)
*)

let to_list gen = List.rev (to_rev_list gen)

let to_array gen =
  let l = to_rev_list gen in
  match l with
  | [] -> [| |]
  | _ ->
    let a = Array.of_list l in
    let n = Array.length a in
    (* reverse array *)
    for i = 0 to (n-1) / 2 do
      let tmp = a.(i) in
      a.(i) <- a.(n-i-1);
      a.(n-i-1) <- tmp
    done;
    a

let of_array ?(start=0) ?len a =
  let len = match len with
  | None -> Array.length a - start
  | Some n -> assert (n + start < Array.length a); n in
  let i = ref start in
  fun () ->
    if !i >= start + len
      then None
      else (let x = a.(!i) in incr i; Some x)

(*$Q
  (Q.array Q.small_int) (fun a -> \
    of_array a |> to_array = a)
*)

let rand_int i =
  repeatedly (fun () -> Random.int i)

let int_range i j =
  let r = ref i in
  fun () ->
    let x = !r in
    if x > j then None
      else begin
        incr r;
        Some x
      end

let pp ?(start="") ?(stop="") ?(sep=",") ?(horizontal=false) pp_elem formatter gen =
  (if horizontal
    then Format.pp_open_hbox formatter ()
    else Format.pp_open_hvbox formatter 0);
  Format.pp_print_string formatter start;
  let rec next is_first =
    match gen() with
    | Some x ->
      if not is_first
        then begin
          Format.pp_print_string formatter sep;
          Format.pp_print_space formatter ();
          pp_elem formatter x
        end else pp_elem formatter x;
        next false
    | None -> ()
  in
  next true;
  Format.pp_print_string formatter stop;
  Format.pp_close_box formatter ()

module Infix = struct
  let (--) = int_range

  let (>>=) x f = flat_map f x
end

include Infix

module Restart = struct
  type 'a t = unit -> 'a gen

  type 'a restartable = 'a t

  let lift f e = f (e ())
  let lift2 f e1 e2 = f (e1 ()) (e2 ())

  let empty () = empty

  let singleton x () = singleton x

  let iterate x f () = iterate x f

  let repeat x () = repeat x

  let repeatedly f () = repeatedly f

  let unfold f acc () = unfold f acc 

  let init ?limit f () = init ?limit f

  let seq start step cond () = seq start step cond

  let cycle enum =
    assert (not (is_empty (enum ())));
    fun () ->
      let gen = ref (enum ()) in  (* start cycle *)
      let rec next () =
        match (!gen) () with
        | (Some _) as res -> res
        | None -> gen := enum(); next()
      in next

  let is_empty e = is_empty (e ())

  let fold f acc e = fold f acc (e ())

  let reduce f e = reduce f (e ())

  let scan f acc e () = scan f acc (e ())

  let iter f e = iter f (e ())

  let iteri f e = iteri f (e ())

  let length e = length (e ())

  let map f e () = map f (e ())

  let append e1 e2 () = append (e1 ()) (e2 ())

  let flatten e () = flatten (e ())

  let flat_map f e () = flat_map f (e ())

  let mem ?eq x e = mem ?eq x (e ())

  let take n e () = take n (e ())


  let drop n e () = drop n (e ())

  let nth n e = nth n (e ())

  let take_nth n e () = take_nth n (e ())

  let filter p e () = filter p (e ())

  let take_while p e () = take_while p (e ())

  let drop_while p e () = drop_while p (e ())

  let filter_map f e () = filter_map f (e ())

  let zip_with f e1 e2 () = zip_with f (e1 ()) (e2 ())

  let zip e1 e2 () = zip (e1 ()) (e2 ())

  let zip_index e () = zip_index (e ())

  let unzip e = map fst e, map snd e

  let partition p e =
    filter p e, filter (fun x -> not (p x)) e

  let for_all p e =
    for_all p (e ())

  let exists p e =
    exists p (e ())

  let for_all2 p e1 e2 =
    for_all2 p (e1 ()) (e2 ())

  let exists2 p e1 e2 =
    exists2 p (e1 ()) (e2 ())

  let map2 f e1 e2 () =
    map2 f (e1()) (e2())

  let iter2 f e1 e2 =
    iter2 f (e1()) (e2())

  let fold2 f acc e1 e2 =
    fold2 f acc (e1()) (e2())

  let min ?lt e = min ?lt (e ())

  let max ?lt e = max ?lt (e ())

  let ___eq = eq
  let eq ?eq e1 e2 = ___eq ?eq (e1 ()) (e2 ())

  let lexico ?cmp e1 e2 = lexico ?cmp (e1 ()) (e2 ())

  let compare ?cmp e1 e2 = compare ?cmp (e1 ()) (e2 ())

  let sum e = sum (e())

  let find f e = find f (e())

  let merge e () = merge (e ())

  let intersection ?cmp e1 e2 () =
    intersection ?cmp (e1 ()) (e2 ())

  let sorted_merge ?cmp e1 e2 () =
    sorted_merge ?cmp (e1 ()) (e2 ())

  let sorted_merge_n ?cmp l () =
    sorted_merge_n ?cmp (List.map (fun g -> g()) l)

  let tee ?n e = tee ?n (e ())

  let round_robin ?n e = round_robin ?n (e ())

  let interleave e1 e2 () = interleave (e1 ()) (e2 ())

  let intersperse x e () = intersperse x (e ())

  let product e1 e2 () = product (e1 ()) (e2 ())

  let group ?eq e () = group ?eq (e ())

  let group_by f e () = group_by f (e ())

  let uniq ?eq e () = uniq ?eq (e ())

  let sort ?(cmp=Pervasives.compare) enum =
    fun () -> sort ~cmp (enum ())

  let sort_uniq ?(cmp=Pervasives.compare) e =
    let e' = sort ~cmp e in
    uniq ~eq:(fun x y -> cmp x y = 0) e'

  let chunks n e () = chunks n (e())

  let of_list l () = of_list l

  let to_rev_list e = to_rev_list (e ())
  
  let to_list e = to_list (e ())

  let to_array e = to_array (e ())

  let of_array ?start ?len a () = of_array ?start ?len a

  let rand_int i () = rand_int i

  let int_range i j () = int_range i j

  module Infix = struct
    let (--) = int_range

    let (>>=) x f = flat_map f x
  end

  include Infix

  let pp ?start ?stop ?sep ?horizontal pp_elem fmt e =
    pp ?start ?stop ?sep ?horizontal pp_elem fmt (e ())
end

(** {2 Generator functions} *)

let start g = g ()

(** {6 Unrolled mutable list} *)
module MList = struct
  type 'a node =
    | Nil
    | Cons of 'a array * int ref * 'a node ref

  let of_gen gen =
    let start = ref Nil in
    let chunk_size = ref 8 in
    (* fill the list. prev: tail-reference from previous node,
     * cur: current list node *)
    let rec fill prev cur =
      match cur, gen() with
      | _, None -> prev := cur; ()  (* done *)
      | Nil, Some x ->
          let n = !chunk_size in
          if n < 4096 then chunk_size := 2 * !chunk_size;
          fill prev (Cons (Array.make n x, ref 1, ref Nil))
      | Cons (a, n, next), Some x ->
          assert (!n < Array.length a);
          a.(!n) <- x;
          incr n;
          if !n = Array.length a
          then begin
            prev := cur;
            fill next Nil
          end else fill prev cur 
    in
    fill start !start ;
    !start

  let to_gen l () =
    let cur = ref l in
    let i = ref 0 in
    let rec next() = match !cur with
    | Nil -> None
    | Cons (a,n,l') ->
        if !i = !n
        then begin
          cur := !l';
          i := 0;
          next()
        end else begin
          let y = a.(!i) in
          incr i;
          Some y
        end
    in
    next
end

(** Store content of the generator in an enum *)
let persistent gen =
  let l = MList.of_gen gen in
  MList.to_gen l

(*$T
  let g = 1--10 in let g' = persistent g in \
    Restart.to_list g' = Restart.to_list g'
  let g = 1--10 in let g' = persistent g in \
    Restart.to_list g' = [1;2;3;4;5;6;7;8;9;10]
*)
