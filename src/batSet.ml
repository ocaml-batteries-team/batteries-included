(*
 * BatSet - Extended operations on sets
 * Copyright (C) 1996 Xavier Leroy
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

module type OrderedType = BatInterfaces.OrderedType
(** Input signature of the functor {!Set.Make}. *)

module Concrete = struct
  type 'a set =
    | Empty
    | Node of 'a set * 'a * 'a set * int

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false
  (* Sets are represented by balanced binary trees (the heights of the
     children differ by at most 2 *)
  let height = function
    | Empty -> 0
    | Node (_, _, _, h) -> h

  (* Creates a new node with left son l, value v and right son r.
     We must have all elements of l < v < all elements of r.
     l and r must be balanced and | height l - height r | <= 2.
     Inline expansion of height for better speed. *)
  let create l v r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

  (* Same as create, but performs one step of rebalancing if necessary.
     Assumes l and r balanced and | height l - height r | <= 3.
     Inline expansion of create for better speed in the most frequent case
     where no rebalancing is required. *)
  let bal l v r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Set.bal"
      | Node(ll, lv, lr, _) ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else begin
          match lr with
            Empty -> invalid_arg "Set.bal"
          | Node(lrl, lrv, lrr, _)->
            create (create ll lv lrl) lrv (create lrr v r)
        end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Set.bal"
      | Node(rl, rv, rr, _) ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "Set.bal"
          | Node(rll, rlv, rlr, _) ->
            create (create l v rll) rlv (create rlr rv rr)
        end
    end else
      Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

  (* Smallest and greatest element of a set *)
  let rec min_elt = function
      Empty -> raise Not_found
    | Node(Empty, v, r, _) -> v
    | Node(l, v, r, _) -> min_elt l

  let get_root = function
    | Empty -> raise Not_found
    | Node(_l, v, _r, _) -> v

  let pop_min s =
    let mini = ref (get_root s) in
    let rec loop = function
        Empty -> raise Not_found
      | Node(Empty, v, r, _) -> mini := v; r
      | Node(l, v, r, _) -> bal (loop l) v r
    in
    let others = loop s in
    (!mini, others)

  let pop_max s =
    let maxi = ref (get_root s) in
    let rec loop = function
        Empty -> raise Not_found
      | Node(l, v, Empty, _) -> maxi := v; l
      | Node(l, v, r, _) -> bal l v (loop r)
    in
    let others = loop s in
    (!maxi, others)

  let rec max_elt = function
      Empty -> raise Not_found
    | Node(l, v, Empty, _) -> v
    | Node(l, v, r, _) -> max_elt r

  (* Remove the smallest element of the given set *)
  let rec remove_min_elt = function
      Empty -> invalid_arg "Set.remove_min_elt"
    | Node(Empty, v, r, _) -> r
    | Node(l, v, r, _) -> bal (remove_min_elt l) v r

  (* Merge two trees l and r into one.
     All elements of l must precede the elements of r.
     Assume | height l - height r | <= 2. *)
  let merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

  let pop s =
    match s with
    | Empty -> raise Not_found
    | Node (l, v, r, _) ->
      v, merge l r

  (* Insertion of one element *)
  let rec add cmp x = function
      Empty -> Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t ->
      let c = cmp x v in
      if c = 0 then t else
      if c < 0 then bal (add cmp x l) v r else bal l v (add cmp x r)

  let rec remove cmp x = function
      Empty -> Empty
    | Node(l, v, r, _) ->
      let c = cmp x v in
      if c = 0 then merge l r else
      if c < 0 then bal (remove cmp x l) v r else bal l v (remove cmp x r)

  let update cmp x y s =
    if cmp x y <> 0 then
      add cmp y (remove cmp x s)
    else
      let rec loop = function
        | Empty -> raise Not_found
        | Node(l, v, r, h) ->
          let c = cmp x v in
          if c = 0 then
            Node(l, y, r, h)
          else if c < 0 then
            Node(loop l, v, r, h)
          else
            Node(l, v, loop r, h)
      in
      loop s

  let rec mem cmp x = function
      Empty -> false
    | Node(l, v, r, _) ->
      let c = cmp x v in
      c = 0 || mem cmp x (if c < 0 then l else r)

  let rec find cmp x = function
      Empty -> raise Not_found
    | Node(l, v, r, _) ->
      let c = cmp x v in
      if c = 0 then v else  find cmp x (if c < 0 then l else r)

  let rec iter f = function
      Empty -> ()
    | Node(l, v, r, _) -> iter f l; f v; iter f r

  let get_root = function
    | Empty -> raise Not_found
    | Node(l, v, r, _) -> v

  let rec fold f s accu =
    match s with
      Empty -> accu
    | Node(l, v, r, _) -> fold f r (f v (fold f l accu))

  exception Found

  let at_rank_exn i s =
    if i < 0 then invalid_arg "Set.at_rank_exn: negative index not allowed";
    let res = ref (get_root s) in (* raises Not_found if empty *)
    try
      let (_: int) =
        fold (fun node j ->
            if j <> i then j + 1
            else begin
              res := node;
              raise Found
            end
          ) s 0
      in
      invalid_arg "Set.at_rank_exn i s: i >= (Set.cardinal s)"
    with Found -> !res

  let map cmp f s =
    fold (fun v acc -> add cmp (f v) acc) s empty

  let rec op_map f = function
    | Empty -> Empty
    | Node (l,x,r,h) -> Node (op_map f l, f x, op_map f r, h)

  let singleton x = Node(Empty, x, Empty, 1)

  let rec add_min v = function
    | Empty -> singleton v
    | Node (l, x, r, h) ->
      bal (add_min v l) x r

  let rec add_max v = function
    | Empty -> singleton v
    | Node (l, x, r, h) ->
      bal l x (add_max v r)

  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)
  let rec join l v r =
    match (l, r) with
      (Empty, _) -> add_min v r
    | (_, Empty) -> add_max v l
    | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
        create l v r

  (* Splitting.  split x s returns a triple (l, present, r) where
     - l is the set of elements of s that are < x
     - r is the set of elements of s that are > x
     - present is false if s contains no element equal to x,
     or true if s contains an element equal to x. *)
  let rec split cmp x = function
      Empty ->
      (Empty, false, Empty)
    | Node(l, v, r, _) ->
      let c = cmp x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split cmp x l in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split cmp x r in (join l v lr, pres, rr)

  (* split_opt x s returns a triple (l, maybe_v, r) where
     - l is the set of elements of s that are < x
     - r is the set of elements of s that are > x
     - maybe_v is None if s contains no element equal to x,
       or (Some v) if s contains an element v that compares equal to x. *)
  let rec split_opt cmp x = function
    | Empty -> (Empty, None, Empty)
    | Node(l, v, r, _) ->
      let c = cmp x v in
      if c = 0 then (l, Some v, r)
      else if c < 0 then
        let (ll, pres, rl) = split_opt cmp x l in
        (ll, pres, join rl v r)
      else (* c > 0 *)
        let (lr, pres, rr) = split_opt cmp x r in
        (join l v lr, pres, rr)

  (*$inject
    let s12    = of_list [1; 2         ] ;;
    let s45    = of_list [         4; 5] ;;
    let s1245  = of_list [1; 2;    4; 5] ;;
    let s12345 = of_list [1; 2; 3; 4; 5] ;;
  *)
  (*$T split_opt
    let l1, mv1, r1 = split_opt 3 s1245 in \
    (elements l1, mv1, elements r1) = ([1; 2], None  , [4; 5])
    let l2, mv2, r2 = split_opt 3 s12345 in \
    (elements l2, mv2, elements r2) = ([1; 2], Some 3, [4; 5])
  *)

  (* returns a pair of sets: ({y | y < x}, {y | y >= x}) *)
  let split_lt cmp x s =
      let l, maybe, r = split_opt cmp x s in
      match maybe with
        | None -> l, r
        | Some eq_x -> l, add cmp eq_x r

  (*$T split_lt
    let l, r = split_lt 3 s12345 in \
    (elements l, elements r) = ([1; 2], [3; 4; 5])
    let l, r = split_lt 3 s12 in \
    (elements l, elements r) = ([1; 2], [])
    let l, r = split_lt 3 s45 in \
    (elements l, elements r) = ([], [4; 5])
  *)

  (* returns a pair of sets: ({y | y <= x}, {y | y > x}) *)
  let split_le cmp x s =
    let l, maybe, r = split_opt cmp x s in
    match maybe with
      | None -> l, r
      | Some eq_x -> add cmp eq_x l, r

  (*$T split_le
    let l, r = split_le 3 s12345 in \
    (elements l, elements r) = ([1; 2; 3], [4; 5])
    let l, r = split_le 3 s12 in \
    (elements l, elements r) = ([1; 2], [])
    let l, r = split_le 3 s45 in \
    (elements l, elements r) = ([], [4; 5])
  *)

  type 'a iter = E | C of 'a * 'a set * 'a iter

  let rec cardinal = function
      Empty -> 0
    | Node(l, v, r, _) -> cardinal l + 1 + cardinal r

  let rec elements_aux accu = function
      Empty -> accu
    | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

  let elements s = elements_aux [] s
  let to_list = elements

  let to_array s =
    match s with
    | Empty -> [||]
    | Node (_, e, _, _) ->
      let arr = Array.make (cardinal s) e in
      let i = ref 0 in
      iter (fun x -> Array.unsafe_set arr (!i) x; incr i) s;
      arr

  let rec cons_iter s t = match s with
      Empty -> t
    | Node (l, e, r, _) -> cons_iter l (C (e, r, t))

  let rec rev_cons_iter s t = match s with
      Empty -> t
    | Node (l, e, r, _) -> rev_cons_iter r (C (e, l, t))

  let enum_next l () = match !l with
      E -> raise BatEnum.No_more_elements
    | C (e, s, t) -> l := cons_iter s t; e

  let enum_backwards_next l () = match !l with
      E -> raise BatEnum.No_more_elements
    | C (e, s, t) -> l := rev_cons_iter s t; e

  let enum_count l () =
    let rec aux n = function
        E -> n
      | C (e, s, t) -> aux (n + 1 + cardinal s) t
    in aux 0 !l

  let enum t =
    let rec make l =
      let l = ref l in
      let clone() = make !l in
      BatEnum.make ~next:(enum_next l) ~count:(enum_count l) ~clone
    in make (cons_iter t E)

  let backwards t =
    let rec make l =
      let l = ref l in
      let clone() = make !l in
      BatEnum.make ~next:(enum_backwards_next l) ~count:(enum_count l) ~clone
    in make (rev_cons_iter t E)

  let of_enum cmp e =
    BatEnum.fold (fun acc elem -> add cmp elem acc) empty e

  let of_list cmp l = List.fold_left (fun a x -> add cmp x a) empty l

  let of_array cmp l = Array.fold_left (fun a x -> add cmp x a) empty l

  let print ?(first="{") ?(last="}") ?(sep=",") print_elt out t =
    BatEnum.print ~first ~last ~sep (fun out e -> BatPrintf.fprintf out "%a" print_elt e) out (enum t)

  let filter cmp f e = fold (fun x acc -> if f x then add cmp x acc else acc) e empty

  let filter_map cmp f e = fold (fun x acc -> match f x with Some v -> add cmp v acc | _ -> acc) e empty

  let choose = min_elt (* I'd rather this chose the root, but okay *)

  let rec for_all p = function
      Empty -> true
    | Node(l, v, r, _) -> p v && for_all p l && for_all p r

  let rec exists p = function
      Empty -> false
    | Node(l, v, r, _) -> p v || exists p l || exists p r

  let partition cmp p s =
    let rec part (t, f as accu) = function
      | Empty -> accu
      | Node(l, v, r, _) ->
        part (part (if p v then (add cmp v t, f) else (t, add cmp v f)) l) r in
    part (Empty, Empty) s

  let concat t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

  let cartesian_product a b =
    let rec product a b = match a with
      | Empty -> Empty
      | Node (la, xa, ra, _) ->
        let lab = product la b in
        let xab = op_map (fun xb -> (xa,xb)) b in
        let rab = product ra b in
        concat lab (concat xab rab)
    in
    product a b

  let rec union cmp12 s1 s2 =
    match (s1, s2) with
      (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      if h1 >= h2 then
        if h2 = 1 then add cmp12 v2 s1 else begin
          let (l2, _, r2) = split cmp12 v1 s2 in
          join (union cmp12 l1 l2) v1 (union cmp12 r1 r2)
        end
      else
      if h1 = 1 then add cmp12 v1 s2 else begin
        let (l1, _, r1) = split cmp12 v2 s1 in
        join (union cmp12 l1 l2) v2 (union cmp12 r1 r2)
      end

  let rec sym_diff cmp12 s1 s2 =
    match (s1, s2) with
      (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, _), t2) ->
      match split cmp12 v1 t2 with
        (l2, false, r2) ->
        join (sym_diff cmp12 l1 l2) v1 (sym_diff cmp12 r1 r2)
      | (l2, true, r2) ->
        concat (sym_diff cmp12 l1 l2) (sym_diff cmp12 r1 r2)

  let rec inter cmp12 s1 s2 =
    match (s1, s2) with
      (Empty, t2) -> Empty
    | (t1, Empty) -> Empty
    | (Node(l1, v1, r1, _), t2) ->
      match split cmp12 v1 t2 with
        (l2, false, r2) ->
        concat (inter cmp12 l1 l2) (inter cmp12 r1 r2)
      | (l2, true, r2) ->
        join (inter cmp12 l1 l2) v1 (inter cmp12 r1 r2)

  let rec diff cmp12 s1 s2 =
    match (s1, s2) with
      (Empty, t2) -> Empty
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, _), t2) ->
      match split cmp12 v1 t2 with
        (l2, false, r2) ->
        join (diff cmp12 l1 l2) v1 (diff cmp12 r1 r2)
      | (l2, true, r2) ->
        concat (diff cmp12 l1 l2) (diff cmp12 r1 r2)

  let rec disjoint cmp12 s1 s2 =
    match (s1, s2) with
      (Empty, _)
    | (_, Empty) -> true
    | (Node(l1, v1, r1, _), t2) ->
      match split cmp12 v1 t2 with
        (l2, false, r2) ->
        disjoint cmp12 l1 l2 && disjoint cmp12 r1 r2
      | (l2, true, r2) -> false

  let compare cmp s1 s2 =
    let rec compare_aux t1' t2' =
      match (t1', t2') with
        E, E ->  0
      | E, _ -> -1
      | _, E ->  1
      | C (e1, r1, t1), C (e2, r2, t2) ->
        let c = cmp e1 e2 in
        if c = 0 then
          compare_aux (cons_iter r1 t1) (cons_iter r2 t2)
        else
          c in
    compare_aux (cons_iter s1 E) (cons_iter s2 E)

  let equal cmp s1 s2 = compare cmp s1 s2 = 0

  let rec subset cmp s1 s2 =
    match (s1, s2) with
      Empty, _ ->
      true
    | _, Empty ->
      false
    | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
      let c = cmp v1 v2 in
      if c = 0 then
        subset cmp l1 l2 && subset cmp r1 r2
      else if c < 0 then
        subset cmp (Node (l1, v1, Empty, 0)) l2 && subset cmp r1 t2
      else
        subset cmp (Node (Empty, v1, r1, 0)) r2 && subset cmp l1 t2
end

module type S =
sig
  type elt
  type t
  val empty: t
  val is_empty: t -> bool
  val singleton: elt -> t
  val mem: elt -> t -> bool
  val find: elt -> t -> elt
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val update: elt -> elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val sym_diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val disjoint: t -> t -> bool
  val compare_subset: t -> t -> int
  val iter: (elt -> unit) -> t -> unit
  val at_rank_exn: int -> t -> elt
  val map: (elt -> elt) -> t -> t
  val filter: (elt -> bool) -> t -> t
  val filter_map: (elt -> elt option) -> t -> t
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val partition: (elt -> bool) -> t -> t * t
  val split: elt -> t -> t * bool * t
  val split_opt: elt -> t -> t * elt option * t
  val split_lt: elt -> t -> t * t
  val split_le: elt -> t -> t * t
  val cardinal: t -> int
  val elements: t -> elt list
  val to_list: t -> elt list
  val to_array: t -> elt array
  val min_elt: t -> elt
  val pop_min: t -> elt * t
  val pop_max: t -> elt * t
  val max_elt: t -> elt
  val choose: t -> elt
  val pop: t -> elt * t
  val enum: t -> elt BatEnum.t
  val backwards: t -> elt BatEnum.t
  val of_enum: elt BatEnum.t -> t
  val of_list: elt list -> t
  val of_array: elt array -> t
  val print :  ?first:string -> ?last:string -> ?sep:string ->
    ('a BatInnerIO.output -> elt -> unit) ->
    'a BatInnerIO.output -> t -> unit
  module Infix : sig
    val (<--) : t -> elt -> t (** insertion *)
    val (<.) : t -> t -> bool  (** strict subset *)
    val (>.) : t -> t -> bool  (** strict superset *)
    val (<=.) : t -> t -> bool (** subset *)
    val (>=.) : t -> t -> bool (** superset *)
    val (-.) : t -> t -> t     (** difference *)
    val (&&.) : t -> t -> t   (** intersection *)
    val (||.) : t -> t -> t   (** union *)
  end
  (** Operations on {!Set} without exceptions.*)
  module Exceptionless : sig
    val min_elt: t -> elt option
    val max_elt: t -> elt option
    val choose:  t -> elt option
    val find: elt -> t -> elt option
  end
  (** Operations on {!Set} with labels. *)
  module Labels : sig
    val iter : f:(elt -> unit) -> t -> unit
    val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
    val for_all : f:(elt -> bool) -> t -> bool
    val exists : f:(elt -> bool) -> t -> bool
    val map: f:(elt -> elt) -> t -> t
    val filter : f:(elt -> bool) -> t -> t
    val filter_map: f:(elt -> elt option) -> t -> t
    val partition : f:(elt -> bool) -> t -> t * t
  end
end
(** Output signature of the functor {!Set.Make}. *)

module Make (Ord : OrderedType) =
struct
  include Set.Make(Ord)

  (*Breaking the abstraction*)

  type implementation = elt Concrete.set
  external impl_of_t : t -> implementation = "%identity"
  external t_of_impl : implementation -> t = "%identity"

  let cardinal t = Concrete.cardinal (impl_of_t t)
  let enum t = Concrete.enum (impl_of_t t)
  let of_enum e = t_of_impl (Concrete.of_enum Ord.compare e)
  let backwards t = Concrete.backwards (impl_of_t t)

  let remove e t = t_of_impl (Concrete.remove Ord.compare e (impl_of_t t))
  let update e1 e2 t =
    t_of_impl (Concrete.update Ord.compare e1 e2 (impl_of_t t))
  let add e t = t_of_impl (Concrete.add Ord.compare e (impl_of_t t))

  let iter f t = Concrete.iter f (impl_of_t t)
  let at_rank_exn i t = Concrete.at_rank_exn i (impl_of_t t)
  let map f t = t_of_impl (Concrete.map Ord.compare f (impl_of_t t))
  let fold f t acc = Concrete.fold f (impl_of_t t) acc
  let filter f t = t_of_impl (Concrete.filter Ord.compare f (impl_of_t t))
  let filter_map f t = t_of_impl (Concrete.filter_map Ord.compare f (impl_of_t t))

  let find x t = Concrete.find Ord.compare x (impl_of_t t)
  let exists f t = Concrete.exists f (impl_of_t t)
  let for_all f t = Concrete.for_all f (impl_of_t t)
  let paritition f t =
    let l, r = Concrete.partition Ord.compare f (impl_of_t t) in
    (t_of_impl l, t_of_impl r)

  let min_elt t = Concrete.min_elt (impl_of_t t)
  let pop_min t =
    let mini, others = Concrete.pop_min (impl_of_t t) in
    (mini, t_of_impl others)
  let pop_max t =
    let maxi, others = Concrete.pop_max (impl_of_t t) in
    (maxi, t_of_impl others)

  let max_elt t = Concrete.max_elt (impl_of_t t)
  let choose t = Concrete.choose (impl_of_t t)
  let pop t =
    let e, t = Concrete.pop (impl_of_t t) in
    e, t_of_impl t

  let split e s =
    let l, v, r = Concrete.split Ord.compare e (impl_of_t s) in
    (t_of_impl l, v, t_of_impl r)

  let split_opt e s =
    let l, maybe_v, r = Concrete.split_opt Ord.compare e (impl_of_t s) in
    (t_of_impl l, maybe_v, t_of_impl r)

  let split_lt e s =
    let l, r = Concrete.split_lt Ord.compare e (impl_of_t s) in
    (t_of_impl l, t_of_impl r)

  let split_le e s =
    let l, r = Concrete.split_le Ord.compare e (impl_of_t s) in
    (t_of_impl l, t_of_impl r)

  let singleton e = t_of_impl (Concrete.singleton e)
  let elements t = Concrete.elements (impl_of_t t)
  let to_list = elements
  let to_array t = Concrete.to_array (impl_of_t t)

  let union s1 s2 =
    t_of_impl (Concrete.union Ord.compare (impl_of_t s1) (impl_of_t s2))
  let diff s1 s2 =
    t_of_impl (Concrete.diff Ord.compare (impl_of_t s1) (impl_of_t s2))
  let inter s1 s2 =
    t_of_impl (Concrete.inter Ord.compare (impl_of_t s1) (impl_of_t s2))
  let sym_diff s1 s2 =
    t_of_impl (Concrete.sym_diff Ord.compare (impl_of_t s1) (impl_of_t s2))

  let compare t1 t2 = Concrete.compare Ord.compare (impl_of_t t1) (impl_of_t t2)
  let equal t1 t2 = Concrete.equal Ord.compare (impl_of_t t1) (impl_of_t t2)
  let subset t1 t2 = Concrete.subset Ord.compare (impl_of_t t1) (impl_of_t t2)
  let disjoint t1 t2 = Concrete.disjoint Ord.compare (impl_of_t t1) (impl_of_t t2)

  let rec compare_subset s1 s2 =
    match (s1, impl_of_t s2) with
      (Concrete.Empty, Concrete.Empty) -> 0
    | (Concrete.Empty, t2) -> -1
    | (t1, Concrete.Empty) -> 1
    | (Concrete.Node(l1, v1, r1, _), t2) ->
      match split v1 (t_of_impl t2) with
        (l2, true, r2) -> (* v1 in both s1 and s2 *)
        (match compare_subset l1 l2, compare_subset r1 r2 with
         | -1, -1 | -1, 0 | 0, -1 -> -1
         | 0, 0 -> 0
         | 1, 1 | 1, 0 | 0, 1 -> 1
         | _ -> min_int)
      | (l2, false, r2) -> (* v1 in s1, but not in s2 *)
        if (compare_subset l1 l2) >= 0 && (compare_subset r1 r2) >= 0
        then 1 else min_int

  let compare_subset s1 s2 = compare_subset (impl_of_t s1) s2

  let of_list l = t_of_impl (Concrete.of_list Ord.compare l)
  let of_array a = t_of_impl (Concrete.of_array Ord.compare a)

  let print ?first ?last ?sep print_elt out t =
    Concrete.print ?first ?last ?sep print_elt out (impl_of_t t)

  module Infix = struct
    let (<--) s x = add x s
    let (<.) a b = not (equal a b) && subset a b
    let (>.) a b = not (equal a b) && subset b a
    let (<=.) = subset
    let (>=.) a b = subset b a
    let (-.) = diff
    let (&&.) = inter
    let (||.) = union
  end

  module Exceptionless =
  struct
    let min_elt t = try Some (min_elt t) with Not_found -> None
    let max_elt t = try Some (max_elt t) with Not_found -> None
    let choose  t = try Some (choose t)  with Not_found -> None
    let find  e t = try Some (find e t)  with Not_found -> None
  end

  module Labels =
  struct
    let iter ~f t = iter f t
    let fold ~f t ~init = fold f t init
    let for_all ~f t    = for_all f t
    let exists ~f t     = exists f t
    let map    ~f t     = map f t
    let filter ~f t     = filter f t
    let filter_map ~f t = filter_map f t
    let partition ~f t  = partition f t
  end
end

module Int = Make (BatInt)
module Int32 = Make (BatInt32)
module Int64 = Make (BatInt64)
module Nativeint = Make (BatNativeint)
module Float = Make (BatFloat)
module Char = Make (BatChar)
module String = Make (BatString)

module Make2(O1 : OrderedType)(O2 : OrderedType) = struct
  module Set1 = Make(O1)
  module Set2 = Make(O2)
  module Product = Make(
  struct
    type t = O1.t * O2.t
    let compare (x1,y1)(x2,y2) =
      let c = O1.compare x1 x2 in
        if c = 0 then O2.compare y1 y2 else c
  end)

  let cartesian_product set1 set2 =
    let p = Concrete.cartesian_product (Set1.impl_of_t set1) (Set2.impl_of_t set2) in
    Product.t_of_impl p
end

(*$T
  let module S1 = Make(BatInt) in \
  let module S2 = Make(BatString) in \
  let module P = Make2(BatInt)(BatString) in \
  P.cartesian_product \
    (List.fold_right S1.add [1;2;3] S1.empty) \
    (List.fold_right S2.add ["a";"b"] S2.empty) \
    |> P.Product.to_list = [1, "a"; 1, "b"; 2, "a"; 2, "b"; 3, "a"; 3, "b"]
*)

module PSet = struct (*$< PSet *)

  type 'a t = {
    cmp : 'a -> 'a -> int;
    set : 'a Concrete.set;
  }

  type 'a enumerable = 'a t
  type 'a mappable = 'a t

  let empty    = { cmp = compare; set = Concrete.empty }
  let create cmp  = { cmp = cmp; set = Concrete.empty }
  let get_cmp {cmp} = cmp

  (*$T get_cmp
    get_cmp (create BatInt.compare) == BatInt.compare
  *)


  let singleton ?(cmp = compare) x = { cmp = cmp; set = Concrete.singleton x }
  let is_empty s = Concrete.is_empty s.set
  let mem x s = Concrete.mem s.cmp x s.set
  let find x s = Concrete.find s.cmp x s.set
  let add x s  = { s with set = Concrete.add s.cmp x s.set }
  let remove x s = { s with set = Concrete.remove s.cmp x s.set }
  let update x y s = { s with set = Concrete.update s.cmp x y s.set }
  let iter f s = Concrete.iter f s.set
  let at_rank_exn i s = Concrete.at_rank_exn i s.set
  let fold f s acc = Concrete.fold f s.set acc
  let map f s =
    { cmp = Pervasives.compare; set = Concrete.map Pervasives.compare f s.set }
  let filter f s = { s with set = Concrete.filter s.cmp f s.set }
  let filter_map f s =
    { cmp = compare; set = Concrete.filter_map compare f s.set }
  let exists f s = Concrete.exists f s.set
  let cardinal s = fold (fun _ acc -> acc + 1) s 0
  let elements s = Concrete.elements s.set
  let to_list = elements
  let to_array s = Concrete.to_array s.set
  let choose s = Concrete.choose s.set
  let min_elt s = Concrete.min_elt s.set
  let pop_min s =
    let mini, others = Concrete.pop_min s.set in
    (mini, { s with set = others })
  let pop_max s =
    let maxi, others = Concrete.pop_max s.set in
    (maxi, { s with set = others })

  let max_elt s = Concrete.max_elt s.set
  let enum s = Concrete.enum s.set
  let of_enum e = { cmp = compare; set = Concrete.of_enum compare e }
  let of_enum_cmp ~cmp t = { cmp = cmp; set = Concrete.of_enum cmp t }
  let of_list l = { cmp = compare; set = Concrete.of_list compare l }
  let of_array a = { cmp = compare; set = Concrete.of_array compare a }
  let print ?first ?last ?sep print_elt out s =
    Concrete.print ?first ?last ?sep print_elt out s.set
  let for_all f s = Concrete.for_all f s.set
  let partition f s =
    let l, r = Concrete.partition s.cmp f s.set in
    { s with set = l }, { s with set = r }
  let pop s =
    let v, s' = Concrete.pop s.set in
    v, { s with set = s' }
  let split e s =
    let s1, found, s2 = Concrete.split s.cmp e s.set in
    { s with set = s1 }, found, { s with set = s2 }
  let split_opt e s =
    let s1, maybe_v, s2 = Concrete.split_opt s.cmp e s.set in
    { s with set = s1 }, maybe_v, { s with set = s2 }
  let split_lt e s =
    let s1, s2 = Concrete.split_lt s.cmp e s.set in
    { s with set = s1 }, {s with set = s2 }
  let split_le e s =
    let s1, s2 = Concrete.split_le s.cmp e s.set in
    { s with set = s1 }, {s with set = s2 }
  let union s1 s2 =
    { s1 with set = Concrete.union s1.cmp s1.set s2.set }
  let diff s1 s2 =
    { s1 with set = Concrete.diff s1.cmp s1.set s2.set }
  let sym_diff s1 s2 =
    { s1 with set = Concrete.sym_diff s1.cmp s1.set s2.set }
  let intersect s1 s2 =
    { s1 with set = Concrete.inter s1.cmp s1.set s2.set }
  let compare s1 s2 = Concrete.compare s1.cmp s1.set s2.set
  let equal s1 s2 = Concrete.equal s1.cmp s1.set s2.set
  let subset s1 s2 = Concrete.subset s1.cmp s1.set s2.set
  let disjoint s1 s2 = Concrete.disjoint s1.cmp s1.set s2.set

  module Infix = struct
    let (<--) s x = add x s
    let (<.) a b = not (equal a b) && subset a b
    let (>.) a b = not (equal a b) && subset b a
    let (<=.) = subset
    let (>=.) a b = subset b a
    let (-.) = diff
    let (&&.) = intersect
    let (||.) = union
  end
end (*$>*)

type 'a t = 'a Concrete.set

type 'a enumerable = 'a t
type 'a mappable = 'a t

let empty    = Concrete.empty

let singleton x = Concrete.singleton x

let is_empty s = s = Concrete.Empty

let mem x s = Concrete.mem Pervasives.compare x s

let find x s = Concrete.find Pervasives.compare x s

(*$T find
  (find 1 (of_list [1;2;3;4;5;6;7;8])) == 1
  (find 8 (of_list [1;2;3;4;5;6;7;8])) == 8
  (find 1 (singleton 1)) == 1
  let x = "abc" in (find "abc" (singleton x)) == x
  let x = (1,1) in (find (1,1) (singleton x)) == x
  let x,y = (1,1),(1,1) in find x (singleton y) == y
  let x,y = "a","a" in find x (singleton y) != x
  try ignore (find (1,2) (singleton (1,1))); false with Not_found -> true
*)

let add x s  = Concrete.add Pervasives.compare x s

let remove x s = Concrete.remove Pervasives.compare x s

let update x y s = Concrete.update Pervasives.compare x y s

let iter f s = Concrete.iter f s

let at_rank_exn i s = Concrete.at_rank_exn i s

(*$T
  at_rank_exn 0 (of_list [1;2]) == 1
  at_rank_exn 1 (of_list [1;2]) == 2
  try ignore (at_rank_exn 0 empty); false with Not_found -> true
  try ignore (at_rank_exn (-1) (singleton 1)); false \
  with Invalid_argument _msg -> true
  try ignore (at_rank_exn 1 (singleton 1)); false \
  with Invalid_argument _msg -> true
*)

let fold f s acc = Concrete.fold f s acc

let map f s = Concrete.map Pervasives.compare f s

(*$T map
  map (fun _x -> 1) (of_list [1;2;3]) |> cardinal = 1
*)

let filter f s = Concrete.filter Pervasives.compare f s

let filter_map f s = Concrete.filter_map Pervasives.compare f s

let exists f s = Concrete.exists f s

let cardinal s = fold (fun _ acc -> acc + 1) s 0

let elements s = Concrete.elements s
let to_list = elements
let to_array s = Concrete.to_array s

let choose s = Concrete.choose s

let min_elt s = Concrete.min_elt s

(*$Q min_elt
  (Q.list Q.small_int) (fun l -> l = [] || \
    let xs = List.map (fun i -> i mod 2, i) l in \
    let s = ref (of_list xs) in \
    let m = ref (min_elt !s) in \
    while fst !m = 0 do \
      s := remove !m !s; \
      s := add (2,snd !m) !s; \
      m := min_elt !s; \
    done; \
    for_all (fun (x,_) -> x <> 0) !s \
  )
*)

let pop_min s = Concrete.pop_min s

(*$T pop_min
  try ignore (pop_min empty); false with Not_found -> true
  pop_min (of_list [1;2]) = (1, singleton 2)
  pop_min (singleton 2) = (2, empty)
  pop_min (of_list [4;5;6;7]) = (4, of_list [5;6;7])
*)

let pop_max s = Concrete.pop_max s

(*$T pop_max
  try ignore (pop_max empty); false with Not_found -> true
  pop_max (of_list [1;2]) = (2, singleton 1)
  pop_max (singleton 2) = (2, empty)
  let maxi, others = pop_max (of_list [4;5;6;7]) in \
  maxi = 7 && diff others (of_list [4;5;6]) = empty
*)

let max_elt s = Concrete.max_elt s

let enum s = Concrete.enum s

let of_enum e = Concrete.of_enum Pervasives.compare e

let backwards s = Concrete.backwards s

let of_list l = Concrete.of_list Pervasives.compare l

(*$Q of_list
  (Q.list Q.small_int) (fun l -> let xs = List.map (fun i -> i mod 5, i) l in \
    let s1 = of_list xs |> enum |> List.of_enum in \
    let s2 = List.sort_unique Pervasives.compare xs in \
    s1 = s2 \
  )
*)

let of_array a = Concrete.of_array Pervasives.compare a

let print ?first ?last ?sep print_elt out s =
  Concrete.print ?first ?last ?sep print_elt out s

let for_all f s = Concrete.for_all f s
let partition f s = Concrete.partition Pervasives.compare f s
let pop s = Concrete.pop s
let cartesian_product = Concrete.cartesian_product
let split e s = Concrete.split Pervasives.compare e s
let split_opt e s = Concrete.split_opt Pervasives.compare e s
let split_lt e s = Concrete.split_lt Pervasives.compare e s
let split_le e s = Concrete.split_le Pervasives.compare e s
let union s1 s2 = Concrete.union Pervasives.compare s1 s2
let diff s1 s2 = Concrete.diff Pervasives.compare s1 s2
let sym_diff s1 s2 = Concrete.sym_diff Pervasives.compare s1 s2
let intersect s1 s2 = Concrete.inter Pervasives.compare s1 s2
let compare s1 s2 = Concrete.compare Pervasives.compare s1 s2
let equal s1 s2 = Concrete.equal Pervasives.compare s1 s2
let subset s1 s2 = Concrete.subset Pervasives.compare s1 s2
let disjoint s1 s2 = Concrete.disjoint Pervasives.compare s1 s2

(*$T subset
   subset (of_list [1;2;3]) (of_list [1;2;3;4])
   not (subset (of_list [1;2;3;5]) (of_list [1;2;3;4]))
   not (subset (of_list [1;2;3;4]) (of_list [1;2;3]))
*)

(*$T compare
  compare (of_list [1;2;3]) (of_list [1;2;3;4]) <> 0
  let a = of_list [1;2;3] and b = of_list [1;2;3;4] in compare a b = - (compare b a)
  let a = of_list [1;2;3] and b = of_list [1;2;3;4] and c = of_list [3;1;2] in\
    compare a b = - (compare b c)
  compare (of_list [1;2;3]) (of_list [3;1;2]) = 0
*)

(*$T cartesian_product
  cartesian_product (of_list [1;2;3]) (of_list ["a"; "b"]) |> to_list = \
    [1, "a"; 1, "b"; 2, "a"; 2, "b"; 3, "a"; 3, "b"]
  is_empty @@ cartesian_product (of_list [1;2;3]) empty
  is_empty @@ cartesian_product empty (of_list [1;2;3])
  let s1, s2 = of_list ["a"; "b"; "c"], of_list [1;2;3] in \
    equal (cartesian_product s1 s2) \
          (map BatTuple.Tuple2.swap (cartesian_product s2 s1))
*)

(*$inject
  module TestSet =
    Set.Make
      (struct
        type t = int * int
        let compare (x, _) (y, _) = BatInt.compare x y
      end) ;;
  let ts = TestSet.of_list [(1,0);(2,0);(3,0)] ;;
*)
(*$T
  try ignore(TestSet.update (1, 0) (1, 1) TestSet.empty); false \
  with Not_found -> true
  TestSet.update (1,0) (1,1)  ts = TestSet.of_list [(1,1);(2,0);(3,0)]
  TestSet.update (2,0) (2,1)  ts = TestSet.of_list [(1,0);(2,1);(3,0)]
  TestSet.update (3,0) (3,1)  ts = TestSet.of_list [(1,0);(2,0);(3,1)]
  TestSet.update (3,0) (-1,0) ts = TestSet.of_list [(1,0);(2,0);(-1,0)]
*)

module Infix = struct
  let (<--) s x = add x s
  let (<.) a b = not (equal a b) && subset a b
  let (>.) a b = not (equal a b) && subset b a
  let (<=.) = subset
  let (>=.) a b = subset b a
  let (-.) = diff
  let (&&.) = intersect
  let (||.) = union
end

module Incubator = struct (*$< Incubator *)
  let op_map f s = Concrete.op_map f s
    (*$T op_map
      of_enum (1--3) |> op_map ((+) 2) |> mem 5
      of_enum (1--3) |> op_map ((+) 2) |> mem 4
      of_enum (1--3) |> op_map ((+) 2) |> mem 3
    *)


end (*$>*)
