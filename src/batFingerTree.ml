(*
 * Copyright (C) 2011  Batteries Included Development Team
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

module type Monoid =
sig
  type t
  val zero : t
  val combine : t -> t -> t
end

module type S =
sig
  type measure
  type 'a t
  exception Empty
  type ('wrapped_type, 'a) wrap_measure
  val empty : 'a t
  val singleton : 'a -> 'a t
  val cons : ('a t -> 'a -> 'a t, 'a) wrap_measure
  val snoc : ('a t -> 'a -> 'a t, 'a) wrap_measure
  val front : ('a t -> ('a * 'a t) option, 'a) wrap_measure
  val front_exn : ('a t -> ('a * 'a t), 'a) wrap_measure
  val head : 'a t -> 'a option
  val head_exn : 'a t -> 'a
  val last : 'a t -> 'a option
  val last_exn : 'a t -> 'a
  val tail : ('a t -> 'a t option, 'a) wrap_measure
  val tail_exn : ('a t -> 'a t, 'a) wrap_measure
  val init : ('a t -> 'a t option, 'a) wrap_measure
  val init_exn : ('a t -> 'a t, 'a) wrap_measure
  val rear : ('a t -> ('a * 'a t) option, 'a) wrap_measure
  val rear_exn : ('a t -> ('a * 'a t), 'a) wrap_measure
  val size : 'a t -> int
  val lookup : ((measure -> bool) -> 'a t -> 'a, 'a) wrap_measure
  val measure : ('a t -> measure, 'a) wrap_measure
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val fold_right : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val iter : ('a -> unit) -> 'a t -> unit
  val iter_right : ('a -> unit) -> 'a t -> unit
  val enum : 'a t -> 'a BatEnum.t
  val backwards : 'a t -> 'a BatEnum.t
  val to_list : 'a t -> 'a list
  val to_list_backwards : 'a t -> 'a list
  val of_enum : ('a BatEnum.t -> 'a t, 'a) wrap_measure
  val of_backwards : ('a BatEnum.t -> 'a t, 'a) wrap_measure
  val of_list : ('a list -> 'a t, 'a) wrap_measure
  val of_list_backwards : ('a list -> 'a t, 'a) wrap_measure
  val map : (('a -> 'b) -> 'a t -> 'b t, 'b) wrap_measure
  val map_right : (('a -> 'b) -> 'a t -> 'b t, 'b) wrap_measure
  val append : ('a t -> 'a t -> 'a t, 'a) wrap_measure
  val reverse : ('a t -> 'a t, 'a) wrap_measure
  val split : ((measure -> bool) -> 'a t -> 'a t * 'a t, 'a) wrap_measure
end

type 'a format_printer = Format.formatter -> 'a -> unit

module Make (M : Monoid) :
  sig
    include S with
      type measure = M.t
      and type ('wrapped_type, 'a) wrap_measure = ('a -> M.t) -> 'wrapped_type

    (* this function is a not in S because it is not meant to be exported
     * since it prints the actual structure of the tree *)
    val pp_debug :
      ?pp_measure:(M.t format_printer) -> 'a format_printer ->
        'a t format_printer
  end
= struct

  exception Empty (* the name collides with a constructor below,
                   * so making an alias *)
  exception EmptyAlias = Empty

  (* All the datatypes in here are the same as the same described in the
   * paper in the mli.
   * Since there are several variants mentioned:
   * - we define 'a digit not as being an 'a list as done initially in the
   *   paper but (as suggested later) as a sum types that cover sequence of
   *   length 1, 2, 3 or 4
   *   I didn't test with lists, but I suspect it would be slower and take
   *   more memory. On the minus side, the code is rather annoying to write
   *   with the current digits.
   * - there are measure caches not only on nodes, but also on digits.
   *   It is slightly faster when benchmarking construction/deconstruction
   *   even with dummy annotations.

   * In many places, it looks like functions are defined twice in slighly
   * different versions. This is for performance reasons, to avoid higher
   * order calls (made everything 30% slower on my tests).
   *)

  type measure = M.t
  type 'a node =
    | Node2 of M.t * 'a * 'a
    | Node3 of M.t * 'a * 'a * 'a
  type 'a digit =
    | One of M.t * 'a
    | Two of M.t * 'a * 'a
    | Three of M.t * 'a * 'a * 'a
    | Four of M.t * 'a * 'a * 'a * 'a
  type 'a t =
    | Empty
    | Single of 'a
    | Deep of M.t * 'a digit * 'a node t * 'a digit

  let poly_recursion = Obj.magic

  let empty = Empty
  let singleton a = Single a

  let is_empty = function
    | Empty -> true
    | Single _ | Deep _ -> false

  (*---------------------------------*)
  (*              fold               *)
  (*---------------------------------*)
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

  let rec fold_right f acc = function
    | Empty -> acc
    | Single x -> f acc x
    | Deep (_, pr, m, sf) ->
      let acc = fold_right_digit f acc sf in
      let acc = (poly_recursion fold_right : _ -> _ -> _ -> _) (fun acc elt -> fold_right_node f acc elt) acc m in
      let acc = fold_right_digit f acc pr in
      acc
  let rec fold_left f acc = function
    | Empty -> acc
    | Single x -> f acc x
    | Deep (_, pr, m, sf) ->
      let acc = fold_left_digit f acc pr in
      let acc = (poly_recursion fold_left : _ -> _ -> _ -> _) (fun acc elt -> fold_left_node f acc elt) acc m in
      let acc = fold_left_digit f acc sf in
      acc

  (*---------------------------------*)
  (*          debug printing         *)
  (*---------------------------------*)
  let pp_debug_digit pp_measure pp_a f = function
    | One (m, a) ->
      Format.fprintf f "@[@[<2>One (@,%a,@ %a@])@]" pp_measure m pp_a a
    | Two (m, a, b) ->
      Format.fprintf f "@[@[<2>Two (@,%a,@ %a,@ %a@])@]" pp_measure m pp_a a pp_a b
    | Three (m, a, b, c) ->
      Format.fprintf f "@[@[<2>Three (@,%a,@ %a,@ %a,@ %a@])@]" pp_measure m pp_a a pp_a b pp_a c
    | Four (m, a, b, c, d) ->
      Format.fprintf f "@[@[<2>Four (@,%a,@ %a,@ %a,@ %a,@ %a@])@]" pp_measure m pp_a a pp_a b pp_a c pp_a d

  let pp_debug_node pp_measure pp_a f = function
    | Node2 (m, a, b) ->
      Format.fprintf f "@[@[<2>Node2 (@,%a,@ %a,@ %a@])@]" pp_measure m pp_a a pp_a b
    | Node3 (m, a, b, c) ->
      Format.fprintf f "@[@[<2>Node3 (@,%a,@ %a,@ %a,@ %a@])@]" pp_measure m pp_a a pp_a b pp_a c

  (*let pp_debug_tree : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit = fun _ _ _ -> assert false*)
  let rec pp_debug_tree pp_measure pp_a f = function
    | Empty -> Format.fprintf f "Empty"
    | Single a -> Format.fprintf f "@[<2>Single@ %a@]" pp_a a
    | Deep (v, pr, m, sf) ->
      Format.fprintf f "@[@[<v2>Deep (@,%a,@ %a,@ %a,@ %a@]@\n)@]"
        pp_measure v
        (pp_debug_digit pp_measure pp_a) pr
        ((poly_recursion pp_debug_tree : _ -> _ -> _ -> _)
            pp_measure (pp_debug_node pp_measure pp_a)) m
        (pp_debug_digit pp_measure pp_a) sf

  let dummy_printer f _ =
    Format.pp_print_string f "_"

  let pp_debug ?(pp_measure = dummy_printer) pp_a f t =
    pp_debug_tree pp_measure pp_a f t

  let pp_list pp_a f = function
    | [] -> Format.fprintf f "[]"
    | h :: t ->
      Format.fprintf f "[%a" pp_a h;
      List.iter (fun a -> Format.fprintf f "; %a" pp_a a) t;
      Format.fprintf f "]"

  (*---------------------------------*)
  (*     measurement functions       *)
  (*---------------------------------*)
  type 'a measurer = 'a -> M.t
  type ('wrapped_type, 'a) wrap_measure = 'a measurer -> 'wrapped_type
  let measure_node : 'a node measurer = function
    | Node2 (v, _, _)
    | Node3 (v, _, _, _) -> v

  let measure_digit = function
    | One (v, _)
    | Two (v, _, _)
    | Three (v, _, _, _)
    | Four (v, _, _, _, _) -> v

  let measure_t_node = function
    | Empty -> M.zero
    | Single x -> measure_node x
    | Deep (v, _, _, _) -> v
  let measure_t measure = function
    | Empty -> M.zero
    | Single x -> measure x
    | Deep (v, _, _, _) -> v

  (*---------------------------------*)
  (*  a bunch of smart constructors  *)
  (*---------------------------------*)
  let node2 measure a b =
    Node2 (M.combine (measure a) (measure b), a, b)
  let node2_node a b =
    Node2 (M.combine (measure_node a) (measure_node b), a, b)

  let node3 measure a b c =
    Node3 (M.combine (measure a) (M.combine (measure b) (measure c)), a, b, c)
  let node3_node a b c =
    Node3 (M.combine (measure_node a) (M.combine (measure_node b) (measure_node c)), a, b, c)

  let deep pr m sf =
    let v = measure_digit pr in
    let v = M.combine v (measure_t_node m) in
    let v = M.combine v (measure_digit sf) in
    Deep (v, pr, m, sf)

  let one_node a =
    One (measure_node a, a)
  let one measure a =
    One (measure a, a)

  let two_node a b =
    Two (M.combine (measure_node a) (measure_node b), a, b)
  let two measure a b =
    Two (M.combine (measure a) (measure b), a, b)

  let three_node a b c =
    Three (M.combine (M.combine (measure_node a) (measure_node b)) (measure_node c), a, b, c)
  let three measure a b c =
    Three (M.combine (M.combine (measure a) (measure b)) (measure c), a, b, c)

  let four_node a b c d =
    Four (M.combine (M.combine (measure_node a) (measure_node b)) (M.combine (measure_node c) (measure_node d)), a, b, c, d)
  let four measure a b c d =
    Four (M.combine (M.combine (measure a) (measure b)) (M.combine (measure c) (measure d)), a, b, c, d)

  (*---------------------------------*)
  (*          cons / snoc            *)
  (*---------------------------------*)
  let cons_digit_node d x =
    match d with
    | One (v, a) -> Two (M.combine (measure_node x) v, x, a)
    | Two (v, a, b) -> Three (M.combine (measure_node x) v, x, a, b)
    | Three (v, a, b, c) -> Four (M.combine (measure_node x) v, x, a, b, c)
    | Four _ -> assert false
  let cons_digit measure d x =
    match d with
    | One (v, a) -> Two (M.combine (measure x) v, x, a)
    | Two (v, a, b) -> Three (M.combine (measure x) v, x, a, b)
    | Three (v, a, b, c) -> Four (M.combine (measure x) v, x, a, b, c)
    | Four _ -> assert false

  let snoc_digit_node d x =
    match d with
    | One (v, a) -> Two (M.combine v (measure_node x), a, x)
    | Two (v, a, b) -> Three (M.combine v (measure_node x), a, b, x)
    | Three (v, a, b, c) -> Four (M.combine v (measure_node x), a, b, c, x)
    | Four _ -> assert false
  let snoc_digit measure d x =
    match d with
    | One (v, a) -> Two (M.combine v (measure x), a, x)
    | Two (v, a, b) -> Three (M.combine v (measure x), a, b, x)
    | Three (v, a, b, c) -> Four (M.combine v (measure x), a, b, c, x)
    | Four _ -> assert false

  (*let cons_aux : 'a node t -> 'a node -> 'a node t = fun _ _ -> assert false*)
  let rec cons_aux t a =
    match t with
    | Empty -> Single a
    | Single b -> deep (one_node a) Empty (one_node b)
    | Deep (_, Four (_, b, c, d, e), m, sf) -> deep (two_node a b) ((poly_recursion cons_aux : _ -> _ -> _) m (node3_node c d e)) sf
    | Deep (v, pr, m, sf) -> Deep (M.combine (measure_node a) v, cons_digit_node pr a, m, sf)
  let cons measure t a =
    match t with
    | Empty -> Single a
    | Single b -> deep (one measure a) Empty (one measure b)
    | Deep (_, Four (_, b, c, d, e), m, sf) -> deep (two measure a b) (cons_aux m (node3 measure c d e)) sf
    | Deep (v, pr, m, sf) -> Deep (M.combine (measure a) v, cons_digit measure pr a, m, sf)

  (*let snoc : 'a measurer -> 'a t -> 'a -> 'a t = fun _ _ _ -> assert false*)
  let rec snoc_aux t a =
    match t with
    | Empty -> Single a
    | Single b -> deep (one_node b) Empty (one_node a)
    | Deep (_, pr, m, Four (_, b, c, d, e)) -> deep pr ((poly_recursion snoc_aux : _ -> _ -> _) m (node3_node b c d)) (two_node e a)
    | Deep (v, pr, m, sf) -> Deep (M.combine v (measure_node a), pr, m, snoc_digit_node sf a)
  let snoc measure t a =
    match t with
    | Empty -> Single a
    | Single b -> deep (one measure b) Empty (one measure a)
    | Deep (_, pr, m, Four (_, b, c, d, e)) -> deep pr (snoc_aux m (node3 measure b c d)) (two measure e a)
    | Deep (v, pr, m, sf) -> Deep (M.combine v (measure a), pr, m, snoc_digit measure sf a)

  (*---------------------------------*)
  (*     various conversions         *)
  (*---------------------------------*)
  let to_tree_digit measure d =
    match d with
    | One (_, a) -> Single a
    | Two (v, a, b) -> Deep (v, one measure a, Empty, one measure b)
    | Three (v, a, b, c) -> Deep (v, two measure a b, Empty, one measure c)
    | Four (v, a, b, c, d) -> Deep (v, three measure a b c, Empty, one measure d)
  let to_tree_list measure = function
    | [] -> Empty
    | [a] -> Single a
    | [a; b] -> deep (one measure a) Empty (one measure b)
    | [a; b; c] -> deep (two measure a b) Empty (one measure c)
    | [a; b; c; d] -> deep (three measure a b c) Empty (one measure d)
    | _ -> assert false
  let to_tree_digit_node d =
    to_tree_digit measure_node d

  let to_digit_node = function
    | Node2 (v, a, b) -> Two (v, a, b)
    | Node3 (v, a, b, c) -> Three (v, a, b, c)
  let to_digit_list measure = function
    | [a] -> one measure a
    | [a; b] -> two measure a b
    | [a; b; c] -> three measure a b c
    | [a; b; c; d] -> four measure a b c d
    | _ -> assert false
  let to_digit_list_node = function
    | [a] -> one_node a
    | [a; b] -> two_node a b
    | [a; b; c] -> three_node a b c
    | [a; b; c; d] -> four_node a b c d
    | _ -> assert false

  (*---------------------------------*)
  (*     front / rear / etc.         *)
  (*---------------------------------*)
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
  let tail_digit_node = function
    | One _ -> assert false
    | Two (_, _, a) -> one_node a
    | Three (_, _, a, b) -> two_node a b
    | Four (_, _, a, b, c) -> three_node a b c
  let tail_digit measure = function
    | One _ -> assert false
    | Two (_, _, a) -> one measure a
    | Three (_, _, a, b) -> two measure a b
    | Four (_, _, a, b, c) -> three measure a b c
  let init_digit_node = function
    | One _ -> assert false
    | Two (_, a, _) -> one_node a
    | Three (_, a, b, _) -> two_node a b
    | Four (_, a, b, c, _) -> three_node a b c
  let init_digit measure = function
    | One _ -> assert false
    | Two (_, a, _) -> one measure a
    | Three (_, a, b, _) -> two measure a b
    | Four (_, a, b, c, _) -> three measure a b c

  type ('a, 'rest) view =
    | Vnil
    | Vcons of 'a * 'rest

  (*let view_left_aux : 'a t -> ('a, 'a t) view = fun _ _ -> assert false*)
  let rec view_left_aux = function
    | Empty -> Vnil
    | Single x -> Vcons (x, Empty)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match (poly_recursion view_left_aux : _ -> _) m with
        | Vnil -> to_tree_digit_node sf
        | Vcons (a, m') -> deep (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep (tail_digit_node pr) m sf in
      Vcons (head_digit pr, vcons)
  let view_left measure = function
    | Empty -> Vnil
    | Single x -> Vcons (x, Empty)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match view_left_aux m with
        | Vnil -> to_tree_digit measure sf
        | Vcons (a, m') -> deep (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep (tail_digit measure pr) m sf in
      Vcons (head_digit pr, vcons)

  let rec view_right_aux = function
    | Empty -> Vnil
    | Single x -> Vcons (x, Empty)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match (poly_recursion view_right_aux : _ -> _) m with
        | Vnil -> to_tree_digit_node pr
        | Vcons (a, m') -> deep pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep pr m (init_digit_node sf) in
      Vcons (last_digit sf, vcons)
  let view_right measure = function
    | Empty -> Vnil
    | Single x -> Vcons (x, Empty)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match view_right_aux m with
        | Vnil -> to_tree_digit measure pr
        | Vcons (a, m') -> deep pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep pr m (init_digit measure sf) in
      Vcons (last_digit sf, vcons)

  let head_exn = function
    | Empty -> raise EmptyAlias
    | Single a -> a
    | Deep (_, pr, _, _) -> head_digit pr
  let head = function
    | Empty -> None
    | Single a -> Some a
    | Deep (_, pr, _, _) -> Some (head_digit pr)

  let last_exn = function
    | Empty -> raise EmptyAlias
    | Single a -> a
    | Deep (_, _, _, sf) -> last_digit sf
  let last = function
    | Empty -> None
    | Single a -> Some a
    | Deep (_, _, _, sf) -> Some (last_digit sf)

  let tail measure t =
    match view_left measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let tail_exn measure t =
    match view_left measure t with
    | Vnil -> raise EmptyAlias
    | Vcons (_, tl) -> tl

  let front measure t =
    match view_left measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (hd, tl)
  let front_exn measure t =
    match view_left measure t with
    | Vnil -> raise EmptyAlias
    | Vcons (hd, tl) -> (hd, tl)

  let init measure t =
    match view_right measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let init_exn measure t =
    match view_right measure t with
    | Vnil -> raise EmptyAlias
    | Vcons (_, tl) -> tl

  let rear measure t =
    match view_right measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (hd, tl)
  let rear_exn measure t =
    match view_right measure t with
    | Vnil -> raise EmptyAlias
    | Vcons (hd, tl) -> (hd, tl)

  (*---------------------------------*)
  (*            append               *)
  (*---------------------------------*)
  let nodes =
    let add_digit_to digit l =
      match digit with
      | One (_, a) -> a :: l
      | Two (_, a, b) -> a :: b :: l
      | Three (_, a, b, c) -> a :: b :: c :: l
      | Four (_, a, b, c, d) -> a :: b :: c :: d :: l in

    let rec nodes_aux measure ts sf2 = (* no idea if this should be tail rec *)
      match ts, sf2 with
      | [], One _ -> assert false
      | [], Two (_, a, b)
      | [a], One (_, b) -> [node2 measure a b]
      | [], Three (_, a, b, c)
      | [a], Two (_, b, c)
      | [a; b], One (_, c) -> [node3 measure a b c]
      | [], Four (_, a, b, c, d)
      | [a], Three (_, b, c, d)
      | [a; b], Two (_, c, d)
      | [a; b; c], One (_, d) -> [node2 measure a b; node2 measure c d]
      | a :: b :: c :: ts, _ -> node3 measure a b c :: nodes_aux measure ts sf2
      | [a], Four (_, b, c, d, e)
      | [a; b], Three (_, c, d, e) -> [node3 measure a b c; node2 measure d e]
      | [a; b], Four (_, c, d, e, f) -> [node3 measure a b c; node3 measure d e f] in

    fun measure sf1 ts sf2 ->
      let ts = add_digit_to sf1 ts in
      nodes_aux measure ts sf2

  (*let app3 : 'a measurer -> 'a t -> 'a list -> 'a t -> 'a t = fun _ _ _ -> assert false*)
  let rec app3 measure t1 elts t2 =
    match t1, t2 with
    | Empty, _ -> List.fold_right (fun elt acc -> cons measure acc elt) elts t2
    | _, Empty -> List.fold_left (fun acc elt -> snoc measure acc elt) t1 elts
    | Single x1, _ -> cons measure (List.fold_right (fun elt acc -> cons measure acc elt) elts t2) x1
    | _, Single x2 -> snoc measure (List.fold_left (fun acc elt -> snoc measure acc elt) t1 elts) x2
    | Deep (v1, pr1, m1, sf1), Deep (v2, pr2, m2, sf2) ->
      Deep (M.combine v1 v2, pr1, ((poly_recursion app3 : _ ->  _ -> _ -> _ -> _) measure_node m1 (nodes measure sf1 elts pr2) m2), sf2)

  let append measure t1 t2 = app3 measure t1 [] t2

  (*---------------------------------*)
  (*            reverse              *)
  (*---------------------------------*)
  (* unfortunately, when reversing, we need to rebuild every annotation
   * because the monoid does not have to be commutative *)

  let reverse_digit_node rev_a = function
    | One (_, a) -> one_node (rev_a a)
    | Two (_, a, b) -> two_node (rev_a b) (rev_a a)
    | Three (_, a, b, c) -> three_node (rev_a c) (rev_a b) (rev_a a)
    | Four (_, a, b, c, d) -> four_node (rev_a d) (rev_a c) (rev_a b) (rev_a a)
  let reverse_digit measure = function
    | One _ as d -> d
    | Two (_, a, b) -> two measure b a
    | Three (_, a, b, c) -> three measure c b a
    | Four (_, a, b, c, d) -> four measure d c b a
  let reverse_node_node rev_a = function
    | Node2 (_, a, b) -> node2_node (rev_a b) (rev_a a)
    | Node3 (_, a, b, c) -> node3_node (rev_a c) (rev_a b) (rev_a a)
  let reverse_node measure = function
    | Node2 (_, a, b) -> node2 measure b a
    | Node3 (_, a, b, c) -> node3 measure c b a

  (*let reverse_aux : ('a node -> 'a node) -> 'a node t -> 'a node t = fun _ _ -> assert false*)
  let rec reverse_aux reverse_a = function
    | Empty -> Empty
    | Single a -> Single (reverse_a a)
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit_node reverse_a pr in
      let rev_sf = reverse_digit_node reverse_a sf in
      let rev_m =
        (poly_recursion reverse_aux : _ -> _ -> _)
          (reverse_node_node (reverse_a)) m in
      deep rev_sf rev_m rev_pr
  let reverse measure = function
    | Empty
    | Single _ as t -> t
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit measure pr in
      let rev_sf = reverse_digit measure sf in
      let rev_m = reverse_aux (reverse_node measure) m in
      deep rev_sf rev_m rev_pr

  (*---------------------------------*)
  (*             split               *)
  (*---------------------------------*)
  type ('a, 'rest) split = Split of 'rest * 'a * 'rest
  let split_digit measure p i = function
    | One (_, a) -> Split ([], a, [])
    | Two (_, a, b) ->
      let i' = M.combine i (measure a) in
      if p i' then Split ([], a, [b]) else
        Split ([a], b, [])
    | Three (_, a, b, c) ->
      let i' = M.combine i (measure a) in
      if p i' then Split ([], a, [b; c]) else
        let i'' = M.combine i' (measure b) in
        if p i'' then Split ([a], b, [c]) else
          Split ([a; b], c, [])
    | Four (_, a, b, c, d) ->
      let i' = M.combine i (measure a) in
      if p i' then Split ([], a, [b; c; d]) else
        let i'' = M.combine i' (measure b) in
        if p i'' then Split ([a], b, [c; d]) else
          let i''' = M.combine i'' (measure c) in
          if p i''' then Split ([a; b], c, [d]) else
            Split ([a; b; c], d, [])

  let deep_left measure pr m sf =
    match pr with
    | [] -> (
      match view_left measure_node m with
      | Vnil -> to_tree_digit measure sf
      | Vcons (a, m') -> deep (to_digit_node a) m' sf
    )
    | _ ->
      deep (to_digit_list measure pr) m sf
  let deep_right measure pr m sf =
    match sf with
    | [] -> (
      match view_right measure_node m with
      | Vnil -> to_tree_digit measure pr
      | Vcons (a, m') -> deep pr m' (to_digit_node a)
    )
    | _ ->
      deep pr m (to_digit_list measure sf)

  (*let split_tree : 'a measurer -> (M.t -> bool) -> M.t -> 'a t -> ('a, 'a t) split = fun _ _ _ _ -> assert false*)
  let rec split_tree measure p i = function
    | Empty -> raise EmptyAlias
    | Single x -> Split (Empty, x, Empty)
    | Deep (_, pr, m, sf) ->
      let vpr = M.combine i (measure_digit pr) in
      if p vpr then
        let Split (l, x, r) = split_digit measure p i pr in
        Split (to_tree_list measure l, x, deep_left measure r m sf)
      else
        let vm = M.combine vpr (measure_t_node m) in
        if p vm then
          let Split (ml, xs, mr) = (poly_recursion split_tree : _ -> _ -> _ -> _ -> _) measure_node p vpr m in
          let Split (l, x, r) = split_digit measure p (M.combine vpr (measure_t_node ml)) (to_digit_node xs) in
          Split (deep_right measure pr ml l, x, deep_left measure r mr sf)
        else
          let Split (l, x, r) = split_digit measure p vm sf in
          Split (deep_right measure pr m l, x, to_tree_list measure r)

  let split measure f t =
    match t with
    | Empty -> (Empty, Empty)
    | _ ->
      if f (measure_t measure t) then
        let Split (l, x, r) = split_tree measure f M.zero t in
        (l, cons measure r x)
      else
        (t, Empty)

  (*---------------------------------*)
  (*            lookup               *)
  (*---------------------------------*)
  (* This is a simplification of splitTree that avoids rebuilding the tree
   * two trees aroud the elements being looked up
   * But you can't just find the element, so instead these functions find the
   * element _and_ the measure of the elements of the current node that are on
   * the left of the element.
   *
   * (this is needed because in splitTree, at some point, you measure the left
   * tree returned by a recursive call, but here we don't have the left tree!)
   *)
  let lookup_digit measure p i = function
    | One (_, a) -> M.zero, a
    | Two (_, a, b) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else m_a, b
    | Three (_, a, b, c) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else
        let m_b = measure b in
        let i'' = M.combine i' m_b in
        if p i'' then m_a, b else M.combine m_a m_b, c
    | Four (_, a, b, c, d) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else
        let m_b = measure b in
        let i'' = M.combine i' m_b in
        if p i'' then m_a, b else
          let m_c = measure c in
          let i''' = M.combine i'' m_c in
          if p i''' then M.combine m_a m_b, c else M.combine (M.combine m_a m_b) m_c, d

  let lookup_node measure p i = function
    | Node2 (_, a, b) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else m_a, b
    | Node3 (_, a, b, c) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else
        let m_b = measure b in
        let i'' = M.combine i' m_b in
        if p i'' then m_a, b else M.combine m_a m_b, c

  (*let lookup_tree : 'a measurer -> (M.t -> bool) -> M.t -> 'a t -> 'a = fun _ _ _ _ -> assert false*)
  let rec lookup_tree measure p i = function
    | Empty -> raise EmptyAlias
    | Single x -> M.zero, x
    | Deep (_, pr, m, sf) ->
      let m_pr = measure_digit pr in
      let vpr = M.combine i m_pr in
      if p vpr then lookup_digit measure p i pr else
        let m_m = measure_t_node m in
        let vm = M.combine vpr m_m in
        if p vm then
          let v_left, node =
            (poly_recursion lookup_tree : _ -> _ -> _ -> _ -> _)
              measure_node p vpr m in
          let v, x = lookup_node measure p (M.combine vpr v_left) node in
          M.combine (M.combine m_pr v_left) v, x
        else
          let v, x = lookup_digit measure p vm sf in
          M.combine (M.combine m_pr m_m) v, x

  let lookup measure p t =
    snd (lookup_tree measure p M.zero t)

  (*---------------------------------*)
  (*          enumerations           *)
  (*---------------------------------*)
  (* Here enumerations are implemented by iterating over the structure in cps
   * Each time an element is found, a pair consisting of this element and the
   * current continuation is returned.
   * The flag rectypes is needed here because the continuations have type:
   * (unit -> ('a, 'iter) as 'iter)
   *)
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
  let rec enum_aux enum_a t k =
    match t with
    | Empty -> k ()
    | Single a -> enum_a a k
    | Deep (_, pr, m, sf) ->
      enum_digit enum_a pr (fun () ->
        (poly_recursion enum_aux : _ -> _ -> _ -> _) (enum_node enum_a) m (fun () ->
          enum_digit enum_a sf k
        )
      )
  let enum_cps t = enum_aux enum_base t (fun () -> raise BatEnum.No_more_elements)

  let rec enum_aux_backwards enum_a t k =
    match t with
    | Empty -> k ()
    | Single a -> enum_a a k
    | Deep (_, pr, m, sf) ->
      enum_digit_backwards enum_a sf (fun () ->
        (poly_recursion enum_aux_backwards : _ -> _ -> _ -> _) (enum_node_backwards enum_a) m (fun () ->
          enum_digit_backwards enum_a pr k
        )
      )
  let enum_cps_backwards t = enum_aux_backwards enum_base t (fun () -> raise BatEnum.No_more_elements)

  (*---------------------------------*)
  (*           conversion            *)
  (*---------------------------------*)
  let enum t =
    BatEnum.from_loop
      (fun () -> enum_cps t)
      (fun k -> k ())
  let backwards t =
    BatEnum.from_loop
      (fun () -> enum_cps_backwards t)
      (fun k -> k ())

  let of_enum measure enum =
    BatEnum.fold (fun t elt -> snoc measure t elt) empty enum
  let of_backwards measure enum =
    BatEnum.fold (fun t elt -> cons measure t elt) empty enum

  let to_list t =
    BatList.of_backwards (backwards t)
  let to_list_backwards t =
    BatList.of_backwards (enum t)
  let of_list measure l =
    List.fold_left (fun t elt -> snoc measure t elt) empty l
  let of_list_backwards measure l =
    List.fold_left (fun t elt -> cons measure t elt) empty l

  (*---------------------------------*)
  (*        classic traversals       *)
  (*---------------------------------*)
  let iter f t =
    fold_left (fun () elt -> f elt) () t
  let iter_right f t =
    fold_right (fun () elt -> f elt) () t
  let map measure f t =
    fold_left (fun acc elt -> snoc measure acc (f elt)) empty t
  let map_right measure f t =
    fold_right (fun acc elt -> cons measure acc (f elt)) empty t

  (*---------------------------------*)
  (*              misc               *)
  (*---------------------------------*)
  let measure = measure_t (* no defined because many local variables are
                           * already called measure, so forgetting to bind
                           * them would cause weird type errors if this
                           * definition was in the scope *)
  let size t = fold_left (fun acc _ -> acc + 1) 0 t

end

(* can be used to check the overhead of a not dummy measure
   but apart from that??
   or can be used as a deque
module UnitMonoid =
struct
  type t = unit
  let zero = ()
  let combine () () = ()
end
let unit_measurer = fun _ -> ()

module U = struct
  module M = Make(UnitMonoid)
  type 'a t = 'a M.t
  type ('wrapped_type, 'useless) wrap_measure = 'wrapped_type
  let cons t x = M.cons unit_measurer t x
  let snoc t x = M.snoc unit_measurer t x
  let front t = M.front unit_measurer t
  let front_exn t = M.front_exn unit_measurer t
  let tail_exn t = M.tail_exn unit_measurer t
  let init_exn t = M.init_exn unit_measurer t
  let rear t = M.rear unit_measurer t
  let rear_exn t = M.rear_exn unit_measurer t
  let append t1 t2 = M.append unit_measurer t1 t2
  let measure t = M.measure unit_measurer t
  let reverse t = M.reverse unit_measurer t
end
*)

module NatPlusMonoid =
struct
  type t = int
  let zero = 0
  let combine = (+)
end
let size_measurer = fun _ -> 1

module M = Make(NatPlusMonoid) (* can't include because of the need to
                                * redefine the type wrap_measure *)
exception Empty = M.Empty
type 'a t = 'a M.t
type measure = M.measure

let last_exn = M.last_exn
(**T last_exn
   last_exn (cons (cons empty 2) 1) = 2
   try ignore (last_exn empty); false with Empty -> true
**)
(**Q last_exn
   (Q.list Q.pos_int) (fun l -> last_exn (snoc (of_list l) (-1)) = -1)
**)

let head_exn = M.head_exn
(**T head_exn
   head_exn (cons (cons empty 2) 1) = 1
   try ignore (head_exn empty); false with Empty -> true
**)
(**Q head_exn
   (Q.list Q.pos_int) (fun l -> head_exn (cons (of_list l) (-1)) = -1)
**)

let last = M.last
(**T last
   last (cons (cons empty 2) 1) = Some 2
   last_exn empty = None
**)
(**Q last
   (Q.list Q.pos_int) (fun l -> last (snoc (of_list l) (-1)) = Some (-1))
**)

let head = M.head
(**T head
   head (cons (cons empty 2) 1) = Some 1
   head empty = None
**)
(**Q head
   (Q.list Q.pos_int) (fun l -> head (cons (of_list l) (-1)) = Some (-1))
**)

let singleton = M.singleton
(**T singleton
   to_list (singleton 78) = [78]
**)

let empty = M.empty
(**T empty
   to_list empty = []
**)

let fold_left = M.fold_left
let fold_right = M.fold_right
(**T fold
   fold_left (Printf.sprintf "%s%d") "" (cons (cons empty 2) 1) = "12"
   fold_right (Printf.sprintf "%s%d") "" (cons (cons empty 2) 1) = "21"
**)

let enum = M.enum
let backwards = M.backwards
let to_list = M.to_list
let to_list_backwards = M.to_list_backwards
(**Q conversions
   (Q.list Q.int) (fun l -> to_list (of_list l) = l)
   (Q.list Q.int) (fun l -> to_list (of_list_backwards l) = List.rev l)
   (Q.list Q.int) (fun l -> to_list_backwards (of_list l) = List.rev l)
   (Q.list Q.int) (fun l -> BatList.of_enum (enum (of_list l)) = l)
   (Q.list Q.int) (fun l -> BatList.of_enum (backwards (of_list l)) = List.rev l)
   (Q.list Q.int) (fun l ->  to_list (of_enum (BatList.enum l)) = l)
**)

let iter = M.iter
let iter_right = M.iter_right
(**T iter
   let b = Buffer.create 10 in iter (Printf.bprintf b "%d") (snoc (snoc empty 1) 2); Buffer.contents b = "12"
   let b = Buffer.create 10 in iter_right (Printf.bprintf b "%d") (snoc (snoc empty 1) 2); Buffer.contents b = "21"
**)

type ('wrapped_type, 'useless) wrap_measure = 'wrapped_type
let cons t x = M.cons size_measurer t x
let snoc t x = M.snoc size_measurer t x
let front t = M.front size_measurer t
let tail t = M.tail size_measurer t
let init t = M.init size_measurer t
let rear t = M.rear size_measurer t
let front_exn t = M.front_exn size_measurer t
let tail_exn t = M.tail_exn size_measurer t
let init_exn t = M.init_exn size_measurer t
let rear_exn t = M.rear_exn size_measurer t
(**Q
  (Q.list Q.pos_int) (fun l -> tail_exn (Q.cons l (-1)) = l)
  (Q.list Q.pos_int) (fun l -> init_exn (Q.snoc l (-1)) = l)
  (Q.list Q.pos_int) (fun l -> front_exn (Q.cons l (-1)) = (-1, l))
  (Q.list Q.pos_int) (fun l -> init_exn (Q.snoc l (-1)) = (-1, l))
**)

let append t1 t2 = M.append size_measurer t1 t2
(**Q append
   (Q.list Q.int) (fun l -> let l1, l2 = BatList.split_at (List.length l) l in to_list (append (of_list l1) (of_list l2)) = l)
**)

let measure t = M.measure size_measurer t
let size = measure (* O(1) this time *)
(**Q size
   (Q.list Q.int) (fun l -> List.length l = size (of_list l))
**)

let reverse t = M.reverse size_measurer t
(**Q reverse
   (Q.list Q.int) (fun l -> to_list_backwards (reverse (of_list l)) = l)
**)

let split f t = M.split size_measurer f t
let split_at t i =
  if i < 0 || i >= size t then invalid_arg "Index out of bounds";
  split (fun index -> i < index) t
(**T split_at__split
  let n = 50 in let l = Q.lg_size (fun () -> n) Q.uig () in let t = of_list l in let i = ref (-1) in BatList.for_all (fun _ -> incr i; let t1, t2 = split_at t !i in let l1, l2 = BatList.split_at !i l in to_list t1 = l1 && to_list t2 = l2) l
   try ignore (split_at empty 0); false with Invalid_argument _ -> true
**)

let lookup f t = M.lookup size_measurer f t
let get i t =
  if i < 0 || i >= size t then invalid_arg "Index out of bounds";
  lookup (fun index -> i < index) t
(**T get__lookup
  let n = 50 in let l = Q.lg_size (fun () -> n) Q.uig () in let t = of_list l in let i = ref (-1) in BatList.for_all (fun elt -> incr i; elt = get !i t) l
   try ignore (get 1 (singleton 1)); false with Invalid_argument _ -> true
   try ignore (get (-1) (singleton 1)); false with Invalid_argument _ -> true
**)

let set i v t =
  if i < 0 || i >= size t then invalid_arg "Index out of bounds";
  let left, right = split_at t i in
  append (snoc left v) (tail_exn right)
(**T set
  to_list (set 1 4 (snoc (snoc (snoc empty 1) 2) 3)) = [1; 4; 3]
  to_list (set 0 4 (snoc (snoc (snoc empty 1) 2) 3)) = [4; 2; 3]
  to_list (set 2 4 (snoc (snoc (snoc empty 1) 2) 3)) = [1; 2; 4]
  try ignore (set (-1) 4 (snoc (snoc (snoc empty 1) 2) 3)); false with Invalid_argument _ -> true
  try ignore (set 3 4 (snoc (snoc (snoc empty 1) 2) 3)); false with Invalid_argument _ -> true
**)

let update i f t =
  set i (f (get i t)) t
(**T update
  to_list (update 1 (fun x -> x + 1) (snoc (snoc (snoc empty 1) 2) 3)) = [1; 3; 3]
  to_list (update 0 (fun x -> x + 1) (snoc (snoc (snoc empty 1) 2) 3)) = [2; 2; 3]
  to_list (update 2 (fun x -> x + 1) (snoc (snoc (snoc empty 1) 2) 3)) = [1; 2; 4]
  try ignore (update (-1) (fun x -> x + 1) (snoc (snoc (snoc empty 1) 2) 3)); false with Invalid_argument _ -> true
  try ignore (update 3 (fun x -> x + 1) (snoc (snoc (snoc empty 1) 2) 3)); false with Invalid_argument _ -> true
**)

let of_enum e = M.of_enum size_measurer e
let of_list l = M.of_list size_measurer l
let of_backwards e = M.of_backwards size_measurer e
let of_list_backwards l = M.of_list_backwards size_measurer l

let map f t = M.map size_measurer f t
let map_right f t = M.map_right size_measurer f t
(**T map
   let b = Buffer.create 10 in let res = map (fun d -> Printf.bprintf b "%d" d; d + 1) (snoc (snoc empty 1) 2) in Buffer.contents b = "12" && to_list res = [2;3]
   let b = Buffer.create 10 in let res = map_right (fun d -> Printf.bprintf b "%d" d; d + 1) (snoc (snoc empty 1) 2) in Buffer.contents b = "21" && to_list res = [2;3]
**)
