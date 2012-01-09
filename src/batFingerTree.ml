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

type 'a monoid = {
  zero : 'a;
  combine : 'a -> 'a -> 'a ;
}

module type S =
sig

  type 'm m
  type ('a, 'm) fg
  type ('wrapped_type, 'a, 'm) wrap
  val empty : ('a, 'm) fg
  val singleton : 'a -> ('a, 'm) fg
  val cons : (('a, 'm) fg -> 'a -> ('a, 'm) fg, 'a, 'm) wrap
  val snoc : (('a, 'm) fg -> 'a -> ('a, 'm) fg, 'a, 'm) wrap
  val front : (('a, 'm) fg -> ('a * ('a, 'm) fg) option, 'a, 'm) wrap
  val front_exn : (('a, 'm) fg -> ('a * ('a, 'm) fg), 'a, 'm) wrap
  val head : ('a, 'm) fg -> 'a option
  val head_exn : ('a, 'm) fg -> 'a
  val last : ('a, 'm) fg -> 'a option
  val last_exn : ('a, 'm) fg -> 'a
  val tail : (('a, 'm) fg -> ('a, 'm) fg option, 'a, 'm) wrap
  val tail_exn : (('a, 'm) fg -> ('a, 'm) fg, 'a, 'm) wrap
  val init : (('a, 'm) fg -> ('a, 'm) fg option, 'a, 'm) wrap
  val init_exn : (('a, 'm) fg -> ('a, 'm) fg, 'a, 'm) wrap
  val rear : (('a, 'm) fg -> ('a * ('a, 'm) fg) option, 'a, 'm) wrap
  val rear_exn : (('a, 'm) fg -> ('a * ('a, 'm) fg), 'a, 'm) wrap
  val size : ('a, 'm) fg -> int
  val lookup : (('m m -> bool) -> ('a, 'm) fg -> 'a, 'a, 'm) wrap
  val measure : (('a, 'm) fg -> 'm m, 'a, 'm) wrap
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc
  val fold_right : ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc
  val iter : ('a -> unit) -> ('a, 'm) fg -> unit
  val iter_right : ('a -> unit) -> ('a, 'm) fg -> unit
  val enum : ('a, 'm) fg -> 'a BatEnum.t
  val backwards : ('a, 'm) fg -> 'a BatEnum.t
  val to_list : ('a, 'm) fg -> 'a list
  val to_list_backwards : ('a, 'm) fg -> 'a list
  val of_enum : ('a BatEnum.t -> ('a, 'm) fg, 'a, 'm) wrap
  val of_backwards : ('a BatEnum.t -> ('a, 'm) fg, 'a, 'm) wrap
  val of_list : ('a list -> ('a, 'm) fg, 'a, 'm) wrap
  val of_list_backwards : ('a list -> ('a, 'm) fg, 'a, 'm) wrap
  val map : (('a -> 'b) -> ('a, 'm) fg -> ('b, 'm) fg, 'b, 'm) wrap
  val map_right : (('a -> 'b) -> ('a, 'm) fg -> ('b, 'm) fg, 'b, 'm) wrap
  val append : (('a, 'm) fg -> ('a, 'm) fg -> ('a, 'm) fg, 'a, 'm) wrap
  val reverse : (('a, 'm) fg -> ('a, 'm) fg, 'a, 'm) wrap
  val split : (('m m -> bool) -> ('a, 'm) fg -> ('a, 'm) fg * ('a, 'm) fg, 'a, 'm) wrap
  val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) -> 'a BatInnerIO.output -> ('b, _) fg -> unit
  val t_printer : 'a BatValuePrinter.t -> ('a, _) fg BatValuePrinter.t

end

exception Empty (* the name collides with a constructor below,
                 * so making an alias *)
exception EmptyAlias = Empty

module Generic : S
  with type ('wrapped_type, 'a, 'm) wrap = monoid:'m monoid -> measure:('a -> 'm) -> 'wrapped_type
  and type 'm m = 'm
= struct

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

  type ('a, 'm) node =
    | Node2 of 'm * 'a * 'a
    | Node3 of 'm * 'a * 'a * 'a
  type ('a, 'm) digit =
    | One of 'm * 'a
    | Two of 'm * 'a * 'a
    | Three of 'm * 'a * 'a * 'a
    | Four of 'm * 'a * 'a * 'a * 'a
  type ('a, 'm) fg =
    | Empty
    | Single of 'a
    | Deep of 'm * ('a, 'm) digit * (('a, 'm) node, 'm) fg * ('a, 'm) digit
  type 'm m = 'm

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

  type ('acc, 'a, 'm) fold = ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc
  let rec fold_right : ('acc, 'a, 'm) fold = fun f acc -> function
    | Empty -> acc
    | Single x -> f acc x
    | Deep (_, pr, m, sf) ->
      let acc = fold_right_digit f acc sf in
      let acc = (poly_recursion fold_right : ('acc, ('a, 'm) node, 'm) fold) (fun acc elt -> fold_right_node f acc elt) acc m in
      let acc = fold_right_digit f acc pr in
      acc
  let rec fold_left : ('acc, 'a, 'm) fold = fun f acc -> function
    | Empty -> acc
    | Single x -> f acc x
    | Deep (_, pr, m, sf) ->
      let acc = fold_left_digit f acc pr in
      let acc = (poly_recursion fold_left : ('acc, ('a, 'm) node, 'm) fold) (fun acc elt -> fold_left_node f acc elt) acc m in
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

  type ('a, 'm) pp_debug_tree = (Format.formatter -> 'm -> unit) -> (Format.formatter -> 'a -> unit) -> Format.formatter -> ('a, 'm) fg -> unit
  let rec pp_debug_tree : ('a, 'm) pp_debug_tree = fun pp_measure pp_a f -> function
    | Empty -> Format.fprintf f "Empty"
    | Single a -> Format.fprintf f "@[<2>Single@ %a@]" pp_a a
    | Deep (v, pr, m, sf) ->
      Format.fprintf f "@[@[<v2>Deep (@,%a,@ %a,@ %a,@ %a@]@\n)@]"
        pp_measure v
        (pp_debug_digit pp_measure pp_a) pr
        ((poly_recursion pp_debug_tree : (('a, 'm) node, 'm) pp_debug_tree)
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
    | Empty -> monoid.zero
    | Single x -> measure_node x
    | Deep (v, _, _, _) -> v
  let measure_t ~monoid ~measure = function
    | Empty -> monoid.zero
    | Single x -> measure x
    | Deep (v, _, _, _) -> v

  (*---------------------------------*)
  (*  a bunch of smart constructors  *)
  (*---------------------------------*)
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

  (*---------------------------------*)
  (*          cons / snoc            *)
  (*---------------------------------*)
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

  type ('a, 'm) cons_aux = monoid:'m monoid -> (('a, 'm) node, 'm) fg -> ('a, 'm) node -> (('a, 'm) node, 'm) fg
  let rec cons_aux : ('a, 'm) cons_aux = fun ~monoid t a ->
    match t with
    | Empty -> Single a
    | Single b -> deep ~monoid (one_node a) Empty (one_node b)
    | Deep (_, Four (_, b, c, d, e), m, sf) -> deep ~monoid (two_node ~monoid a b) ((poly_recursion cons_aux : (('a, 'm) node, 'm) cons_aux) ~monoid m (node3_node ~monoid c d e)) sf
    | Deep (v, pr, m, sf) -> Deep (monoid.combine (measure_node a) v, cons_digit_node ~monoid pr a, m, sf)
  let cons ~monoid ~measure t a =
    match t with
    | Empty -> Single a
    | Single b -> deep ~monoid (one measure a) Empty (one measure b)
    | Deep (_, Four (_, b, c, d, e), m, sf) -> deep ~monoid (two ~monoid ~measure a b) (cons_aux ~monoid m (node3 ~monoid ~measure c d e)) sf
    | Deep (v, pr, m, sf) -> Deep (monoid.combine (measure a) v, cons_digit ~monoid ~measure pr a, m, sf)

  let rec snoc_aux : ('a, 'm) cons_aux = fun ~monoid t a ->
    match t with
    | Empty -> Single a
    | Single b -> deep ~monoid (one_node b) Empty (one_node a)
    | Deep (_, pr, m, Four (_, b, c, d, e)) -> deep ~monoid pr ((poly_recursion snoc_aux : (('a, 'm) node, 'm) cons_aux) ~monoid m (node3_node ~monoid b c d)) (two_node ~monoid e a)
    | Deep (v, pr, m, sf) -> Deep (monoid.combine v (measure_node a), pr, m, snoc_digit_node ~monoid sf a)
  let snoc ~monoid ~measure t a =
    match t with
    | Empty -> Single a
    | Single b -> deep ~monoid (one ~measure b) Empty (one ~measure a)
    | Deep (_, pr, m, Four (_, b, c, d, e)) -> deep ~monoid pr (snoc_aux ~monoid m (node3 ~monoid ~measure b c d)) (two ~measure ~monoid e a)
    | Deep (v, pr, m, sf) -> Deep (monoid.combine v (measure a), pr, m, snoc_digit ~monoid ~measure sf a)

  (*---------------------------------*)
  (*     various conversions         *)
  (*---------------------------------*)
  let to_tree_digit ~monoid ~measure d =
    match d with
    | One (_, a) -> Single a
    | Two (v, a, b) -> Deep (v, one ~measure a, Empty, one ~measure b)
    | Three (v, a, b, c) -> Deep (v, two ~monoid ~measure a b, Empty, one ~measure c)
    | Four (v, a, b, c, d) -> Deep (v, three ~monoid ~measure a b c, Empty, one ~measure d)
  let to_tree_list ~monoid ~measure = function
    | [] -> Empty
    | [a] -> Single a
    | [a; b] -> deep ~monoid (one ~measure a) Empty (one ~measure b)
    | [a; b; c] -> deep ~monoid (two ~monoid ~measure a b) Empty (one ~measure c)
    | [a; b; c; d] -> deep ~monoid (three ~monoid ~measure a b c) Empty (one ~measure d)
    | _ -> assert false
  let to_tree_digit_node ~monoid d =
    to_tree_digit ~monoid ~measure:measure_node d

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

  type ('a, 'm) view_aux = monoid:'m monoid -> ('a, 'm) fg -> ('a, ('a, 'm) fg) view
  let rec view_left_aux : ('a, 'm) view_aux = fun ~monoid -> function
    | Empty -> Vnil
    | Single x -> Vcons (x, Empty)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match (poly_recursion view_left_aux : (('a, 'm) node, 'm) view_aux) ~monoid m with
        | Vnil -> to_tree_digit_node ~monoid sf
        | Vcons (a, m') -> deep ~monoid (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid (tail_digit_node ~monoid pr) m sf in
      Vcons (head_digit pr, vcons)
  let view_left ~monoid ~measure = function
    | Empty -> Vnil
    | Single x -> Vcons (x, Empty)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match view_left_aux ~monoid m with
        | Vnil -> to_tree_digit ~monoid ~measure sf
        | Vcons (a, m') -> deep ~monoid (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid (tail_digit ~monoid ~measure pr) m sf in
      Vcons (head_digit pr, vcons)

  let rec view_right_aux : ('a, 'm) view_aux = fun ~monoid -> function
    | Empty -> Vnil
    | Single x -> Vcons (x, Empty)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match (poly_recursion view_right_aux : (('a, 'm) node, 'm) view_aux) ~monoid m with
        | Vnil -> to_tree_digit_node ~monoid pr
        | Vcons (a, m') -> deep ~monoid pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid pr m (init_digit_node ~monoid sf) in
      Vcons (last_digit sf, vcons)
  let view_right ~monoid ~measure = function
    | Empty -> Vnil
    | Single x -> Vcons (x, Empty)
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

  let tail ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let tail_exn ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> raise EmptyAlias
    | Vcons (_, tl) -> tl

  let front ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (hd, tl)
  let front_exn ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> raise EmptyAlias
    | Vcons (hd, tl) -> (hd, tl)

  let init ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let init_exn ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> raise EmptyAlias
    | Vcons (_, tl) -> tl

  let rear ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (hd, tl)
  let rear_exn ~monoid ~measure t =
    match view_right ~monoid ~measure t with
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

    let rec nodes_aux ~monoid ~measure ts sf2 = (* no idea if this should be tail rec *)
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

  type ('a, 'm) app3 = monoid:'m monoid -> measure:('a -> 'm) -> ('a, 'm) fg -> 'a list -> ('a, 'm) fg -> ('a, 'm) fg
  let rec app3 ~monoid ~measure t1 elts t2 =
    match t1, t2 with
    | Empty, _ -> List.fold_right (fun elt acc -> cons ~monoid ~measure acc elt) elts t2
    | _, Empty -> List.fold_left (fun acc elt -> snoc ~monoid ~measure acc elt) t1 elts
    | Single x1, _ -> cons ~monoid ~measure (List.fold_right (fun elt acc -> cons ~monoid ~measure acc elt) elts t2) x1
    | _, Single x2 -> snoc ~monoid ~measure (List.fold_left (fun acc elt -> snoc ~monoid ~measure acc elt) t1 elts) x2
    | Deep (v1, pr1, m1, sf1), Deep (v2, pr2, m2, sf2) ->
      Deep (monoid.combine v1 v2, pr1, ((poly_recursion app3 : (('a, 'm) node, 'm) app3) ~monoid ~measure:measure_node m1 (nodes ~monoid ~measure sf1 elts pr2) m2), sf2)

  let append ~monoid ~measure t1 t2 = app3 ~monoid ~measure t1 [] t2

  (*---------------------------------*)
  (*            reverse              *)
  (*---------------------------------*)
  (* unfortunately, when reversing, we need to rebuild every annotation
   * because the monoid does not have to be commutative *)

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

  type ('a, 'm) reverse_aux = monoid:'m monoid -> (('a, 'm) node -> ('a, 'm) node) -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, 'm) fg
  let rec reverse_aux : ('a, 'm) reverse_aux = fun ~monoid reverse_a -> function
    | Empty -> Empty
    | Single a -> Single (reverse_a a)
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit_node ~monoid reverse_a pr in
      let rev_sf = reverse_digit_node ~monoid reverse_a sf in
      let rev_m =
        (poly_recursion reverse_aux : (('a, 'm) node, 'm) reverse_aux)
          ~monoid (reverse_node_node ~monoid (reverse_a)) m in
      deep ~monoid rev_sf rev_m rev_pr
  let reverse ~monoid ~measure = function
    | Empty
    | Single _ as t -> t
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit ~monoid ~measure pr in
      let rev_sf = reverse_digit ~monoid ~measure sf in
      let rev_m = reverse_aux ~monoid (reverse_node ~monoid ~measure) m in
      deep ~monoid rev_sf rev_m rev_pr

  (*---------------------------------*)
  (*             split               *)
  (*---------------------------------*)
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

  type ('a, 'm) split_tree = monoid:'m monoid -> measure:('a -> 'm) -> ('m -> bool) -> 'm -> ('a, 'm) fg -> ('a, ('a, 'm) fg) split
  let rec split_tree : ('a, 'm) split_tree = fun ~monoid ~measure p i -> function
    | Empty -> raise EmptyAlias
    | Single x -> Split (Empty, x, Empty)
    | Deep (_, pr, m, sf) ->
      let vpr = monoid.combine i (measure_digit pr) in
      if p vpr then
        let Split (l, x, r) = split_digit ~monoid ~measure p i pr in
        Split (to_tree_list ~monoid ~measure l, x, deep_left ~monoid ~measure r m sf)
      else
        let vm = monoid.combine vpr (measure_t_node ~monoid m) in
        if p vm then
          let Split (ml, xs, mr) = (poly_recursion split_tree : (('a, 'm) node, 'm) split_tree) ~monoid ~measure:measure_node p vpr m in
          let Split (l, x, r) = split_digit ~monoid ~measure p (monoid.combine vpr (measure_t_node ~monoid ml)) (to_digit_node xs) in
          Split (deep_right ~monoid ~measure pr ml l, x, deep_left ~monoid ~measure r mr sf)
        else
          let Split (l, x, r) = split_digit ~monoid ~measure p vm sf in
          Split (deep_right ~monoid ~measure pr m l, x, to_tree_list ~monoid ~measure r)

  let split ~monoid ~measure f t =
    match t with
    | Empty -> (Empty, Empty)
    | _ ->
      if f (measure_t ~monoid ~measure t) then
        let Split (l, x, r) = split_tree ~monoid ~measure f monoid.zero t in
        (l, cons ~monoid ~measure r x)
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

  type ('a, 'm) lookup_tree = monoid:'m monoid -> measure:('a -> 'm) -> ('m -> bool) -> 'm -> ('a, 'm) fg -> 'm * 'a
  let rec lookup_tree : ('a, 'm) lookup_tree = fun ~monoid ~measure p i -> function
    | Empty -> raise EmptyAlias
    | Single x -> monoid.zero, x
    | Deep (_, pr, m, sf) ->
      let m_pr = measure_digit pr in
      let vpr = monoid.combine i m_pr in
      if p vpr then lookup_digit ~monoid ~measure p i pr else
        let m_m = measure_t_node ~monoid m in
        let vm = monoid.combine vpr m_m in
        if p vm then
          let v_left, node =
            (poly_recursion lookup_tree : (('a, 'm) node, 'm) lookup_tree)
              ~monoid ~measure:measure_node p vpr m in
          let v, x = lookup_node ~monoid ~measure p (monoid.combine vpr v_left) node in
          monoid.combine (monoid.combine m_pr v_left) v, x
        else
          let v, x = lookup_digit ~monoid ~measure p vm sf in
          monoid.combine (monoid.combine m_pr m_m) v, x

  let lookup ~monoid ~measure p t =
    snd (lookup_tree ~monoid ~measure p monoid.zero t)

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

  type 'a iter = unit -> 'a * 'a iter
  type ('input, 'output) iter_into = 'input -> 'output iter -> 'output * 'output iter
  type ('top_a, 'a, 'm) enum_aux = ('a, 'top_a) iter_into -> (('a, 'm) fg, 'top_a) iter_into
  let rec enum_aux : ('top_a, 'a, 'm) enum_aux = fun enum_a t k ->
    match t with
    | Empty -> k ()
    | Single a -> enum_a a k
    | Deep (_, pr, m, sf) ->
      enum_digit enum_a pr (fun () ->
        (poly_recursion enum_aux : ('top_a, ('a, 'm) node, 'm) enum_aux) (enum_node enum_a) m (fun () ->
          enum_digit enum_a sf k
        )
      )
  let enum_cps t = enum_aux enum_base t (fun () -> raise BatEnum.No_more_elements)

  let rec enum_aux_backwards  : ('top_a, 'a, 'm) enum_aux = fun enum_a t k ->
    match t with
    | Empty -> k ()
    | Single a -> enum_a a k
    | Deep (_, pr, m, sf) ->
      enum_digit_backwards enum_a sf (fun () ->
        (poly_recursion enum_aux_backwards : ('top_a, ('a, 'm) node, 'm) enum_aux) (enum_node_backwards enum_a) m (fun () ->
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

  let of_enum ~monoid ~measure enum =
    BatEnum.fold (fun t elt -> snoc ~monoid ~measure t elt) empty enum
  let of_backwards ~monoid ~measure enum =
    BatEnum.fold (fun t elt -> cons ~monoid ~measure t elt) empty enum

  let to_list t =
    BatList.of_backwards (backwards t)
  let to_list_backwards t =
    BatList.of_backwards (enum t)
  let of_list ~monoid ~measure l =
    List.fold_left (fun t elt -> snoc ~monoid ~measure t elt) empty l
  let of_list_backwards ~monoid ~measure l =
    List.fold_left (fun t elt -> cons ~monoid ~measure t elt) empty l

  (*---------------------------------*)
  (*        classic traversals       *)
  (*---------------------------------*)
  let iter f t =
    fold_left (fun () elt -> f elt) () t
  let iter_right f t =
    fold_right (fun () elt -> f elt) () t
  let map ~monoid ~measure f t =
    fold_left (fun acc elt -> snoc ~monoid ~measure acc (f elt)) empty t
  let map_right ~monoid ~measure f t =
    fold_right (fun acc elt -> cons ~monoid ~measure acc (f elt)) empty t

  (*---------------------------------*)
  (*              misc               *)
  (*---------------------------------*)
  let measure = measure_t (* no defined because many local variables are
                           * already called measure, so forgetting to bind
                           * them would cause weird type errors if this
                           * definition was in the scope *)
  let size t = fold_left (fun acc _ -> acc + 1) 0 t

  let print ?first ?last ?sep f oc x =
    BatEnum.print ?first ?last ?sep f oc (enum x)
  let t_printer a_printer paren out e =
    print ~first:"[" ~sep:"; " ~last:"]" (a_printer false) out e

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

type nat = int
let nat_plus_monoid = {
  zero = 0;
  combine = (+);
}
let size_measurer = fun _ -> 1

type measure = nat
type ('a, 'm) fg = ('a, int) Generic.fg
type 'a t = ('a, measure) fg
type 'm m = measure

let last_exn = Generic.last_exn
(**T last_exn
   last_exn (cons (cons empty 2) 1) = 2
   try ignore (last_exn empty); false with Empty -> true
**)
(**Q last_exn
   (Q.list Q.pos_int) (fun l -> last_exn (snoc (of_list l) (-1)) = -1)
**)

let head_exn = Generic.head_exn
(**T head_exn
   head_exn (cons (cons empty 2) 1) = 1
   try ignore (head_exn empty); false with Empty -> true
**)
(**Q head_exn
   (Q.list Q.pos_int) (fun l -> head_exn (cons (of_list l) (-1)) = -1)
**)

let last = Generic.last
(**T last
   last (cons (cons empty 2) 1) = Some 2
   last empty = None
**)
(**Q last
   (Q.list Q.pos_int) (fun l -> last (snoc (of_list l) (-1)) = Some (-1))
**)

let head = Generic.head
(**T head
   head (cons (cons empty 2) 1) = Some 1
   head empty = None
**)
(**Q head
   (Q.list Q.pos_int) (fun l -> head (cons (of_list l) (-1)) = Some (-1))
**)

let singleton = Generic.singleton
(**T singleton
   to_list (singleton 78) = [78]
**)

let empty = Generic.empty
(**T empty
   to_list empty = []
**)

let fold_left = Generic.fold_left
let fold_right = Generic.fold_right
(**T fold
   fold_left (Printf.sprintf "%s%d") "" (cons (cons empty 2) 1) = "12"
   fold_right (Printf.sprintf "%s%d") "" (cons (cons empty 2) 1) = "21"
**)

let enum = Generic.enum
let backwards = Generic.backwards
let to_list = Generic.to_list
let to_list_backwards = Generic.to_list_backwards
(**Q conversions
   (Q.list Q.int) (fun l -> to_list (of_list l) = l)
   (Q.list Q.int) (fun l -> to_list (of_list_backwards l) = List.rev l)
   (Q.list Q.int) (fun l -> to_list_backwards (of_list l) = List.rev l)
   (Q.list Q.int) (fun l -> BatList.of_enum (enum (of_list l)) = l)
   (Q.list Q.int) (fun l -> BatList.of_enum (backwards (of_list l)) = List.rev l)
   (Q.list Q.int) (fun l ->  to_list (of_enum (BatList.enum l)) = l)
**)

let iter = Generic.iter
let iter_right = Generic.iter_right
(**T iter
   let b = Buffer.create 10 in iter (Printf.bprintf b "%d") (snoc (snoc empty 1) 2); Buffer.contents b = "12"
   let b = Buffer.create 10 in iter_right (Printf.bprintf b "%d") (snoc (snoc empty 1) 2); Buffer.contents b = "21"
**)

type ('wrapped_type, 'a, 'm) wrap = 'wrapped_type

let cons t x = Generic.cons ~monoid:nat_plus_monoid ~measure:size_measurer t x
let snoc t x = Generic.snoc ~monoid:nat_plus_monoid ~measure:size_measurer t x
let front t = Generic.front ~monoid:nat_plus_monoid ~measure:size_measurer t
let tail t = Generic.tail ~monoid:nat_plus_monoid ~measure:size_measurer t
let init t = Generic.init ~monoid:nat_plus_monoid ~measure:size_measurer t
let rear t = Generic.rear ~monoid:nat_plus_monoid ~measure:size_measurer t
let front_exn t = Generic.front_exn ~monoid:nat_plus_monoid ~measure:size_measurer t
let tail_exn t = Generic.tail_exn ~monoid:nat_plus_monoid ~measure:size_measurer t
let init_exn t = Generic.init_exn ~monoid:nat_plus_monoid ~measure:size_measurer t
let rear_exn t = Generic.rear_exn ~monoid:nat_plus_monoid ~measure:size_measurer t
(**Q
  (Q.list Q.pos_int) (fun l -> to_list (tail_exn (cons (of_list l) (-1))) = l)
  (Q.list Q.pos_int) (fun l -> to_list (init_exn (snoc (of_list l) (-1))) = l)
  (Q.list Q.pos_int) (fun l -> let (hd, tl) = front_exn (cons (of_list l) (-1)) in (hd, to_list tl) = (-1, l))
  (Q.list Q.pos_int) (fun l -> let (hd, tl) = rear_exn (snoc (of_list l) (-1)) in (hd, to_list tl) = (-1, l))
**)

let append t1 t2 = Generic.append ~monoid:nat_plus_monoid ~measure:size_measurer t1 t2
(**Q append
   (Q.list Q.int) (fun l -> let l1, l2 = BatList.split_at (List.length l / 2) l in to_list (append (of_list l1) (of_list l2)) = l)
**)

let measure t = Generic.measure ~monoid:nat_plus_monoid ~measure:size_measurer t
let size = measure (* O(1) this time *)
(**Q size
   (Q.list Q.int) (fun l -> List.length l = size (of_list l))
**)

let reverse t = Generic.reverse ~monoid:nat_plus_monoid ~measure:size_measurer t
(**Q reverse
   (Q.list Q.int) (fun l -> to_list_backwards (reverse (of_list l)) = l)
**)

let split f t = Generic.split ~monoid:nat_plus_monoid ~measure:size_measurer f t
let split_at t i =
  if i < 0 || i >= size t then invalid_arg "Index out of bounds";
  split (fun index -> i < index) t
(**T split_at__split
  let n = 50 in let l = Q.lg_size (fun () -> n) Q.uig () in let t = of_list l in let i = ref (-1) in BatList.for_all (fun _ -> incr i; let t1, t2 = split_at t !i in let l1, l2 = BatList.split_at !i l in to_list t1 = l1 && to_list t2 = l2) l
   try ignore (split_at empty 0); false with Invalid_argument _ -> true
**)

let lookup f t = Generic.lookup ~monoid:nat_plus_monoid ~measure:size_measurer f t
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

let of_enum e = Generic.of_enum ~monoid:nat_plus_monoid ~measure:size_measurer e
let of_list l = Generic.of_list ~monoid:nat_plus_monoid ~measure:size_measurer l
let of_backwards e = Generic.of_backwards ~monoid:nat_plus_monoid ~measure:size_measurer e
let of_list_backwards l = Generic.of_list_backwards ~monoid:nat_plus_monoid ~measure:size_measurer l

let map f t = Generic.map ~monoid:nat_plus_monoid ~measure:size_measurer f t
let map_right f t = Generic.map_right ~monoid:nat_plus_monoid ~measure:size_measurer f t
(**T map
   let b = Buffer.create 10 in let res = map (fun d -> Printf.bprintf b "%d" d; d + 1) (snoc (snoc empty 1) 2) in Buffer.contents b = "12" && to_list res = [2;3]
   let b = Buffer.create 10 in let res = map_right (fun d -> Printf.bprintf b "%d" d; d + 1) (snoc (snoc empty 1) 2) in Buffer.contents b = "21" && to_list res = [2;3]
**)

let print = Generic.print
let t_printer = Generic.t_printer
