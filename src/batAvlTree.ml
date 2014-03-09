(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree * int (* height *)

let empty = Empty

let is_empty = function
  | Empty -> true
  | Node _ -> false

let singleton_tree x =
  Node (Empty, x, Empty, 1)

let left_branch = function
  | Empty -> raise Not_found
  | Node (l, _, _, _) -> l

let right_branch = function
  | Empty -> raise Not_found
  | Node (_, _, r, _) -> r

let root = function
  | Empty -> raise Not_found
  | Node (_, v, _, _) -> v

let height = function
  | Empty -> 0
  | Node (_, _, _, h) -> h


let create l v r =
  let h' = 1 + BatInt.max (height l) (height r) in
  assert (abs (height l - height r ) < 2);
  Node (l, v, r, h')

(* Assume |hl - hr| < 3 *)
let bal l v r =
  let hl = height l in
  let hr = height r in
  if hl >= hr + 2 then
    match l with
    | Empty -> assert false
    | Node (ll, lv, lr, _) ->
      if height ll >= height lr then
        create ll lv (create lr v r)
      else
        match lr with
        | Empty -> assert false
        | Node (lrl, lrv, lrr, _) ->
          create (create ll lv lrl) lrv (create lrr v r)
  else if hr >= hl + 2 then
    match r with
    | Empty -> assert false
    | Node (rl, rv, rr, _) ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else
        match rl with
        | Empty -> assert false
        | Node (rll, rlv, rlr, _) ->
          create (create l v rll) rlv (create rlr rv rr)
  else
    create l v r

let rec add_left v = function
  | Empty -> Node (Empty, v, Empty, 1)
  | Node (l, v', r, _) -> bal (add_left v l) v' r

let rec add_right v = function
  | Empty -> Node (Empty, v, Empty, 1)
  | Node (l, v', r, _) -> bal l v' (add_right v r)

(* No assumption of height of l and r. *)
let rec make_tree l v r =
  match l , r with
  | Empty, _ -> add_left v r
  | _, Empty -> add_right v l
  | Node (ll, lv, lr, lh), Node (rl, rv, rr, rh) ->
    if lh > rh + 1 then bal ll lv (make_tree lr v r) else
    if rh > lh + 1 then bal (make_tree l v rl) rv rr else
      create l v r

(* Generate pseudo-random trees in an imbalanced fashion using function [f].

   The trees generated are determined solely by the input list. *)
(*${*)
let rec of_list_for_test f = function
  | []     -> empty
  | h :: t ->
    let len = BatList.length t in
    let (l, r) = BatList.split_at (abs (h mod (len+1))) t in
    f (of_list_for_test f l) h (of_list_for_test f r)
(*$}*)

(* This tests three aspects of [make_tree] and the rebalancing algorithm:

   - The height value in a node is accurate.
   - The height of two subnodes differs at most by one (main AVL tree invariant).
   - All elements put into a tree stay in a tree even if it is rebalanced.
*)
(*$Q make_tree & ~small:List.length
  (Q.list Q.small_int) (fun l -> \
    let t = of_list_for_test make_tree l in \
    check_height_cache t && check_height_balance t \
  )
  (Q.list Q.small_int) (fun l -> \
    let t = of_list_for_test make_tree l in \
    (enum t |> List.of_enum |> List.sort compare) = List.sort compare l \
  )
*)

(* Utilities *)
let rec split_leftmost = function
  | Empty -> raise Not_found
  | Node (Empty, v, r, _) -> (v, r)
  | Node (l, v, r, _) ->
    let v0, l' = split_leftmost l in
    (v0, make_tree l' v r)

let rec split_rightmost = function
  | Empty -> raise Not_found
  | Node (l, v, Empty, _) -> (v, l)
  | Node (l, v, r, _) ->
    let v0, r' = split_rightmost r in
    (v0, make_tree l v r')

let rec concat t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | Node (l1, v1, r1, h1), Node (l2, v2, r2, h2) ->
    if h1 < h2 then
      make_tree (concat t1 l2) v2 r2
    else
      make_tree l1 v1 (concat r1 t2)

let rec iter proc = function
  | Empty -> ()
  | Node (l, v, r, _) ->
    iter proc l;
    proc v;
    iter proc r

let rec fold f t init =
  match t with
  | Empty -> init
  | Node (l, v, r, _) ->
    let x = fold f l init in
    let x = f v x in
    fold f r x

(* FIXME: this is nlog n because of the left nesting of appends *)
let rec enum = function
  | Empty -> BatEnum.empty ()
  | Node (l, v, r, _) ->
    BatEnum.append (enum l) (BatEnum.delay (fun () -> BatEnum.append (BatEnum.singleton v) (enum r)))

(* Helpers for testing *)

(* Check that the height value in a node is correct. *)
let check_height_cache t =
  let rec go = function
    | Empty -> Some 0
    | Node (l, _, r, h) ->
      let open BatOption.Monad in
      bind (go l) (fun lh ->
        bind (go r) (fun rh ->
          if max lh rh + 1 = h then Some h else None
        )
      )
  in BatOption.is_some (go t)

(* Check that the difference of the height of the left and right subnode is 0
   or 1 based on the height value in the nodes. *)
let check_height_balance t =
  let balanced l r = match (l, r) with
    | (Node (_, _, _, hl), Node (_, _, _, hr)) -> abs (hl - hr) < 2
    | _ -> true in
  let rec go = function
    | Empty -> true
    | Node (l, _, r, _) -> go l && go r && balanced l r in
  go t

(* Sanity checks *)
let check t = check_height_cache t && check_height_balance t

