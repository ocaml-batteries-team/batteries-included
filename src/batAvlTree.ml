(* $Id: avlTree.ml,v 1.2 2003/06/08 04:50:48 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree * int

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let singleton_tree x = Node (Empty, x, Empty, 1)

let left_branch = function
    Empty -> raise Not_found
  | Node (l, _, _, _) -> l

let right_branch = function
    Empty -> raise Not_found
  | Node (_, _, r, _) -> r

let root = function
    Empty -> raise Not_found
  | Node (_, v, _, _) -> v

let height = function
    Empty -> 0
  | Node (_, _, _, h) -> h


let create l v r =
  let h' = 1 + max (height l) (height r) in
  assert(abs (height l - height r ) < 2);
  Node (l, v, r, h')

(* Assume |hl - hr| < 3 *)
let rec bal l v r =
  let hl = height l in
  let hr = height r in
  if hl >= hr + 2 then
    match l with
      Empty -> assert false
    | Node (ll, lv, lr, _) ->
	if height ll >= height lr then
	  create ll lv (create lr v r)
	else
	  match lr with
	    Empty -> assert false
	  | Node (lrl, lrv, lrr, _) ->
	      create (create ll lv lrl) lrv (create lrr v r)
  else if hr >= hl + 2 then
    match r with
      Empty -> assert false
    | Node (rl, rv, rr, _) ->
	if height rr >= height rl then
	  create (create l v rl) rv rr 
	else
	  match rl with
	    Empty -> assert false
	  | Node (rll, rlv, rlr, _) ->
	      create (create l v rll) rlv (create rlr rv rr) 
  else
    create l v r

let rec add_left v = function
    Empty -> Node(Empty, v, Empty, 1)
  | Node(l, v', r, _) -> bal (add_left v l) v' r

let rec add_right v = function
    Empty -> Node(Empty, v, Empty, 1)
  | Node(l, v', r, _) -> bal l v' (add_right v r)

(* No assumption of height of l and r. *)
let rec make_tree l v r =
  match l , r with
    Empty, _ -> add_left v r
  | _, Empty -> add_right v l
  | Node(ll, lv, lr, lh), Node(rl, rv, rr, rh) ->
      if lh > rh + 1 then bal ll lv (make_tree lr v r) else
      if rh > lh + 1 then bal (make_tree l v rl) rv rr else
      create l v r

(* Utilities *)
let rec split_leftmost = function
    Empty -> raise Not_found
  | Node (Empty, v, r, _) -> (v, r)
  | Node (l, v, r, _) ->
      let v0, l' = split_leftmost l in
      (v0, make_tree l' v r)

let rec split_rightmost = function
    Empty -> raise Not_found
  | Node (l, v, Empty, _) -> (v, l)
  | Node (l, v, r, _) ->
      let v0, r' = split_rightmost r in
      (v0, make_tree l v r')

let rec concat t1 t2 =
  match t1, t2 with
    Empty, _ -> t2
  | _, Empty -> t1
  | Node (l1, v1, r1, h1), Node (l2, v2, r2, h2) ->
      if h1 < h2 then
	make_tree (concat t1 l2) v2 r2
      else
	make_tree l1 v1 (concat r1 t2)

let rec iter proc = function
    Empty -> ()
  | Node (l, v, r, _) ->
      iter proc l;
      proc v;
      iter proc r

let rec fold f t init =
  match t with
    Empty -> init
  | Node (l, v, r, _) ->
      let x = fold f l init in
      let x = f v x in
      fold f r x
	
let rec enum = function
  | Empty             -> BatEnum.empty ()
  | Node (l, v, r, _) ->
      BatEnum.append (enum l) (BatEnum.delay (fun () -> BatEnum.append (BatEnum.singleton v) (enum r)))

let rec enum_f f = function
  | Empty             -> BatEnum.empty ()
  | Node (l, v, r, _) ->
      BatEnum.append (enum l) (BatEnum.delay (fun () -> BatEnum.append (BatEnum.singleton (f v)) (enum r)))

(*
let rec enum_post = function
  | Empty             -> BatEnum.empty ()
  | Node (l, v, r, _) ->
      BatEnum.append (enum l) (BatEnum.delay (fun () -> BatEnum.append (enum r) (BatEnum.singleton v)))
*)
