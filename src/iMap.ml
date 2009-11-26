(* $Id: iMap.ml,v 1.2 2006/08/13 17:13:01 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

(*
let (>!) = (>)

let compare_uint n1 n2 =
  let sgn1 = (n1 lsr 24) - (n2 lsr 24) in
  if sgn1 = 0 then (n1 land 0xffffff) - (n2 land 0xffffff) else sgn1

let (>) n1 n2 = compare_uint n1 n2 > 0
let (>=) n1 n2 = compare_uint n1 n2 >= 0
let (<) n1 n2 = compare_uint n1 n2 < 0
let (<=) n1 n2 = compare_uint n1 n2 <= 0
let compare = compare_uint

let max n1 n2 = if n1 >= n2 then n1 else n2
let min n1 n2 = if n1 <= n2 then n1 else n2

let max_int = ~-1
let min_int = 0
*)

type 'a t = (int * int * 'a) AvlTree.tree

type 'a map = 'a t

type key = int

include AvlTree

let singleton n v = singleton_tree (n, n, v)

let make ?(eq = (==)) l (n1, n2, v) r =
  let n1, l =
    if is_empty l || n1 = min_int then n1, empty else
    let (k1, k2, v0), l' = split_rightmost l in
    if k2 + 1 = n1 && eq v v0 then k1, l' else n1, l in
  let n2, r =
    if is_empty r || n2 = max_int then n2, empty else
    let (k1, k2, v0), r' = split_leftmost r in
    if n2 + 1 = k1 && eq v v0 then k2, r' else n2, r in
  make_tree l (n1, n2, v) r

let rec add ?(eq = (==)) n v m =
  if is_empty m then make_tree empty (n, n, v) empty else
  let (n1, n2, v0) as x = root m in
  let l = left_branch m in
  let r = right_branch m in
  if n1 <> min_int && n = n1 - 1 && eq v v0 then
    make l (n, n2, v) r
  else if n < n1 then
    make_tree (add n v l) x r
  else if n1 <= n && n <= n2 then
    if eq v v0 then m else
    let l = 
      if n1 = n then l else
      make_tree l (n1, n - 1, v0) empty in
    let r =
      if n2 = n then r else
      make_tree empty (n + 1, n2, v0) r in
    make l (n, n, v) r
  else if n2 <> max_int && n = n2 + 1 && eq v v0 then
    make l (n1, n, v) r
  else
    make_tree l x (add n v r)

let rec from n s =
  if is_empty s then empty else
  let (n1, n2, v) as x = root s in
  let s0 = left_branch s in
  let s1 = right_branch s in
  if n < n1 then make_tree (from n s0) x s1 else
  if n > n2 then from n s1 else
  make_tree empty (n, n2, v)  s1

let after n s = if n = max_int then empty else from (n + 1) s

let rec until n s =
  if is_empty s then empty else
  let (n1, n2, v) as x = root s in
  let s0 = left_branch s in
  let s1 = right_branch s in
  if n > n2 then make_tree s0 x (until n s1) else
  if n < n1 then until n s0 else
  make_tree s0 (n1, n, v) empty

let rec before n s = if n = min_int then empty else until (n - 1) s

let add_range ?eq n1 n2 v s =
  if n1 > n2 then invalid_arg "IMap.add_range" else
  make ?eq (before n1 s) (n1, n2, v) (after n2 s)

let rec find n m =
  if is_empty m then raise Not_found else
  let (n1, n2, v) = root m in
  if n < n1 then find n (left_branch m) else
  if n1 <= n && n <= n2 then v else
  find n (right_branch m)

let rec remove n m =
  if is_empty m then empty else
  let (n1, n2, v) as x = root m in
  let l = left_branch m in
  let r = right_branch m in
  if n < n1 then
    make_tree (remove n l) x r
  else if n1 = n then
    if n2 = n then concat l r else
    make_tree l (n + 1, n2, v) r
  else if n1 < n && n < n2 then
    make_tree (make_tree l (n1, n - 1, v) empty) (n + 1, n2, v) r
  else if n = n2 then
    make_tree l (n1, n - 1, v) r
  else
    make_tree l x (remove n r)

let remove_range n1 n2 m =
  if n1 > n2 then invalid_arg "IMap.remove_range" else
  concat (before n1 m) (after n2 m)

let rec mem n m =
  if is_empty m then false else
  let (n1, n2, _) = root m in
  if n < n1 then mem n (left_branch m) else
  if n1 <= n && n <= n2 then true else
  mem n (right_branch m)

let iter_range proc m =
  AvlTree.iter (fun (n1, n2, v) -> proc n1 n2 v) m

let fold_range f m a =
  AvlTree.fold (fun (n1, n2, v) a -> f n1 n2 v a) m a

let fold f m a =
  let rec loop n1 n2 v a =
    let a = f n1 v a in
    if n1 = n2 then a else
    loop (n1 + 1) n2 v a in
  fold_range loop m a

let iter proc m =
  fold (fun n v () -> proc n v) m ()

let rec map ?eq f m =
  if is_empty m then empty else
  let n1, n2, v = root m in
  let l = map f (left_branch m) in
  let r = map f (right_branch m) in
  let v = f v in
  make ?eq l (n1, n2, v) r

let mapi ?eq f m = fold (fun n v a -> add ?eq n (f n v) a) m empty

let rec set_to_map s v =
  if is_empty s then empty else
  let (n1, n2) = root s in
  let l = left_branch s in
  let r = right_branch s in
  make_tree (set_to_map l v) (n1, n2, v) (set_to_map r v)

let rec domain m =
  if is_empty m then empty else
  let (k1, k2, _), m' = split_leftmost m in
  let f n1 n2 _ (k1, k2, s) =
    if k1 = n2 + 1 then (k1, n2, s) else
    (n1, n2, make_tree s (k1, k2) empty) in
  let k1, k2, s =fold_range f m' (k1, k2, empty) in
  make_tree s (k1, k2) empty

let rec map_to_set p m =
  let rec loop m =
    if is_empty m then None else
    let (k1, k2, v), m' = split_leftmost m in
    if p v then Some (k1, k2, m') else
    loop m' in
  match loop m with
    Some (k1, k2, m') ->
      let f n1 n2 v (k1, k2, s) =
	if p v then
	  if k1 = n2 + 1 then (k1, n2, s) else
	  (n1, n2, make_tree s (k1, k2) empty) 
	else
	  (k1, k2, s) in
      let (k1, k2, s) = fold_range f m' (k1, k2, empty) in
      make_tree s (k1, k2) empty
  | None -> empty
