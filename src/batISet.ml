(* $Id: iSet.ml,v 1.2 2006/08/13 17:13:01 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

include BatAvlTree

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

type t = (int * int) tree
type elt = int

let rec mem n s =
  if is_empty s then false else
  let v1, v2 = root s in
  if n < v1 then mem n (left_branch s) else
  if v1 <= n && n <= v2 then true else
  mem n (right_branch s)

let rec add n s =
  if is_empty s then make_tree empty (n, n) empty else
  let (v1, v2) as v = root s in
  let s0 = left_branch s in
  let s1 = right_branch s in
  if v1 <> min_int && n < v1 - 1 then make_tree (add n s0) v s1 else
  if v2 <> max_int && n > v2 + 1 then make_tree s0 v (add n s1) else
  if n + 1 = v1 then
    if not (is_empty s0) then
      let (u1, u2), s0' = split_rightmost s0 in
      if u2 <> max_int && u2 + 1 = n then
	make_tree s0' (u1, v2) s1
      else
	make_tree s0 (n, v2) s1
    else
      make_tree s0 (n, v2) s1
  else if v2 + 1 = n then
    if not (is_empty s1) then
      let (u1, u2), s1' = split_leftmost s1 in
      if n <> max_int && n + 1 = u1 then
	make_tree s0 (v1, u2) s1'
      else
	make_tree s0 (v1, n) s1
    else
      make_tree s0 (v1, n) s1
  else s

let rec from n s =
  if is_empty s then empty else
  let (v1, v2) as v = root s in
  let s0 = left_branch s in
  let s1 = right_branch s in
  if n < v1 then make_tree (from n s0) v s1 else
  if n > v2 then from n s1 else
  make_tree empty (n, v2) s1

let after n s = if n = max_int then empty else from (n + 1) s

let rec until n s =
  if is_empty s then empty else
  let (v1, v2) as v = root s in
  let s0 = left_branch s in
  let s1 = right_branch s in
  if n > v2 then make_tree s0 v (until n s1) else
  if n < v1 then until n s0 else
  make_tree s0 (v1, n) empty

let rec before n s = if n = min_int then empty else until (n - 1) s

let add_range n1 n2 s =
  if n1 > n2 then invalid_arg (Printf.sprintf "ISet.add_range - %d > %d" n1 n2)else
  let n1, l =
    if n1 = min_int then n1, empty else
    let l = until (n1 - 1) s in
    if is_empty l then n1, empty else
    let (v1, v2), l' = split_rightmost l in
    if v2 + 1 = n1 then v1, l' else n1, l in
  let n2, r =
    if n2 = max_int then n2, empty else
    let r = from (n2 + 1) s in
    if is_empty r then n2, empty else
    let (v1, v2), r' = split_leftmost r in
    if n2 + 1 = v1 then v2, r' else n2, r in
  make_tree l (n1, n2) r

let singleton n = singleton_tree (n, n)

let rec remove n s =
  if is_empty s then empty else
  let (v1, v2) as v = root s in
  let s1 = left_branch s in
  let s2 = right_branch s in
  if n < v1 then make_tree (remove n s1) v s2
  else if n = v1 then
    if v1 = v2 then concat s1 s2 else
    make_tree s1 (v1 + 1, v2) s2
  else if n > v1 && n < v2 then
    let s = make_tree s1 (v1, n - 1) empty in
    make_tree s (n + 1, v2) s2
  else if n = v2 then make_tree s1 (v1, v2 - 1) s2 else
  make_tree s1 v (remove n s2)

let remove_range n1 n2 s =
  if n1 > n2 then invalid_arg "ISet.remove_range" else
  concat (before n1 s) (after n2 s)

let rec union s1 s2 =
  if is_empty s1 then s2 else
  if is_empty s2 then s1 else
  let s1, s2 = if height s1 >! height s2 then s1, s2 else s2, s1 in
  let n1, n2 = root s1 in
  let l1 = left_branch s1 in
  let r1 = right_branch s1 in
  let l2 = before n1 s2 in
  let r2 = after n2 s2 in
  let n1, l =
    if n1 = min_int then n1, empty else
    let l = union l1 l2 in
    if is_empty l then n1, l else
    let (v1, v2), l' = split_rightmost l in (* merge left *)
    if v2 + 1 = n1 then v1, l' else n1, l in
  let n2, r =
    if n1 = max_int then n2, empty else
    let r = union r1 r2 in
    if is_empty r then n2, r else
    let (v1, v2), r' = split_leftmost r in (* merge right *)
    if n2 + 1 = v1 then v2, r' else n2, r in
  make_tree l (n1, n2) r
  
let rec inter s1 s2 =
  if is_empty s1 then empty else
  if is_empty s2 then empty else
  let s1, s2 = if height s1 >! height s2 then s1, s2 else s2, s1 in
  let n1, n2 = root s1 in
  let l1 = left_branch s1 in
  let r1 = right_branch s1 in
  let l2 = before n1 s2 in
  let r2 = after n2 s2 in
  let m = until n2 (from n1 s2) in
  concat (concat (inter l1 l2) m) (inter r1 r2)

let rec compl_aux n1 n2 s =
  if is_empty s then add_range n1 n2 empty else
  let v1, v2 = root s in
  let l = left_branch s in
  let r = right_branch s in
  let l = if v1 = min_int then empty else compl_aux n1 (v1 - 1) l in
  let r = if v2 = max_int then empty else compl_aux (v2 + 1) n2 r in
  concat l r

let compl s = compl_aux min_int max_int s

let diff s1 s2 = inter s1 (compl s2)

let rec compare_aux x1 x2 =
  match x1, x2 with
    [], [] -> 0
  | `Set s :: rest, x ->
      if is_empty s then compare_aux rest x2 else
      let l = left_branch s in
      let v = root s in
      let r = right_branch s in
      compare_aux (`Set l :: `Range v :: `Set r :: rest) x
  | _x, `Set s :: rest ->
      if is_empty s then compare_aux x1 rest else
      let l = left_branch s in
      let v = root s in
      let r = right_branch s in
      compare_aux x1 (`Set l :: `Range v :: `Set r :: rest)
  | `Range ((v1, v2)) :: rest1, `Range ((v3, v4)) :: rest2 ->
      let sgn = compare v1 v3 in
      if sgn <> 0 then sgn else
      let sgn = compare v2 v4 in
      if sgn <> 0 then sgn else
      compare_aux rest1 rest2
  | [], _ -> ~-1
  | _, [] -> 1

let compare s1 s2 = compare_aux [`Set s1] [`Set s2]

let equal s1 s2 = compare s1 s2 = 0

let rec subset s1 s2 =
  if is_empty s1 then true else
  if is_empty s2 then false else
  let v1, v2 = root s2 in
  let l2 = left_branch s2 in
  let r2 = right_branch s2 in
  let l1 = before v1 s1 in
  let r1 = after v2 s1 in
  (subset l1 l2) && (subset r1 r2)

let fold_range f = BatAvlTree.fold (fun (n1, n2) x -> f n1 n2 x)

let fold f =
  let rec g n1 n2 a =
    if n1 = n2 then f n1 a else
    g (n1 + 1) n2 (f n1 a) in
  fold_range g

let iter proc s = fold (fun n () -> proc n) s ()

let iter_range proc = BatAvlTree.iter (fun (n1, n2) -> proc n1 n2)

let for_all p s =
  let rec test_range n1 n2 =
    if n1 = n2 then p n1 else
    p n1 && test_range (n1 + 1) n2 in
  let rec test_set s =
    if is_empty s then true else
    let n1, n2 = root s in
    test_range n1 n2 && 
    test_set (left_branch s) &&
    test_set (right_branch s) in
  test_set s
  
let exists p s =
  let rec test_range n1 n2 =
    if n1 = n2 then p n1 else
    p n1 || test_range (n1 + 1) n2 in
  let rec test_set s =
    if is_empty s then false else
    let n1, n2 = root s in
    test_range n1 n2 || 
    test_set (left_branch s) ||
    test_set (right_branch s) in
  test_set s

let filter_range p n1 n2 a = 
  let rec loop n1 n2 a = function
      None ->
	if n1 = n2 then
	  make_tree a (n1, n1) empty
	else
	  loop (n1 + 1) n2 a (if p n1 then Some n1 else None)
    | Some v1 as x ->
	if n1 = n2 then	make_tree a (v1, n1) empty else
	if p n1 then
	  loop (n1 + 1) n2 a x
	else
	  loop (n1 + 1) n2 (make_tree a (v1, n1 - 1) empty) None in
  loop n1 n2 a None
  
let filter p s = fold_range (filter_range p) empty s

let partition_range p n1 n2 (a, b) = 
  let rec loop n1 n2 acc =
    let acc = 
      let a, b, (v, n) = acc in
      if p n1 = v then acc else
      if v then
	(make_tree a (n, n1) empty, b, (not v, n1))
      else
	(a, make_tree b (n, n1) empty, (not v, n1)) in
    if n1 = n2 then
      let a, b, (v, n) = acc in
      if v then	(make_tree a (n, n1) empty, b) else
      (a, make_tree b (n, n1) empty)
    else
      loop (n1 + 1) n2 acc in
  loop n1 n2 (a, b, (p n1, n1))

let partition p s = fold_range (partition_range p) s (empty, empty)

let cardinal s =
  fold_range (fun n1 n2 c -> c + n2 - n1 + 1) s 0

let rev_ranges s =
  fold_range (fun n1 n2 a -> (n1, n2) :: a) s []

let rec burst_range n1 n2 a =
  if n1 = n2 then n1 :: a else
  burst_range n1 (n2 - 1) (n2 :: a)

let elements s = 
  let f a (n1, n2) = burst_range n1 n2 a in
  List.fold_left f [] (rev_ranges s)

let ranges s = List.rev (rev_ranges s)

let min_elt s =  
  let (n, _), _ = split_leftmost s in 
  n

let max_elt s =  
  let (_, n), _ = split_rightmost s in 
  n

let choose s = fst (root s)
