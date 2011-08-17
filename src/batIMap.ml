(* $Id: iMap.ml,v 1.2 2006/08/13 17:13:01 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

type 'a t = (int * int * 'a) BatAvlTree.tree

type 'a map = 'a t

type key = int

include BatAvlTree

let singleton n v = singleton_tree (n, n, v)

let make eq l (n1, n2, v) r =
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
    make eq l (n, n2, v) r
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
    make eq l (n, n, v) r
  else if n2 <> max_int && n = n2 + 1 && eq v v0 then
    make eq l (n1, n, v) r
  else
    make_tree l x (add n v r)

(**T imap_add
   let a = add ~eq:(=) in empty |> a 0 0 |> a 2 0 |> a 1 0 |> enum |> List.of_enum = [(0,2,0)]
   let a = add ~eq:(=) in empty |> a 0 "foo" |> a 2 "foo" |> a 1 "foo" |> enum |> List.of_enum = [(0,2,"foo")]
 **)

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

let add_range ?(eq=(==)) n1 n2 v s =
  if n1 > n2 then invalid_arg "IMap.add_range" else
  make eq (before n1 s) (n1, n2, v) (after n2 s)

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
  BatAvlTree.iter (fun (n1, n2, v) -> proc n1 n2 v) m

let fold_range f m a =
  BatAvlTree.fold (fun (n1, n2, v) a -> f n1 n2 v a) m a

let fold f m a =
  let rec loop n1 n2 v a =
    let a = f n1 v a in
    if n1 = n2 then a else
    loop (n1 + 1) n2 v a in
  fold_range loop m a

let iter proc m =
  fold (fun n v () -> proc n v) m ()

let rec map ?(eq=(==)) f m =
  if is_empty m then empty else
  let n1, n2, v = root m in
  let l = map f (left_branch m) in
  let r = map f (right_branch m) in
  let v = f v in
  make eq l (n1, n2, v) r

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

module Enum = BatEnum

(* Fold across two maps *)
let fold2_range f m1 m2 acc = 
  let e1 = enum m1 and e2 = enum m2 in
  let rec aux acc = function 
      None,None -> acc
    | Some (lo,hi,rx), None -> 
	aux (f lo hi (Some rx) None acc) (Enum.get e1, None)
    | None, Some (lo,hi,rx) -> 
	aux (f lo hi None (Some rx) acc) (None, Enum.get e2)
    | Some (lo1,hi1,rx1), Some (lo2,hi2,rx2) when lo1 < lo2 ->
	let hi, v1 = 
	  if hi1 > lo2 then lo2-1, Some (lo2,hi1,rx1)
	  else if hi1 = lo2 then hi1, Some (lo2,lo2,rx1)
	  else hi1, Enum.get e1
	and v2 = Some (lo2,hi2,rx2) in
	aux (f lo1 hi (Some rx1) None acc) (v1, v2)
    | Some (lo1,hi1,rx1), Some (lo2,hi2,rx2) when lo2 < lo1 ->
	let hi, v2 = 
	  if hi2 > lo1 then lo1-1, Some (lo1,hi2,rx2)
	  else if hi2 = lo1 then hi2, Some (lo1,lo1,rx2)
	  else hi2, Enum.get e2	
	and v1 = Some (lo1,hi1,rx1) in
	aux (f lo2 hi None (Some rx2) acc) (v1,v2)
    | Some (lo1,hi1,rx1), Some (_lo2,hi2,rx2) (* lo1 = lo2 *) ->
	let hi, v1, v2 = 
	  if hi1 = hi2 then hi1, Enum.get e1, Enum.get e2
	  else if hi1 < hi2 then hi1, Enum.get e1, Some (hi1+1,hi2,rx2)
	  else (* hi2 < hi1 *) hi2, Some (hi2+1,hi1,rx1), Enum.get e2
	in
(*	printf "#@%a\n" print_rng (lo1, hi); *)
	aux (f lo1 hi (Some rx1) (Some rx2) acc) (v1, v2)
  in
  aux acc (Enum.get e1, Enum.get e2)

let union f m1 m2 = 
  let insert lo hi v1 v2 m = 
    match f v1 v2 with None -> m | Some v -> add_range lo hi v m in
  fold2_range insert m1 m2 empty

let forall2_range f m1 m2 =
  let e1 = enum m1 and e2 = enum m2 in
  let rec aux = function 
      None,None -> true
    | Some (lo,hi,rx), None -> 
	(f lo hi (Some rx) None) && aux (Enum.get e1, None)
    | None, Some (lo,hi,rx) -> 
	(f lo hi None (Some rx)) && aux (None, Enum.get e2)
    | Some (lo1,hi1,rx1), Some (lo2,hi2,rx2) when lo1 < lo2 ->
	let hi, v1 = 
	  if hi1 > lo2 then lo2-1, Some (lo2,hi1,rx1)
	  else hi1, Enum.get e1
	and v2 = Some (lo2,hi2,rx2) in
	(f lo1 hi (Some rx1) None) && aux (v1, v2)
    | Some (lo1,hi1,rx1), Some (lo2,hi2,rx2) when lo2 < lo1 ->
	let hi, v2 = 
	  if hi2 > lo1 then lo1-1, Some (lo1,hi2,rx2)
	  else hi2, Enum.get e2	
	and v1 = Some (lo1,hi1,rx1) in
	(f lo2 hi None (Some rx2)) && aux (v1,v2)
    | Some (lo1,hi1,rx1), Some (_,hi2,rx2) (* lo1 = lo2 *) ->
	let hi, v1, v2 = 
	  if hi1 = hi2 then hi1, Enum.get e1, Enum.get e2
	  else if hi1 < hi2 then hi1, Enum.get e1, Some (hi1+1,hi2,rx2)
	  else (* hi2 < hi1 *) hi2, Some (hi2+1,hi1,rx1), Enum.get e2
	in
	(f lo1 hi (Some rx1) (Some rx2)) && aux (v1, v2)
  in
  aux (Enum.get e1, Enum.get e2)

module Infix =
struct
  let (-->) map key = find key map
  let (<--) map (key, value) = add key value map
end

let make ?(eq=(==)) l c r = make eq l c r
