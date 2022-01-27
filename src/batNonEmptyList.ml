(*
 * BatNonEmptyList - a type-guaranteed never empty list
 * Copyright (C) 2022 Francois Berenger
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 Red Hat Inc.
 * Copyright (C) 2008 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

module L = BatList

type 'a t = { head: 'a;
              tail: 'a list }

let hd l =
  l.head

let tl l =
  l.tail

let create head tail =
  { head; tail }

let cons = create

exception Empty_list

let of_list_exn = function
  | [] -> raise Empty_list
  | head :: tail -> { head; tail }

(* don't expose outside;
   only call when you are _sure_ the list parameter is not empty *)
let of_list = function
  | [] -> assert(false)
  | head :: tail -> { head; tail }

let to_list l =
  l.head :: l.tail

let compare_lengths la lb =
  L.compare_lengths la.tail lb.tail

(*$T compare_lengths
    compare_lengths (of_list [1; 2]) (of_list [3; 4]) = 0
    compare_lengths (of_list [1; 2; 3]) (of_list [3; 4]) = 1
    compare_lengths (of_list [1; 2]) (of_list [2; 3; 4]) = -1
*)

let compare_length_with l n =
  L.compare_length_with (to_list l) n

(*$T compare_length_with
    compare_length_with (of_list [1]) 0 = 1
    compare_length_with (of_list [1; 2]) 2 = 0
    compare_length_with (of_list [1; 2; 3]) 2 = 1
    compare_length_with (of_list [1; 2]) 3 = -1
*)

let nth l index =
  L.nth (to_list l) index

let at = nth

(*$T at
  try ignore (at (of_list [1;2;3]) (-1)); false with Invalid_argument _ -> true
  at (of_list [1;2;3]) 2 = 3
*)

let at_opt l index =
  L.at_opt (to_list l) index
 
let mem_cmp cmp x l =
  L.mem_cmp cmp x (to_list l)

(*$T mem_cmp
  mem_cmp Pervasives.compare 0 (of_list [1; 2]) = false
  mem_cmp Pervasives.compare 1 (of_list [1; 2]) = true
  mem_cmp Pervasives.compare 2 (of_list [1; 2]) = true
*)

let append l1 l2 =
  of_list (L.append (to_list l1) (to_list l2))

(*$T append
  append (of_list [1]) (of_list [2]) = of_list [1; 2]
  append (of_list [1; 2]) (of_list [3]) = of_list [1; 2; 3]
  append (of_list [1]) (of_list [2; 3]) = of_list [1; 2; 3]
*)

let flatten l =
  of_list (L.flatten (to_list l))

let concat = flatten

(*$T flatten
    flatten (of_list [[1;2];[3];[4;5;6]]) = of_list [1;2;3;4;5;6]
*)

let singleton x =
  create x []

let map f l =
  create (f l.head) (L.map f l.tail)

(* we don't support L.drop; because it can create an empty list *)
(* we don't support L.{take, takedrop, take_while, ntake, drop_while,
                       span, fold_while, nsplit} *)

(* let group_consecutive p l =
 *   let rec loop dst = function
 *     | [] -> ()
 *     | x :: rest ->
 *       let xs, rest = span (p x) rest in
 *       loop (Acc.accum dst (x :: xs)) rest
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy l;
 *   dummy.tl
 * 
 * (*$= group_consecutive & ~printer:(IO.to_string (List.print (List.print Int.print)))
 *   (group_consecutive (=) [3; 3; 4; 3; 3]) [[3; 3]; [4]; [3; 3]]
 *   (group_consecutive (=) [3])             [[3]]
 *   (group_consecutive (=) [])              []
 *   (group_consecutive (=) [2; 2])          [[2; 2]]
 * *)
 * 
 * ##V>=4.5##let nth_opt = List.nth_opt
 * ##V<4.5##let nth_opt li n = try Some (nth li n) with _ -> None
 * let takewhile = take_while
 * let dropwhile = drop_while
 * 
 * let interleave ?first ?last (sep:'a) (l:'a list) =
 *   let may_prepend maybe_x lst = match maybe_x with
 *     | None -> lst
 *     | Some x -> x :: lst
 *   in
 *   let rec loop acc = function
 *     | [] -> acc
 *     | x :: xs ->
 *       match acc with
 *       | [] -> loop [x] xs
 *       | _ -> loop (x :: sep :: acc) xs
 *   in
 *   let res = loop [] l in
 *   may_prepend first (rev (may_prepend last res))
 * 
 * (*$= interleave & ~printer:(IO.to_string (List.print Int.print))
 *   (interleave 0 [1;2;3]) [1;0;2;0;3]
 *   (interleave 0 [1]) [1]
 *   (interleave 0 []) []
 *   (interleave ~first:(-1) 0 [1;2;3]) [-1;1;0;2;0;3]
 *   (interleave ~first:(-1) 0 [1]) [-1;1]
 *   (interleave ~first:(-1) 0 []) [-1]
 *   (interleave ~last:(-2) 0 [1;2;3]) [1;0;2;0;3;-2]
 *   (interleave ~last:(-2) 0 [1]) [1;-2]
 *   (interleave ~last:(-2) 0 []) [-2]
 *   (interleave ~first:(-1) ~last:(-2) 0 [1;2;3]) [-1;1;0;2;0;3;-2]
 *   (interleave ~first:(-1) ~last:(-2) 0 [1]) [-1;1;-2]
 *   (interleave ~first:(-1) ~last:(-2) 0 []) [-1;-2]
 * *)
 * 
 * let unique ?(eq = ( = )) l =
 *   let rec loop dst = function
 *     | [] -> ()
 *     | h :: t ->
 *       match exists (eq h) t with
 *       | true -> loop dst t
 *       | false ->
 *         loop (Acc.accum dst h) t
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy l;
 *   dummy.tl
 * 
 * (* FIXME BAD TESTS: RESULT IS SPECIFIC TO IMPLEMENTATION *)
 * (*$= unique & ~printer:(IO.to_string (List.print Int.print))
 *   [1;2;3;4;5;6] (unique [1;1;2;2;3;3;4;5;6;4;5;6])
 *   [1] (unique [1;1;1;1;1;1;1;1;1;1])
 *   [1;2] (unique ~eq:(fun x y -> x land 1 = y land 1) [2;2;2;4;6;8;3;1;2])
 * *)
 * 
 * let unique_cmp ?(cmp = Pervasives.compare) l =
 *   let set = ref (BatSet.PSet.create cmp) in
 *   let should_keep x =
 *     if BatSet.PSet.mem x !set then false
 *     else ( set := BatSet.PSet.add x !set; true )
 *   in
 *   (* use a stateful filter to remove duplicate elements *)
 *   List.filter should_keep l
 * 
 * (*$= unique_cmp & ~printer:(IO.to_string (List.print Int.print))
 *   [1;2;3;4;5;6] (unique_cmp [1;1;2;2;3;3;4;5;6;4;5;6])
 *   [1] (unique_cmp [1;1;1;1;1;1;1;1;1;1])
 *   [2;3] (unique_cmp ~cmp:(fun x y -> Int.compare (x land 1) (y land 1)) [2;2;2;4;6;8;3;1;2])
 * *)
 * 
 * 
 * let unique_hash (type et) ?(hash = Hashtbl.hash) ?(eq = (=)) (l : et list) =
 *   let module HT = Hashtbl.Make(struct type t = et let equal = eq let hash = hash end) in
 *   let ht = HT.create (List.length l) in
 *   let rec loop dst = function
 *     | h::t when not (HT.mem ht h) ->
 *       HT.add ht h (); (* put h in hash table *)
 *       loop
 *         (Acc.accum dst h) (* and to output list *)
 *         t
 *     | _::t -> (* if already in hashtable then don't add to output list *)
 *       loop dst t
 *     | [] -> ()
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy l;
 *   dummy.tl
 * 
 * (*$= unique_hash & ~printer:(IO.to_string (List.print Int.print))
 *   [1;2;3;4;5;6] (unique_hash [1;1;2;2;3;3;4;5;6;4;5;6])
 *   [1] (unique_hash [1;1;1;1;1;1;1;1;1;1])
 *   [2;3] (unique_hash ~hash:(fun x -> Hashtbl.hash (x land 1)) ~eq:(fun x y -> x land 1 = y land 1) [2;2;2;4;6;8;3;1;2])
 * *)
 * 
 * let filter_map f l =
 *   let rec loop dst = function
 *     | [] -> ()
 *     | h :: t ->
 *       match f h with
 *       | None -> loop dst t
 *       | Some x ->
 *         loop (Acc.accum dst x) t
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy l;
 *   dummy.tl
 * 
 * let filteri_map f l =
 *   let rec loop i dst = function
 *     | [] -> ()
 *     | h :: t ->
 *       match f i h with
 *       | None -> loop (succ i) dst t
 *       | Some x ->
 *         loop (succ i) (Acc.accum dst x) t
 *   in
 *   let dummy = Acc.dummy () in
 *   loop 0 dummy l;
 *   dummy.tl
 * (*$T filteri_map
 *   (let r = ref (-1) in filteri_map (fun i _ -> incr r; if i = !r then Some i else None) [5; 4; 8] = [0; 1; 2])
 *   filteri_map (fun _ x -> if x > 4 then Some (x, string_of_int x) else None) [5; 4; 8] = [(5, "5"); (8, "8")]
 *   filteri_map (fun _ _ -> Some ()) [] = []
 *   filteri_map (fun _ _ -> None) [1; 2] = []
 * *)
 * 
 * let rec find_map f = function
 *   | [] -> raise Not_found
 *   | x :: xs ->
 *     match f x with
 *     | Some y -> y
 *     | None -> find_map f xs
 * 
 * let fold_right_max = 1000
 * 
 * let fold_right f l init =
 *   let rec tail_loop acc = function
 *     | [] -> acc
 *     | h :: t -> tail_loop (f h acc) t
 *   in
 *   let rec loop n = function
 *     | [] -> init
 *     | h :: t ->
 *       if n < fold_right_max then
 *         f h (loop (n+1) t)
 *       else
 *         f h (tail_loop init (rev t))
 *   in
 *   loop 0 l
 * 
 * let map2 f l1 l2 =
 *   let rec loop dst src1 src2 =
 *     match src1, src2 with
 *     | [], [] -> ()
 *     | h1 :: t1, h2 :: t2 ->
 *       loop (Acc.accum dst (f h1 h2)) t1 t2
 *     | _ -> invalid_arg "List.map2: list lengths differ"
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy l1 l2;
 *   dummy.tl
 * 
 * let map2i f l1 l2 =
 *   let rec loop i dst src1 src2 =
 *     match src1, src2 with
 *     | [], [] -> ()
 *     | h1 :: t1, h2 :: t2 ->
 *       loop (succ i) (Acc.accum dst (f i h1 h2)) t1 t2
 *     | _ -> invalid_arg "List.map2i: list lengths differ"
 *   in
 *   let dummy = Acc.dummy () in
 *   loop 0 dummy l1 l2;
 *   dummy.tl
 * 
 * (*$T map2i
 *   map2i (fun i x y -> i, x, y) [] [] = []
 *   map2i (fun i x y -> i, x, y) ['a'] ["b"] = [0, 'a', "b"]
 *   map2i (fun i x y -> i, x, y) ['a'; 'b'; 'c'] ["d"; "e"; "f"] = \
 *     [(0, 'a', "d"); (1, 'b', "e"); (2, 'c', "f")]
 *   try ignore (map2i (fun i x y -> i, x, y) [] [0]); false \
 *     with Invalid_argument _ -> true
 *   try ignore (map2i (fun i x y -> i, x, y) [1; 2; 3] ["4"]); false \
 *     with Invalid_argument _ -> true
 * *)
 * 
 * let rec iter2 f l1 l2 =
 *   match l1, l2 with
 *   | [], [] -> ()
 *   | h1 :: t1, h2 :: t2 -> f h1 h2; iter2 f t1 t2
 *   | _ -> invalid_arg "List.iter2: list lengths differ"
 * 
 * let iter2i f l1 l2 =
 *   let rec loop i l1 l2 =
 *     match l1, l2 with
 *     | [], [] -> ()
 *     | h1 :: t1, h2 :: t2 -> f i h1 h2; loop (succ i) t1 t2
 *     | _ -> invalid_arg "List.iter2i: list lengths differ"
 *   in loop 0 l1 l2
 * 
 * (*$T iter2i
 *   try iter2i (fun _ _ _ -> ()) [1] [1;2;3]; false \
 *     with Invalid_argument _ -> true
 *   try iter2i (fun _ _ _ -> ()) [1] []; false \
 *     with Invalid_argument _ -> true
 * *)
 * 
 * (*$T iter2i
 *   iter2i (fun _ _ _ -> assert false) [] []; true
 *   let r = ref 0 in iter2i (fun i x y -> r := !r + i * x + y) [1] [2]; !r = 2
 *   let r = ref 0 in iter2i (fun i x y -> r := !r + i * x + y) [1; 2] [3; 4]; !r = 9
 * *)
 * 
 * let rec fold_left2 f accum l1 l2 =
 *   match l1, l2 with
 *   | [], [] -> accum
 *   | h1 :: t1, h2 :: t2 -> fold_left2 f (f accum h1 h2) t1 t2
 *   | _ -> invalid_arg "List.fold_left2: list lengths differ"
 * 
 * let fold_right2 f l1 l2 init =
 *   let rec tail_loop acc l1 l2 =
 *     match l1, l2 with
 *     | [] , [] -> acc
 *     | h1 :: t1 , h2 :: t2 -> tail_loop (f h1 h2 acc) t1 t2
 *     | _ -> invalid_arg "List.fold_right2: list lengths differ"
 *   in
 *   let rec loop n l1 l2 =
 *     match l1, l2 with
 *     | [], [] -> init
 *     | h1 :: t1, h2 :: t2 ->
 *       if n < fold_right_max then
 *         f h1 h2 (loop (n+1) t1 t2)
 *       else
 *         f h1 h2 (tail_loop init (rev t1) (rev t2))
 *     | _ -> invalid_arg "List.fold_right2: list lengths differ"
 *   in
 *   loop 0 l1 l2
 * 
 * let for_all2 p l1 l2 =
 *   let rec loop l1 l2 =
 *     match l1, l2 with
 *     | [], [] -> true
 *     | h1 :: t1, h2 :: t2 -> if p h1 h2 then loop t1 t2 else false
 *     | _ -> invalid_arg "List.for_all2: list lengths differ"
 *   in
 *   loop l1 l2
 * 
 * let exists2 p l1 l2 =
 *   let rec loop l1 l2 =
 *     match l1, l2 with
 *     | [], [] -> false
 *     | h1 :: t1, h2 :: t2 -> if p h1 h2 then true else loop t1 t2
 *     | _ -> invalid_arg "List.exists2: list lengths differ"
 *   in
 *   loop l1 l2
 * 
 * let remove_assoc x lst =
 *   let rec loop dst = function
 *     | [] -> ()
 *     | (a, _ as pair) :: t ->
 *       if a = x then
 *         dst.tl <- t
 *       else
 *         loop (Acc.accum dst pair) t
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy lst;
 *   dummy.tl
 * 
 * let remove_assq x lst =
 *   let rec loop dst = function
 *     | [] -> ()
 *     | (a, _ as pair) :: t ->
 *       if a == x then
 *         dst.tl <- t
 *       else
 *         loop (Acc.accum dst pair) t
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy lst;
 *   dummy.tl
 * 
 * let remove_at i lst =
 *   let rec loop dst i = function
 *     | [] -> invalid_arg "List.remove_at"
 *     | x :: xs ->
 *       if i = 0 then
 *         dst.tl <- xs
 *       else
 *         loop (Acc.accum dst x) (i - 1) xs
 *   in
 *   if i < 0 then
 *     invalid_arg "List.remove_at"
 *   else
 *     let dummy = Acc.dummy () in
 *     loop dummy i lst;
 *     dummy.tl
 * 
 * (*$T remove_at
 *   try ignore (remove_at 0 []) ; false with Invalid_argument _ -> true
 *   try ignore (remove_at 1 [0]); false with Invalid_argument _ -> true
 *   remove_at 0 [0]       = []
 *   remove_at 0 [0; 1; 2] = [1; 2]
 *   remove_at 1 [0; 1; 2] = [0; 2]
 *   remove_at 2 [0; 1; 2] = [0; 1]
 * *)
 * 
 * let rfind p l = find p (rev l)
 * 
 * let find_all p l =
 *   let rec findnext dst = function
 *     | [] -> ()
 *     | h :: t ->
 *       if p h then
 *         findnext (Acc.accum dst h) t
 *       else
 *         findnext dst t
 *   in
 *   let dummy = Acc.dummy () in
 *   findnext dummy l;
 *   dummy.tl
 * 
 * let findi p l =
 *   let rec loop n = function
 *     | [] -> raise Not_found
 *     | h :: t ->
 *       if p n h then (n,h) else loop (n+1) t
 *   in
 *   loop 0 l
 * 
 * let index_of e l =
 *   let rec loop n = function
 *     | []              -> None
 *     | h::_ when h = e -> Some n
 *     | _::t            -> loop ( n + 1 ) t
 *   in loop 0 l
 * 
 * let index_ofq e l =
 *   let rec loop n = function
 *     | []               -> None
 *     | h::_ when h == e -> Some n
 *     | _::t             -> loop ( n + 1 ) t
 *   in loop 0 l
 * 
 * let rindex_of e l =
 *   let rec loop n acc = function
 *     | []              -> acc
 *     | h::t when h = e -> loop ( n + 1) ( Some n ) t
 *     | _::t            -> loop ( n + 1 ) acc       t
 *   in loop 0 None l
 * 
 * let rindex_ofq e l =
 *   let rec loop n acc = function
 *     | []               -> acc
 *     | h::t when h == e -> loop ( n + 1) ( Some n ) t
 *     | _::t             -> loop ( n + 1 ) acc       t
 *   in loop 0 None l
 * 
 * let filter = find_all
 * 
 * let count_matching p l =
 *   fold_left (fun count x ->
 *       if p x then count + 1
 *       else count
 *     ) 0 l
 * 
 * (*$T count_matching
 *   count_matching (fun _ -> true) [] = 0
 *   count_matching (fun _ -> true) [1] = 1
 *   count_matching (fun _ -> true) [1;2] = 2
 *   count_matching (fun x -> x mod 2 = 1) [1;2;3;4;5;6] = 3
 * *)
 * 
 * ##V>=4.11##let filteri = List.filteri
 * ##V<4.11##let filteri f =
 * ##V<4.11##  let rec aux i = function
 * ##V<4.11##    | [] -> []
 * ##V<4.11##    | x::xs when f i x -> x :: aux (succ i) xs
 * ##V<4.11##    | _x::xs -> aux (succ i) xs
 * ##V<4.11##  in
 * ##V<4.11##  aux 0
 * (*$T filteri
 *   (let r = ref (-1) in filteri (fun i _ -> incr r; i = !r) [5; 4; 8] = [5; 4; 8])
 *   filteri (fun _ x -> x > 4) [5; 4; 8] = [5; 8]
 *   filteri (fun _ _ -> true) [] = []
 * *)
 * 
 * let partition p lst =
 *   let rec loop yesdst nodst = function
 *     | [] -> ()
 *     | h :: t ->
 *       if p h then
 *         loop (Acc.accum yesdst h) nodst t
 *       else
 *         loop yesdst (Acc.accum nodst h) t
 *   in
 *   let yesdummy = Acc.dummy ()
 *   and nodummy = Acc.dummy ()
 *   in
 *   loop yesdummy nodummy lst;
 *   (yesdummy.tl, nodummy.tl)
 * 
 * let partition_map p lst =
 *   let rec loop left right = function
 *     | [] -> ()
 *     | x :: xs ->
 *       match p x with
 *       | BatEither.Left v -> loop (Acc.accum left v) right xs
 *       | BatEither.Right v -> loop left (Acc.accum right v) xs in
 *   let left_acc = Acc.dummy ()
 *   and right_acc = Acc.dummy () in
 *   loop left_acc right_acc lst;
 *   (left_acc.tl, right_acc.tl)
 * 
 * (*$T partition_map
 *   let odd_or_even x = \
 *     if x mod 2 = 1 then BatEither.Left x else BatEither.Right x in \
 *   partition_map odd_or_even [1;2;3;4;5;6] = ([1;3;5], [2;4;6])
 * *)
 * 
 * let split lst =
 *   let rec loop adst bdst = function
 *     | [] -> ()
 *     | (a, b) :: t ->
 *       loop (Acc.accum adst a) (Acc.accum bdst b) t
 *   in
 *   let adummy = Acc.dummy ()
 *   and bdummy = Acc.dummy ()
 *   in
 *   loop adummy bdummy lst;
 *   adummy.tl, bdummy.tl
 * 
 * let combine l1 l2 =
 *   match l1, l2 with
 *     | [], [] -> []
 *     | x :: xs, y :: ys ->
 *       let acc = Acc.create (x, y) in
 *       let rec loop dst l1 l2 = match l1, l2 with
 *         | [], [] -> inj acc
 *         | h1 :: t1, h2 :: t2 -> loop (Acc.accum dst (h1, h2)) t1 t2
 *         | _, _ -> invalid_arg "List.combine: list lengths differ"
 *       in loop acc xs ys
 *     | _, _ -> invalid_arg "List.combine: list lengths differ"
 * 
 * (*$T combine
 *   combine []     []     = []
 *   combine [1]    [2]    = [(1, 2)]
 *   combine [1; 3] [2; 4] = [(1, 2); (3, 4)]
 * *)
 * 
 * let init size f =
 *   if size = 0 then []
 *   else if size < 0 then invalid_arg "BatList.init"
 *   else
 *     let rec loop dst n =
 *       if n < size then
 *         loop (Acc.accum dst (f n)) (n+1)
 *     in
 *     let r = Acc.create (f 0) in
 *     loop r 1;
 *     inj r
 * 
 * let unfold_exn f =
 *   let rec loop dst =
 *     loop (Acc.accum dst (f ()))
 *   in
 *   let acc = Acc.dummy () in
 *   try
 *     loop acc
 *   with exn -> (acc.tl, exn)
 * 
 * (*$T unfold_exn
 *   let exc () = raise End_of_file in \
 *   unfold_exn exc = ([], End_of_file)
 *   let state = ref 0 in \
 *   let just_zero () = \
 *     if !state = 1 then raise End_of_file \
 *     else let _ = incr state in 0 \
 *   in \
 *   unfold_exn just_zero = ([0], End_of_file)
 * *)
 * 
 * let unfold_exc = unfold_exn
 * 
 * let make i x =
 *   if i < 0 then invalid_arg "List.make";
 *   let rec loop x acc = function
 *     | 0 -> acc
 *     | i -> loop x (x::acc) (i-1)
 *   in
 *   loop x [] i
 * 
 * let range i dir j =
 *   let op = match dir with
 *     | `To ->
 *       if i > j
 *       then invalid_arg (Printf.sprintf "List.range %d `To %d" i j)
 *       else pred
 *     | `Downto ->
 *       if i < j
 *       then invalid_arg (Printf.sprintf "List.range %d `Downto %d" i j)
 *       else succ
 *   in
 *   let rec loop acc k =
 *     if i = k then
 *       k :: acc
 *     else
 *       loop (k :: acc) (op k)
 *   in
 *   loop [] j
 * 
 * (*$T range
 *   range 1 `To 3     = [1; 2; 3]
 *   range 1 `To 1     = [1]
 *   range 3 `Downto 1 = [3; 2; 1]
 *   range 3 `Downto 3 = [3]
 *   try ignore(range 1 `To 0); true with Invalid_argument _ -> true
 *   try ignore(range 1 `Downto 2); true with Invalid_argument _ -> true
 * *)
 * 
 * let frange start direction stop n =
 *   if n < 2 then invalid_arg (Printf.sprintf "List.frange: %d < 2" n);
 *   let nb_steps = float_of_int (n - 1) in
 *   match direction with
 *   | `To ->
 *     begin
 *       if start >= stop then
 *         invalid_arg (Printf.sprintf "List.frange %f `To %f" start stop);
 *       let span = stop -. start in
 *       let rec loop acc i =
 *         let x = ((span *. float_of_int (i - 1)) /. nb_steps) +. start in
 *         let acc' = x :: acc in
 *         if i = 1 then acc'
 *         else loop acc' (i - 1)
 *       in
 *       loop [] n
 *     end
 *   | `Downto ->
 *     begin
 *       if start <= stop then
 *         invalid_arg (Printf.sprintf "List.frange %f `Downto %f" start stop);
 *       let span = start -. stop in
 *       let rec loop acc i =
 *         let x = ((span *. float_of_int (i - 1)) /. nb_steps) +. stop in
 *         let acc' = x :: acc in
 *         if i = n then acc'
 *         else loop acc' (i + 1)
 *       in
 *       loop [] 1
 *     end
 * 
 * (*$T frange
 *   try ignore(frange 1. `To 2. 1); true with Invalid_argument _ -> true
 *   try ignore(frange 2. `Downto 1. 1); true with Invalid_argument _ -> true
 *   try ignore(frange 3. `To 1. 3); true with Invalid_argument _ -> true
 *   try ignore(frange 1. `Downto 3. 3); true with Invalid_argument _ -> true
 *   frange 1. `To 3. 3 = [1.; 2.; 3.]
 *   frange 1. `To 2. 2 = [1.; 2.]
 *   frange 3. `Downto 1. 3 = [3.; 2.; 1.]
 *   frange 2. `Downto 1. 2 = [2.; 1.]
 *   length (frange 0.123 `To 3.491 1000) = 1000
 * *)
 * 
 * let mapi f = function
 *   | [] -> []
 *   | h :: t ->
 *     let rec loop dst n = function
 *       | [] -> ()
 *       | h :: t ->
 *         loop (Acc.accum dst (f n h)) (n + 1) t
 *     in
 *     let r = Acc.create (f 0 h) in
 *     loop r 1 t;
 *     inj r
 * 
 * let iteri f l =
 *   let rec loop n = function
 *     | [] -> ()
 *     | h :: t ->
 *       f n h;
 *       loop (n+1) t
 *   in
 *   loop 0 l
 * 
 * let fold_lefti f init l =
 *   let rec loop i acc = function
 *     | [] -> acc
 *     | x :: xs -> loop (i + 1) (f acc i x) xs
 *   in
 *   loop 0 init l
 * 
 * (*$T fold_lefti
 *   fold_lefti (fun acc i x -> (i, x) :: acc) [] []       = []
 *   fold_lefti (fun acc i x -> (i, x) :: acc) [] [0.]     = [(0, 0.)]
 *   fold_lefti (fun acc i x -> (i, x) :: acc) [] [0.; 1.] = [(1, 1.); (0, 0.)]
 * *)
 * 
 * let fold_righti f l init =
 *   let xis =
 *     (* reverse the list and index its elements *)
 *     fold_lefti (fun acc i x -> (i, x) :: acc) [] l
 *   in
 *   fold_left
 *     (fun acc (i, x) -> f i x acc)
 *     init
 *     xis
 * 
 * (*$T fold_righti
 *   fold_righti (fun i x acc -> (i, x) :: acc) []       [] = []
 *   fold_righti (fun i x acc -> (i, x) :: acc) [0.]     [] = [(0, 0.)]
 *   fold_righti (fun i x acc -> (i, x) :: acc) [0.; 1.] [] = [(0, 0.); (1, 1.)]
 * *)
 * 
 * ##V>=4.11##let fold_left_map = List.fold_left_map
 * ##V<4.11##let fold_left_map f acc = function
 * ##V<4.11##  | [] -> acc, []
 * ##V<4.11##  | h :: t ->
 * ##V<4.11##    let rec loop acc dst = function
 * ##V<4.11##      | [] -> acc
 * ##V<4.11##      | h :: t ->
 * ##V<4.11##        let acc', t' = f acc h in
 * ##V<4.11##        loop acc' (Acc.accum dst t') t
 * ##V<4.11##    in
 * ##V<4.11##    let acc', h' = f acc h in
 * ##V<4.11##    let r = Acc.create h' in
 * ##V<4.11##    let res = loop acc' r t in
 * ##V<4.11##    res, inj r
 * 
 * (*$T fold_left_map
 *   fold_left_map (fun acc x -> assert false) 0 [] = (0, [])
 *   fold_left_map (fun acc x -> acc ^ x, int_of_string x) "0" ["1"; "2"; "3"] = ("0123", [1; 2; 3])
 * *)
 * 
 * let first = hd
 * 
 * let rec last = function
 *   | [] -> invalid_arg "Empty List"
 *   | h :: [] -> h
 *   | _ :: t -> last t
 * 
 * let split_nth index = function
 *   | [] -> if index = 0 then [],[] else invalid_arg at_after_end_msg
 *   | (h :: t as l) ->
 *     if index = 0 then [],l
 *     else if index < 0 then invalid_arg at_negative_index_msg
 *     else
 *       let rec loop n dst l =
 *         if n = 0 then l else
 *           match l with
 *           | [] -> invalid_arg at_after_end_msg
 *           | h :: t ->
 *             loop (n - 1) (Acc.accum dst h) t
 *       in
 *       let r = Acc.create h in
 *       inj r, loop (index-1) r t
 * 
 * let split_at = split_nth
 * 
 * let find_exn f e l =
 *   try
 *     find f l
 *   with
 *     Not_found -> raise e
 * 
 * let remove l x =
 *   let rec loop dst = function
 *     | [] -> ()
 *     | h :: t ->
 *       if x = h then
 *         dst.tl <- t
 *       else
 *         loop (Acc.accum dst h) t
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy l;
 *   dummy.tl
 * 
 * let remove_if f lst =
 *   let rec loop dst = function
 *     | [] -> ()
 *     | x :: l ->
 *       if f x then
 *         dst.tl <- l
 *       else
 *         loop (Acc.accum dst x) l
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy lst;
 *   dummy.tl
 * 
 * let remove_all l x =
 *   let rec loop dst = function
 *     | [] -> ()
 *     | h :: t ->
 *       if x = h then
 *         loop dst t
 *       else
 *         loop (Acc.accum dst h) t
 *   in
 *   let dummy = Acc.dummy () in
 *   loop dummy l;
 *   dummy.tl
 * 
 * let transpose = function
 *   | [] -> []
 *   | [x] -> List.map (fun x -> [x]) x
 *   | x::xs ->
 *     let heads = List.map Acc.create x in
 *     ignore ( fold_left
 *         (fun acc x ->
 *            map2
 *              (fun x xs -> Acc.accum xs x)
 *              x acc)
 *         heads xs);
 *     Obj.magic heads (* equivalent to List.map inj heads, but without creating a new list *)
 * 
 * 
 * (*$T transpose
 *   transpose [ [1; 2; 3;]; [4; 5; 6;]; [7; 8; 9;] ] = [[1;4;7];[2;5;8];[3;6;9]]
 *   transpose [] = []
 *   transpose [ [1] ] = [ [1] ]
 * *)
 * 
 * let enum l =
 *   let rec make lr count =
 *     BatEnum.make
 *       ~next:(fun () ->
 *         match !lr with
 *         | [] -> raise BatEnum.No_more_elements
 *         | h :: t ->
 *           decr count;
 *           lr := t;
 *           h
 *       )
 *       ~count:(fun () ->
 *         if !count < 0 then count := length !lr;
 *         !count
 *       )
 *       ~clone:(fun () ->
 *         make (ref !lr) (ref !count)
 *       )
 *   in
 *   make (ref l) (ref (-1))
 * 
 * let of_enum e =
 *   let h = Acc.dummy () in
 *   let _ = BatEnum.fold Acc.accum h e in
 *   h.tl
 * 
 * 
 * 
 * let backwards l = enum (rev l) (*TODO: should we make it more efficient?*)
 * (*let backwards l = (*This version only needs one pass but is actually less lazy*)
 *   let rec aux acc = function
 *     | []   -> acc
 *     | h::t -> aux BatEnum.append (BatEnum.singleton h) acc
 *   in aux l*)
 * 
 * 
 * let of_backwards e =
 *   let rec aux acc = match BatEnum.get e with
 *     | Some h -> aux (h::acc)
 *     | None   -> acc
 *   in aux []
 * 
 * let assoc_inv e l =
 *   let rec aux = function
 *     | []                  -> raise Not_found
 *     | (a,b)::_ when b = e -> a
 *     | _::t                -> aux t
 *   in aux l
 * 
 * let assq_inv e l =
 *   let rec aux = function
 *     | []                    -> raise Not_found
 *     | (a,b)::_ when b == e  -> a
 *     | _::t                  -> aux t
 *   in aux l
 * 
 * let modify_opt a f l =
 *   let rec aux p = function
 *     | [] ->
 *       (match f None with
 *        | None   -> raise Exit
 *        | Some v -> rev ((a,v)::p))
 *     | (a',b)::t when a' = a ->
 *       (match f (Some b) with
 *        | None    -> rev_append p t
 *        | Some b' -> rev_append ((a,b')::p) t)
 *     | p'::t ->
 *       aux (p'::p) t
 *   in
 *   try aux [] l with Exit -> l
 * 
 * (*$= modify_opt & ~printer:(IO.to_string (List.print (fun fmt (a,b) -> Printf.fprintf fmt "%d,%d" a b)))
 *   (* to modify a value *) \
 *   (modify_opt 5 (function Some 1 -> Some 2 | _ -> assert false) [ 1,0 ; 5,1 ; 8,2 ]) \
 *     [ 1,0 ; 5,2 ; 8,2 ]
 *   (* to add a value *) \
 *   (modify_opt 5 (function None -> Some 2 | _ -> assert false) [ 1,0 ; 8,2 ]) \
 *     [ 1,0 ; 8,2 ; 5,2 ]
 *   (* to remove a value *) \
 *   (modify_opt 5 (function Some 1 -> None | _ -> assert false) [ 1,0 ; 5,1 ; 8,2 ]) \
 *     [ 1,0 ; 8,2 ]
 * *)
 * 
 * let modify a f l =
 *   let f' = function
 *     | None   -> raise Not_found
 *     | Some b -> Some (f b)
 *   in
 *   modify_opt a f' l
 * 
 * (*$= modify & ~printer:(IO.to_string (List.print (fun fmt (a,b) -> Printf.fprintf fmt "%d,%d" a b)))
 *   (modify 5 succ [ 1,0 ; 5,1 ; 8,2 ]) [ 1,0 ; 5,2 ; 8,2 ]
 * *)
 * (*$T modify
 *   try ignore (modify 5 succ [ 1,0 ; 8,2 ]); false with Not_found -> true
 * *)
 * 
 * let modify_def dfl a f l =
 *   let f' = function
 *     | None   -> Some (f dfl)
 *     | Some b -> Some (f b)
 *   in
 *   modify_opt a f' l
 * 
 * (*$= modify_def & ~printer:(IO.to_string (List.print (fun fmt (a,b) -> Printf.fprintf fmt "%d,%d" a b)))
 *   (modify_def 0 5 succ [ 1,0 ; 5,1 ; 8,2 ]) [ 1,0 ; 5,2 ; 8,2 ]
 *   (modify_def 0 5 succ [ 1,0 ; 8,2 ]) [ 1,0 ; 8,2 ; 5,1 ]
 * *)
 * 
 * let modify_opt_at n f l =
 *   if n < 0 then invalid_arg at_negative_index_msg;
 *   let rec loop acc n = function
 *     | [] -> invalid_arg at_after_end_msg
 *     | h :: t ->
 *       if n <> 0 then loop (h :: acc) (n - 1) t
 *       else match f h with
 *         | None -> rev_append acc t
 *         | Some v -> rev_append acc (v :: t)
 *   in
 *   loop [] n l
 * 
 * (*$T modify_opt_at
 *   modify_opt_at 2 (fun n -> Some (n*n)) [1;2;3;4;5] = [1;2;9;4;5]
 *   modify_opt_at 2 (fun _ -> None) [1;2;3;4;5] = [1;2;4;5]
 *   try ignore (modify_opt_at 0 (fun _ -> None) []); false \
 *   with Invalid_argument _ -> true
 *   try ignore (modify_opt_at 2 (fun _ -> None) []); false \
 *   with Invalid_argument _ -> true
 *   try ignore (modify_opt_at (-1) (fun _ -> None) [1;2;3]); false \
 *   with Invalid_argument _ -> true
 *   try ignore (modify_opt_at 5 (fun _ -> None) [1;2;3]); false \
 *   with Invalid_argument _ -> true
 *   try ignore (modify_opt_at 3 (fun _ -> None) [1;2;3]); false \
 *   with Invalid_argument _ -> true
 * *)
 * 
 * let modify_at n f l =
 *   modify_opt_at n (fun x -> Some (f x)) l
 * 
 * (*$T modify_at
 *   modify_at 2 ((+) 1) [1;2;3;4] = [1;2;4;4]
 *   try ignore (modify_at 0 ((+) 1) []); false \
 *   with Invalid_argument _ -> true
 *   try ignore (modify_at 2 ((+) 1) []); false \
 *   with Invalid_argument _ -> true
 *   try ignore (modify_at (-1) ((+) 1) [1;2;3]); false \
 *   with Invalid_argument _ -> true
 *   try ignore (modify_at 5 ((+) 1) [1;2;3]); false \
 *   with Invalid_argument _ -> true
 *   try ignore (modify_at 3 ((+) 1) [1;2;3]); false \
 *   with Invalid_argument _ -> true
 * *)
 * 
 * let sort_unique cmp lst =
 *   let sorted = List.sort cmp lst in
 *   let fold first rest = List.fold_left
 *       (fun (acc, last) elem ->
 *         if (cmp last elem) = 0 then (acc, elem)
 *         else (elem::acc, elem)
 *       )
 *       ([first], first)
 *       rest
 *   in
 *   match sorted with
 *   | [] -> []
 *   | hd::tl ->
 *     begin
 *       let rev_result, _ = fold hd tl in
 *       List.rev rev_result
 *     end
 * 
 * ##V<4.2##let sort_uniq = sort_unique
 * ##V>=4.2##let sort_uniq = List.sort_uniq
 * 
 * let group cmp lst =
 *   let sorted = List.sort cmp lst in
 *   let fold first rest = List.fold_left
 *       (fun (acc, agr, last) elem ->
 *         if (cmp last elem) = 0 then (acc, elem::agr, elem)
 *         else (agr::acc, [elem], elem)
 *       )
 *       ([], [first], first)
 *       rest
 *   in
 *   match sorted with
 *   | [] -> []
 *   | hd::tl ->
 *     begin
 *       let groups, lastgr, _ = fold hd tl in
 *       List.rev_map List.rev (lastgr::groups)
 *     end
 * 
 * (*$T group
 *   group Pervasives.compare []                 = []
 *   group Pervasives.compare [1]                = [[1]]
 *   group Pervasives.compare [2; 2]             = [[2; 2]]
 *   group Pervasives.compare [5; 4; 4; 2; 1; 6] = [[1]; [2]; [4; 4]; [5]; [6]]
 * *)
 * 
 * let cartesian_product l1 l2 =
 *   List.concat (List.map (fun i -> List.map (fun j -> (i,j)) l2) l1)
 * 
 * (*$T cartesian_product as cp
 *   cp [1;2;3] ['x';'y'] = [1,'x';1,'y';2,'x';2,'y';3,'x';3,'y']
 * *)
 * 
 * let rec n_cartesian_product = function
 *   | [] -> [[]]
 *   | h :: t ->
 *     let rest = n_cartesian_product t in
 *     List.concat (List.map (fun i -> List.map (fun r -> i :: r) rest) h)
 * 
 * (*$T n_cartesian_product as ncp
 *   ncp []               = [[]]
 *   ncp [[]]             = []
 *   ncp [[1]; [2]; [3]]  = [[1;2;3]]
 *   ncp [[1;2;3]]        = [[1]; [2]; [3]]
 *   ncp [[1;2;3]; []]    = []
 *   ncp [[1;2;3]; [4;5]] = [[1;4]; [1;5]; [2;4]; [2;5]; [3;4]; [3;5]]
 * *)
 * 
 * let print ?(first="[") ?(last="]") ?(sep="; ") print_a  out = function
 *   | []   ->
 *     BatInnerIO.nwrite out first;
 *     BatInnerIO.nwrite out last
 *   | [h]  ->
 *     BatInnerIO.nwrite out first;
 *     print_a out h;
 *     BatInnerIO.nwrite out last
 *   | h::t ->
 *     BatInnerIO.nwrite out first;
 *     print_a out h;
 *     iter (fun x -> BatInnerIO.nwrite out sep; print_a out x) t;
 *     BatInnerIO.nwrite out last
 * 
 * let t_printer a_printer _paren out x = print (a_printer false) out x
 * 
 * let reduce f = function
 *   | [] -> invalid_arg "List.reduce: Empty List"
 *   | h :: t -> fold_left f h t
 * 
 * let min ?cmp:(cmp = Pervasives.compare) l =
 *   let min = BatOrd.min_comp cmp in
 *   reduce min l
 * 
 * let max ?cmp:(cmp = Pervasives.compare) l =
 *   let max = BatOrd.max_comp cmp in
 *   reduce max l
 * 
 * let sum l = fold_left (+) 0 l
 * (*$= sum & ~printer:string_of_int
 *   2 (sum [1;1])
 *   0 (sum [])
 * *)
 * 
 * let fsum l =
 *   match l with
 *   | [] -> 0.
 *   | x::xs ->
 *     let acc = ref x in
 *     let rem = ref xs in
 *     let go = ref true in
 *     while !go do
 *       match !rem with
 *       | [] -> go := false;
 *       | x::xs ->
 *         acc := !acc +. x;
 *         rem := xs
 *     done;
 *     !acc
 * (*$= fsum & ~printer:string_of_float
 *   0. (fsum [])
 *   6. (fsum [1.;2.;3.])
 * *)
 * 
 * let favg l =
 *   match l with
 *   | [] -> invalid_arg "List.favg: Empty List"
 *   | x::xs ->
 *     let acc = ref x in
 *     let len = ref 1 in
 *     let rem = ref xs in
 *     let go = ref true in
 *     while !go do
 *       match !rem with
 *       | [] -> go := false;
 *       | x::xs ->
 *         acc := !acc +. x;
 *         incr len;
 *         rem := xs
 *     done;
 *     !acc /. float_of_int !len
 * (*$T favg
 *   try let _ = favg [] in false with Invalid_argument _ -> true
 *   favg [1.;2.;3.] = 2.
 * *)
 * 
 * let kahan_sum li =
 *   (* This algorithm is written in a particularly untasteful imperative
 *      style to benefit from the nice unboxing of float references that
 *      is harder to obtain with recursive functions today. See the
 *      definition of kahan sum on arrays, on which this one is directly
 *      modeled. *)
 *   let li = ref li in
 *   let continue = ref (!li <> []) in
 *   let sum = ref 0. in
 *   let err = ref 0. in
 *   while !continue do
 *     match !li with
 *       | [] -> continue := false
 *       | x::xs ->
 *         li := xs;
 *         let x = x -. !err in
 *         let new_sum = !sum +. x in
 *         err := (new_sum -. !sum) -. x;
 *         sum := new_sum +. 0.;
 *   done;
 *   !sum +. 0.
 * 
 * (*$T kahan_sum
 *    kahan_sum [ ] = 0.
 *    kahan_sum [ 1.; 2. ] = 3.
 *    let n, x = 1_000, 1.1 in \
 *      Float.approx_equal (float n *. x) \
 *                         (kahan_sum (List.make n x))
 * *)
 * 
 * let min_max ?cmp:(cmp = Pervasives.compare) = function
 *   | [] -> invalid_arg "List.min_max: Empty List"
 *   | x :: xs ->
 *     fold_left
 *       (fun (curr_min, curr_max) y ->
 *          let new_min =
 *            if cmp curr_min y = 1
 *            then y
 *            else curr_min
 *          in
 *          let new_max =
 *            if cmp curr_max y = -1
 *            then y
 *            else curr_max
 *          in
 *          (new_min, new_max)
 *       )
 *       (x, x)
 *       xs
 * 
 * (*$T min_max
 *   min_max [1] = (1, 1)
 *   min_max [1; 1] = (1, 1)
 *   min_max [1; -2; 3; 4; 5; 60; 7; 8] = (-2, 60)
 * *)
 * 
 * let unfold b f =
 *   let acc = Acc.dummy () in
 *   let rec loop dst v =
 *     match f v with
 *     | None -> acc.tl
 *     | Some (a, v) -> loop (Acc.accum dst a) v
 *   in loop acc b
 * 
 * (*$T unfold
 *   unfold 1 (fun x -> None) = []
 *   unfold 0 (fun x -> if x > 3 then None else Some (x, succ x)) = [0;1;2;3]
 * *)
 * 
 * let subset cmp l l' = for_all (fun x -> mem_cmp cmp x l') l
 * 
 * (*$T subset
 *   subset Pervasives.compare [1;2;3;4] [1;2;3] = false
 *   subset Pervasives.compare [1;2;3] [1;2;3] = true
 *   subset Pervasives.compare [3;2;1] [1;2;3] = true
 *   subset Pervasives.compare [1;2] [1;2;3] = true
 * *)
 * 
 * let shuffle ?state l =
 *   let arr = Array.of_list l in
 *   BatInnerShuffle.array_shuffle ?state arr;
 *   Array.to_list arr
 * (*$T shuffle
 *   let s = Random.State.make [|11|] in \
 *   shuffle ~state:s [1;2;3;4;5;6;7;8;9] = [7; 2; 9; 5; 3; 6; 4; 1; 8]
 *   shuffle [] = []
 * *)
 * 
 * module Exceptionless = struct
 *   let rfind p l =
 *     try  Some (rfind p l)
 *     with Not_found -> None
 * 
 *   let find p l =
 *     try Some (find p l)
 *     with Not_found -> None
 * 
 *   let findi p l =
 *     try  Some (findi p l)
 *     with Not_found -> None
 * 
 *   let split_at n l =
 *     try   `Ok (split_at n l)
 *     with Invalid_argument s -> `Invalid_argument s
 * 
 *   let at n l =
 *     try `Ok (at n l)
 *     with Invalid_argument s -> `Invalid_argument s
 * 
 *   let assoc e l =
 *     try Some (assoc e l)
 *     with Not_found -> None
 * 
 *   let assq e l =
 *     try Some (assq e l)
 *     with Not_found -> None
 * 
 *   let assoc_inv e l =
 *     try Some (assoc_inv e l)
 *     with Not_found -> None
 * 
 *   let find_map f l =
 *     try Some(find_map f l)
 *     with Not_found -> None
 * 
 *   let hd l =
 *     try Some (hd l)
 *     with Failure _ -> None
 * 
 *   let tl l =
 *     try Some (tl l)
 *     with Failure _ -> None
 * 
 *   let rec last = function
 *     | [] -> None
 *     | [x] -> Some x
 *     | _ :: l -> last l
 * 
 *   let reduce f = function
 *     | [] -> None
 *     | h :: t -> Some (fold_left f h t)
 * 
 *   let min_max ?cmp:(cmp = Pervasives.compare) l =
 *     try Some (min_max ~cmp l)
 *     with Invalid_argument _ -> None
 * 
 *   let min ?cmp:(cmp = Pervasives.compare) l =
 *     try Some (min ~cmp l)
 *     with Invalid_argument _ -> None
 * 
 *   let max ?cmp:(cmp = Pervasives.compare) l =
 *     try Some (max ~cmp l)
 *     with Invalid_argument _ -> None
 * 
 * end
 * 
 * module Labels = struct
 *   let init i ~f     = init i f
 *   let make n  x     = make n x
 *   let iteri ~f l    = iteri f l
 *   let map ~f l      = map f l
 *   let mapi ~f l     = mapi f l
 *   let rfind ~f l    = rfind f l
 *   let find ~f l     = find f l
 *   let findi ~f      = findi f
 *   let find_exn ~f   = find_exn f
 * ##V>=4.10##let find_map_opt ~f = find_map_opt f
 *   let filter_map ~f = filter_map f
 *   let remove_if ~f  = remove_if f
 *   let take_while ~f = take_while f
 *   let drop_while ~f = drop_while f
 *   let map2 ~f       = map2 f
 *   let iter2 ~f      = iter2 f
 *   let exists2 ~f    = exists2 f
 *   let fold_left ~f ~init         = fold_left f init
 *   let fold = fold_left
 *   let fold_right ~f l ~init      = fold_right f l init
 *   let fold_left2  ~f ~init       = fold_left2 f init
 *   let fold_right2 ~f l1 l2 ~init = fold_right2 f l1 l2 init
 *   let filter ~f     = filter f
 *   let count_matching ~f = count_matching f
 * ##V>=4.10##let concat_map ~f = List.concat_map f
 *   let find_all ~f   = find_all f
 *   let partition ~f  = partition f
 *   let partition_map ~f = partition_map f
 *   let rev_map ~f    = rev_map f
 *   let rev_map2 ~f   = rev_map2 f
 *   let iter ~f       = iter f
 *   let for_all ~f    = for_all f
 *   let for_all2 ~f   = for_all2 f
 *   let exists ~f     = exists f
 *   let subset ~cmp = subset cmp
 *   let stable_sort ?(cmp=compare)  = stable_sort cmp
 *   let fast_sort ?(cmp=compare)    = fast_sort cmp
 *   let sort ?(cmp=compare)         = sort cmp
 *   let merge ?(cmp=compare)        = merge cmp
 * 
 *   module LExceptionless = struct
 *     include Exceptionless
 *     let rfind ~f l = rfind f l
 *     let find ~f l = find f l
 *     let findi ~f l = findi f l
 *   end
 * end
 * 
 * let ( @ ) = List.append
 * 
 * module Infix = struct
 *   let ( @ ) = ( @ )
 * end
 * 
 * open BatOrd
 * 
 * let rec eq eq_elt l1 l2 =
 *   match l1 with
 *   | [] -> (match l2 with [] -> true | _ -> false)
 *   | hd1::tl1 ->
 *     (match l2 with
 *      | [] -> false
 *      | hd2::tl2 -> bin_eq eq_elt hd1 hd2 (eq eq_elt) tl1 tl2)
 * 
 * let rec ord ord_elt l1 l2 =
 *   match l1 with
 *   | [] -> (match l2 with [] -> Eq | _::_ -> Lt)
 *   | hd1::tl1 ->
 *     (match l2 with
 *      | [] -> Gt
 *      | hd2::tl2 -> bin_ord ord_elt hd1 hd2 (ord ord_elt) tl1 tl2)
 * 
 * let rec compare comp_elt l1 l2 =
 *   match l1 with
 *   | [] -> (match l2 with [] -> 0 | _::_ -> -1)
 *   | hd1::tl1 ->
 *     (match l2 with
 *      | [] -> 1
 *      | hd2::tl2 -> bin_comp comp_elt hd1 hd2 (compare comp_elt) tl1 tl2)
 * 
 * module Eq (T : Eq) = struct
 *   type t = T.t list
 *   let eq = eq T.eq
 * end
 * 
 * module Ord (T : Ord) = struct
 *   type t = T.t list
 *   let ord = ord T.ord
 * end
 * 
 * module Comp (T : Comp) = struct
 *   type t = T.t list
 *   let compare = compare T.compare
 * end *)
