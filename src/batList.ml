(*
 * BatList - additional and modified functions for lists.
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


(* ::VH:: GLUE with StdLib *)
let merge = List.merge
let fast_sort = List.fast_sort
let stable_sort = List.stable_sort
let sort = List.sort
let assq = List.assq
let assoc = List.assoc
let find = List.find
let exists = List.exists
let for_all = List.for_all
let fold_left = List.fold_left
let rev_map = List.rev_map
let iter = List.iter
let rev_append = List.rev_append
let rev = List.rev
let length = List.length
let tl = List.tl
let hd = List.hd
let mem = List.mem
let memq = List.memq
let mem_assq = List.mem_assq
let mem_assoc = List.mem_assoc
let rev_map2 = List.rev_map2
(* ::VH:: END GLUE *)

(* Thanks to Jacques Garrigue for suggesting the following structure *)
type 'a mut_list =  {
  hd: 'a;
  mutable tl: 'a list
}

type 'a t = 'a list
type 'a enumerable = 'a t
type 'a mappable = 'a t

external inj : 'a mut_list -> 'a list = "%identity"

module Acc = struct
  let dummy () =
    { hd = Obj.magic (); tl = [] }
  let accum acc x =
    let cell = { hd = x; tl = [] } in
    acc.tl <- inj cell;
    cell
end

let cons h t = h::t

let is_empty = function
  | [] -> true
  | _  -> false


(*$T is_empty
  is_empty []
  not (is_empty [1])
*)

let nth l index =
  if index < 0 then invalid_arg "Negative index not allowed";
  let rec loop n = function
    | [] -> invalid_arg "Index past end of list";
    | h :: t ->
      if n = 0 then h else loop (n - 1) t
  in
  loop index l

let at = nth

(*$T at
  try ignore (at [] 0); false with Invalid_argument _ -> true
  try ignore (at [1;2;3] (-1)); false with Invalid_argument _ -> true
  at [1;2;3] 2 = 3
*)

let append l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t ->
    let rec loop dst = function
      | [] ->
        dst.tl <- l2
      | h :: t ->
        loop (Acc.accum dst h) t
    in
    let r = { hd = h; tl = [] } in
    loop r t;
    inj r

let rec flatten l =
  let rec inner dst = function
    | [] -> dst
    | h :: t ->
      inner (Acc.accum dst h) t
  in
  let rec outer dst = function
    | [] -> ()
    | h :: t -> outer (inner dst h) t
  in
  let r = Acc.dummy () in
  outer r l;
  r.tl

let concat = flatten

(*$T flatten
  flatten [[1;2];[3];[];[4;5;6]] = [1;2;3;4;5;6]
  flatten [[]] = []
*)

let singleton x = [x]
(*$Q singleton
  Q.int (fun x -> let s = singleton x in hd s = x && length s = 1)
*)

let map f = function
  | [] -> []
  | h :: t ->
    let rec loop dst = function
      | [] -> ()
      | h :: t ->
        loop (Acc.accum dst (f h)) t
    in
    let r = { hd = f h; tl = [] } in
    loop r t;
    inj r
(*$Q map
  (Q.pair (Q.fun1 Q.int Q.int) (Q.list Q.small_int)) \
  (fun (f,l) -> map f l = List.map f l)
*)

let rec drop n = function
  | _ :: l when n > 0 -> drop (n-1) l
  | l -> l

(*$= drop & ~printer:(IO.to_string (List.print Int.print))
  (drop 0 [1;2;3]) [1;2;3]
  (drop 3 [1;2;3]) []
  (drop 4 [1;2;3]) []
  (drop 1 [1;2;3]) [2;3]
*)

let take n l =
  let rec loop n dst = function
    | h :: t when n > 0 ->
      loop (n - 1) (Acc.accum dst h) t
    | _ ->
      ()
  in
  let dummy = Acc.dummy () in
  loop n dummy l;
  dummy.tl

(*$= take & ~printer:(IO.to_string (List.print Int.print))
  (take 0 [1;2;3]) []
  (take 3 [1;2;3]) [1;2;3]
  (take 4 [1;2;3]) [1;2;3]
  (take 1 [1;2;3]) [1]
*)

let take_while p li =
  let rec loop dst = function
    | [] -> ()
    | x :: xs ->
      if p x then
        loop (Acc.accum dst x) xs in
  let dummy = Acc.dummy () in
  loop dummy li;
  dummy.tl

(*$= take_while & ~printer:(IO.to_string (List.print Int.print))
  (take_while ((=) 3) [3;3;4;3;3]) [3;3]
  (take_while ((=) 3) [3]) [3]
  (take_while ((=) 3) [4]) []
  (take_while ((=) 3) []) []
  (take_while ((=) 2) [2; 2]) [2; 2]
*)

let rec drop_while f = function
  | [] -> []
  | x :: xs when f x -> drop_while f xs
  | xs -> xs

(*$= drop_while & ~printer:(IO.to_string (List.print Int.print))
  (drop_while ((=) 3) [3;3;4;3;3]) [4;3;3]
  (drop_while ((=) 3) [3]) []
*)

let span p li =
  let rec loop dst = function
    | [] -> []
    | x :: xs as l ->
      if p x then
        loop (Acc.accum dst x) xs
      else l
  in
  let dummy = Acc.dummy () in
  let xs = loop dummy li in
  (dummy.tl , xs)

(*$= span
  (span ((=) 3) [3;3;4;3;3])  ([3;3],[4;3;3])
  (span ((=) 3) [3])          ([3],[])
  (span ((=) 3) [4])          ([],[4])
  (span ((=) 3) [])           ([],[])
  (span ((=) 2) [2; 2])       ([2; 2],[])
*)

let nsplit p = function
  | [] -> []
  (* note that returning [] on empty inputs is an arbitrary choice
     that is made for consistence with the behavior of
     BatString.nsplit. Note having this hardcoded case would have
     `nsplit p []` return `[[]]`, which is also a semantically valid
     return value (in fact the two are equivalent, but `[[]]` would be
     a more natural choice as it allows to enforce the simply
     invariant that `nsplit` return values are always non-empty).

     If that was to redo from scratch, `[[]]` would be a better return
     value for both `BatList.nsplit` and `BatString.nsplit`.
  *)
  | li ->
    let not_p x = not (p x) in
    let rec loop dst l =
      let ok, rest = span not_p l in
      let r = Acc.accum dst ok in
      match rest with
        | [] -> ()
        | x :: xs -> loop r xs
    in
    let dummy = Acc.dummy () in
    loop dummy li;
    dummy.tl

(*$T nsplit
  nsplit ((=) 0) []                    = []
  nsplit ((=) 0) [0]                   = [[]; []]
  nsplit ((=) 0) [1; 0]                = [[1]; []]
  nsplit ((=) 0) [0; 1]                = [[]; [1]]
  nsplit ((=) 0) [1; 2; 0; 0; 3; 4; 0; 5] = [[1; 2]; []; [3; 4]; [5]]
*)

let group_consecutive p l =
  let rec loop dst = function
    | [] -> ()
    | x :: rest ->
      let xs, rest = span (p x) rest in
      loop (Acc.accum dst (x :: xs)) rest
  in
  let dummy = Acc.dummy () in
  loop dummy l;
  dummy.tl

(*$= group_consecutive & ~printer:(IO.to_string (List.print (List.print Int.print)))
  (group_consecutive (=) [3; 3; 4; 3; 3]) [[3; 3]; [4]; [3; 3]]
  (group_consecutive (=) [3])             [[3]]
  (group_consecutive (=) [])              []
  (group_consecutive (=) [2; 2])          [[2; 2]]
*)

let takewhile = take_while
let dropwhile = drop_while

let interleave ?first ?last (sep:'a) (l:'a list) =
  let rec aux acc = function
    | []   -> acc
    | h::t -> aux (h::sep::acc) t
  in
  match (l,first, last) with
  | ([],   None,   None)       -> []
  | ([],   None,   Some x)     -> [x]
  | ([],   Some x, None)       -> [x]
  | ([],   Some x, Some y)     -> [x;y]
  | ([h],  None,   None)       -> [h]
  | ([h],  None,   Some x)     -> [h;x]
  | ([h],  Some x, None)       -> [x;h]
  | ([h],  Some x, Some y)     -> [x;h;y]
  | (h::t, None  , None )      -> rev (aux [h] t)
  | (h::t, Some x, None )      -> x::(rev (aux [h] t))
  | (h::t, None,   Some y)     -> rev_append (aux [h] t) [y]
  | (h::t, Some x, Some y)     -> x::rev_append (aux [h] t) [y]

(*$= interleave & ~printer:(IO.to_string (List.print Int.print))
  (interleave 0 [1;2;3]) [1;0;2;0;3]
  (interleave 0 [1]) [1]
  (interleave 0 []) []
  (interleave ~first:(-1) 0 [1;2;3]) [-1;1;0;2;0;3]
  (interleave ~first:(-1) 0 [1]) [-1;1]
  (interleave ~first:(-1) 0 []) [-1]
  (interleave ~last:(-2) 0 [1;2;3]) [1;0;2;0;3;-2]
  (interleave ~last:(-2) 0 [1]) [1;-2]
  (interleave ~last:(-2) 0 []) [-2]
  (interleave ~first:(-1) ~last:(-2) 0 [1;2;3]) [-1;1;0;2;0;3;-2]
  (interleave ~first:(-1) ~last:(-2) 0 [1]) [-1;1;-2]
  (interleave ~first:(-1) ~last:(-2) 0 []) [-1;-2]
*)

let rec unique ?(eq = ( = )) l =
  let rec loop dst = function
    | [] -> ()
    | h :: t ->
      match exists (eq h) t with
      | true -> loop dst t
      | false ->
        loop (Acc.accum dst h) t
  in
  let dummy = Acc.dummy () in
  loop dummy l;
  dummy.tl

(* FIXME BAD TESTS: RESULT IS SPECIFIC TO IMPLEMENTATION *)
(*$= unique & ~printer:(IO.to_string (List.print Int.print))
  [1;2;3;4;5;6] (unique [1;1;2;2;3;3;4;5;6;4;5;6])
  [1] (unique [1;1;1;1;1;1;1;1;1;1])
  [1;2] (unique ~eq:(fun x y -> x land 1 = y land 1) [2;2;2;4;6;8;3;1;2])
*)

let unique_cmp ?(cmp = Pervasives.compare) l =
  let set      = ref (BatMap.PMap.create cmp) in
  let should_keep x =
    if BatMap.PMap.mem x !set then false
    else ( set := BatMap.PMap.add x true !set; true )
  in
  (* use a stateful filter to remove duplicate elements *)
  List.filter should_keep l

(*$= unique_cmp & ~printer:(IO.to_string (List.print Int.print))
  [1;2;3;4;5;6] (unique_cmp [1;1;2;2;3;3;4;5;6;4;5;6])
  [1] (unique_cmp [1;1;1;1;1;1;1;1;1;1])
  [2;3] (unique_cmp ~cmp:(fun x y -> Int.compare (x land 1) (y land 1)) [2;2;2;4;6;8;3;1;2])
*)


let unique_hash (type et) ?(hash = Hashtbl.hash) ?(eq = (=)) (l : et list) =
  let module HT = Hashtbl.Make(struct type t = et let equal = eq let hash = hash end) in
  let ht = HT.create (List.length l) in
  let rec loop dst = function
    | h::t when not (HT.mem ht h) ->
      HT.add ht h (); (* put h in hash table *)
      loop
        (Acc.accum dst h) (* and to output list *)
        t
    | _::t -> (* if already in hashtable then don't add to output list *)
      loop dst t
    | [] -> ()
  in
  let dummy = Acc.dummy () in
  loop dummy l;
  dummy.tl

(*$= unique_hash & ~printer:(IO.to_string (List.print Int.print))
  [1;2;3;4;5;6] (unique_hash [1;1;2;2;3;3;4;5;6;4;5;6])
  [1] (unique_hash [1;1;1;1;1;1;1;1;1;1])
  [2;3] (unique_hash ~hash:(fun x -> Hashtbl.hash (x land 1)) ~eq:(fun x y -> x land 1 = y land 1) [2;2;2;4;6;8;3;1;2])
*)

let filter_map f l =
  let rec loop dst = function
    | [] -> ()
    | h :: t ->
      match f h with
      | None -> loop dst t
      | Some x ->
        loop (Acc.accum dst x) t
  in
  let dummy = Acc.dummy () in
  loop dummy l;
  dummy.tl

let rec find_map f = function
  | [] -> raise Not_found
  | x :: xs ->
    match f x with
    | Some y -> y
    | None -> find_map f xs

let fold_right_max = 1000

let fold_right f l init =
  let rec tail_loop acc = function
    | [] -> acc
    | h :: t -> tail_loop (f h acc) t
  in
  let rec loop n = function
    | [] -> init
    | h :: t ->
      if n < fold_right_max then
        f h (loop (n+1) t)
      else
        f h (tail_loop init (rev t))
  in
  loop 0 l

let map2 f l1 l2 =
  let rec loop dst src1 src2 =
    match src1, src2 with
    | [], [] -> ()
    | h1 :: t1, h2 :: t2 ->
      loop (Acc.accum dst (f h1 h2)) t1 t2
    | _ -> invalid_arg "map2: Different_list_size"
  in
  let dummy = Acc.dummy () in
  loop dummy l1 l2;
  dummy.tl

let rec iter2 f l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | h1 :: t1, h2 :: t2 -> f h1 h2; iter2 f t1 t2
  | _ -> invalid_arg "iter2: Different_list_size"

let rec fold_left2 f accum l1 l2 =
  match l1, l2 with
  | [], [] -> accum
  | h1 :: t1, h2 :: t2 -> fold_left2 f (f accum h1 h2) t1 t2
  | _ -> invalid_arg "fold_left2: Different_list_size"

let fold_right2 f l1 l2 init =
  let rec tail_loop acc l1 l2 =
    match l1, l2 with
    | [] , [] -> acc
    | h1 :: t1 , h2 :: t2 -> tail_loop (f h1 h2 acc) t1 t2
    | _ -> invalid_arg "fold_left2: Different_list_size"
  in
  let rec loop n l1 l2 =
    match l1, l2 with
    | [], [] -> init
    | h1 :: t1, h2 :: t2 ->
      if n < fold_right_max then
        f h1 h2 (loop (n+1) t1 t2)
      else
        f h1 h2 (tail_loop init (rev t1) (rev t2))
    | _ -> invalid_arg "fold_right2: Different_list_size"
  in
  loop 0 l1 l2

let for_all2 p l1 l2 =
  let rec loop l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> if p h1 h2 then loop t1 t2 else false
    | _ -> invalid_arg "for_all2: Different_list_size"
  in
  loop l1 l2

let exists2 p l1 l2 =
  let rec loop l1 l2 =
    match l1, l2 with
    | [], [] -> false
    | h1 :: t1, h2 :: t2 -> if p h1 h2 then true else loop t1 t2
    | _ -> invalid_arg "exists2: Different_list_size"
  in
  loop l1 l2

let remove_assoc x lst =
  let rec loop dst = function
    | [] -> ()
    | (a, _ as pair) :: t ->
      if a = x then
        dst.tl <- t
      else
        loop (Acc.accum dst pair) t
  in
  let dummy = Acc.dummy () in
  loop dummy lst;
  dummy.tl

let remove_assq x lst =
  let rec loop dst = function
    | [] -> ()
    | (a, _ as pair) :: t ->
      if a == x then
        dst.tl <- t
      else
        loop (Acc.accum dst pair) t
  in
  let dummy = Acc.dummy () in
  loop dummy lst;
  dummy.tl

let rfind p l = find p (rev l)

let find_all p l =
  let rec findnext dst = function
    | [] -> ()
    | h :: t ->
      if p h then
        findnext (Acc.accum dst h) t
      else
        findnext dst t
  in
  let dummy = Acc.dummy () in
  findnext dummy l;
  dummy.tl

let rec findi p l =
  let rec loop n = function
    | [] -> raise Not_found
    | h :: t ->
      if p n h then (n,h) else loop (n+1) t
  in
  loop 0 l

let rec index_of e l =
  let rec loop n = function
    | []              -> None
    | h::_ when h = e -> Some n
    | _::t            -> loop ( n + 1 ) t
  in loop 0 l

let rec index_ofq e l =
  let rec loop n = function
    | []               -> None
    | h::_ when h == e -> Some n
    | _::t             -> loop ( n + 1 ) t
  in loop 0 l

let rec rindex_of e l =
  let rec loop n acc = function
    | []              -> acc
    | h::t when h = e -> loop ( n + 1) ( Some n ) t
    | _::t            -> loop ( n + 1 ) acc       t
  in loop 0 None l

let rec rindex_ofq e l =
  let rec loop n acc = function
    | []               -> acc
    | h::t when h == e -> loop ( n + 1) ( Some n ) t
    | _::t             -> loop ( n + 1 ) acc       t
  in loop 0 None l


let filter = find_all

(* berenger: it is not clear to me if I can use Acc.accum in there *)
let partition p lst =
  let rec loop yesdst nodst = function
    | [] -> ()
    | h :: t ->
      let r = { hd = h; tl = [] } in
      if p h then
        begin
          yesdst.tl <- inj r;
          loop r nodst t
        end
      else
        begin
          nodst.tl <- inj r;
          loop yesdst r t
        end
  in
  let yesdummy = Acc.dummy ()
  and nodummy = Acc.dummy ()
  in
  loop yesdummy nodummy lst;
  yesdummy.tl, nodummy.tl

let split lst =
  let rec loop adst bdst = function
    | [] -> ()
    | (a, b) :: t ->
      loop (Acc.accum adst a) (Acc.accum bdst b) t
  in
  let adummy = Acc.dummy ()
  and bdummy = Acc.dummy ()
  in
  loop adummy bdummy lst;
  adummy.tl, bdummy.tl

let combine l1 l2 =
  let list_sizes_differ = Invalid_argument "combine: Different_list_size" in
  match l1, l2 with
    | [], [] -> []
    | x :: xs, y :: ys ->
      let acc = { hd = (x, y); tl = [] } in
      let rec loop dst l1 l2 = match l1, l2 with
        | [], [] -> inj acc
        | h1 :: t1, h2 :: t2 -> loop (Acc.accum dst (h1, h2)) t1 t2
        | _, _ -> raise list_sizes_differ
      in loop acc xs ys
    | _, _ -> raise list_sizes_differ

(*$T combine
  combine []     []     = []
  combine [1]    [2]    = [(1, 2)]
  combine [1; 3] [2; 4] = [(1, 2); (3, 4)]
*)

let rec init size f =
  if size = 0 then []
  else if size < 0 then invalid_arg "BatList.init"
  else
    let rec loop dst n =
      if n < size then
        loop (Acc.accum dst (f n)) (n+1)
    in
    let r = { hd = f 0; tl = [] } in
    loop r 1;
    inj r

let make i x =
  if i < 0 then invalid_arg "List.make";
  let rec loop x acc = function
    | 0 -> acc
    | i -> loop x (x::acc) (i-1)
  in
  loop x [] i

let mapi f = function
  | [] -> []
  | h :: t ->
    let rec loop dst n = function
      | [] -> ()
      | h :: t ->
        loop (Acc.accum dst (f n h)) (n + 1) t
    in
    let r = { hd = f 0 h; tl = [] } in
    loop r 1 t;
    inj r

let iteri f l =
  let rec loop n = function
    | [] -> ()
    | h :: t ->
      f n h;
      loop (n+1) t
  in
  loop 0 l

let first = hd

let rec last = function
  | [] -> invalid_arg "Empty List"
  | h :: [] -> h
  | _ :: t -> last t

let split_nth index = function
  | [] -> if index = 0 then [],[] else invalid_arg "Index past end of list"
  | (h :: t as l) ->
    if index = 0 then [],l
    else if index < 0 then invalid_arg "Negative index not allowed"
    else
      let rec loop n dst l =
        if n = 0 then l else
          match l with
          | [] -> invalid_arg "Index past end of list"
          | h :: t ->
            loop (n - 1) (Acc.accum dst h) t
      in
      let r = { hd = h; tl = [] } in
      inj r, loop (index-1) r t

let split_at = split_nth

let find_exn f e l =
  try
    find f l
  with
    Not_found -> raise e

let remove l x =
  let rec loop dst = function
    | [] -> ()
    | h :: t ->
      if x = h then
        dst.tl <- t
      else
        loop (Acc.accum dst h) t
  in
  let dummy = Acc.dummy () in
  loop dummy l;
  dummy.tl

let rec remove_if f lst =
  let rec loop dst = function
    | [] -> ()
    | x :: l ->
      if f x then
        dst.tl <- l
      else
        loop (Acc.accum dst x) l
  in
  let dummy = Acc.dummy () in
  loop dummy lst;
  dummy.tl

let rec remove_all l x =
  let rec loop dst = function
    | [] -> ()
    | h :: t ->
      if x = h then
        loop dst t
      else
        loop (Acc.accum dst h) t
  in
  let dummy = Acc.dummy () in
  loop dummy l;
  dummy.tl

let transpose = function
  | [] -> []
  | [x] -> List.map (fun x -> [x]) x
  | x::xs ->
    let heads = List.map (fun x -> {hd=x; tl=[]}) x in
    ignore ( List.fold_left
        (fun acc x ->
           List.map2
             (fun x xs -> Acc.accum xs x)
             x acc)
        heads xs);
    Obj.magic heads (* equivalent to List.map inj heads, but without creating a new list *)


(*$T transpose
  transpose [ [1; 2; 3;]; [4; 5; 6;]; [7; 8; 9;] ] = [[1;4;7];[2;5;8];[3;6;9]]
  transpose [] = []
  transpose [ [1] ] = [ [1] ]
*)

let enum l =
  let rec make lr count =
    BatEnum.make
      ~next:(fun () ->
        match !lr with
        | [] -> raise BatEnum.No_more_elements
        | h :: t ->
          decr count;
          lr := t;
          h
      )
      ~count:(fun () ->
        if !count < 0 then count := length !lr;
        !count
      )
      ~clone:(fun () ->
        make (ref !lr) (ref !count)
      )
  in
  make (ref l) (ref (-1))

let of_enum e =
  let h = Acc.dummy () in
  let _ = BatEnum.fold Acc.accum h e in
  h.tl



let backwards l = enum (rev l) (*TODO: should we make it more efficient?*)
(*let backwards l = (*This version only needs one pass but is actually less lazy*)
  let rec aux acc = function
    | []   -> acc
    | h::t -> aux BatEnum.append (BatEnum.singleton h) acc
  in aux l*)


let of_backwards e =
  let rec aux acc = match BatEnum.get e with
    | Some h -> aux (h::acc)
    | None   -> acc
  in aux []

let assoc_inv e l =
  let rec aux = function
    | []                  -> raise Not_found
    | (a,b)::_ when b = e -> a
    | _::t                -> aux t
  in aux l

let assq_inv e l =
  let rec aux = function
    | []                    -> raise Not_found
    | (a,b)::_ when b == e  -> a
    | _::t                  -> aux t
  in aux l

let modify_opt a f l =
  let rec aux p = function
    | [] ->
      (match f None with
       | None   -> raise Exit
       | Some v -> rev ((a,v)::p))
    | (a',b)::t when a' = a ->
      (match f (Some b) with
       | None    -> rev_append p t
       | Some b' -> rev_append ((a,b')::p) t)
    | p'::t ->
      aux (p'::p) t
  in
  try aux [] l with Exit -> l

(*$= modify_opt & ~printer:(IO.to_string (List.print (fun fmt (a,b) -> Printf.fprintf fmt "%d,%d" a b)))
  (* to modify a value *) \
  (modify_opt 5 (function Some 1 -> Some 2 | _ -> assert false) [ 1,0 ; 5,1 ; 8,2 ]) \
    [ 1,0 ; 5,2 ; 8,2 ]
  (* to add a value *) \
  (modify_opt 5 (function None -> Some 2 | _ -> assert false) [ 1,0 ; 8,2 ]) \
    [ 1,0 ; 8,2 ; 5,2 ]
  (* to remove a value *) \
  (modify_opt 5 (function Some 1 -> None | _ -> assert false) [ 1,0 ; 5,1 ; 8,2 ]) \
    [ 1,0 ; 8,2 ]
*)

let modify a f l =
  let f' = function
    | None   -> raise Not_found
    | Some b -> Some (f b)
  in
  modify_opt a f' l

(*$= modify & ~printer:(IO.to_string (List.print (fun fmt (a,b) -> Printf.fprintf fmt "%d,%d" a b)))
  (modify 5 succ [ 1,0 ; 5,1 ; 8,2 ]) [ 1,0 ; 5,2 ; 8,2 ]
*)
(*$T modify
  try ignore (modify 5 succ [ 1,0 ; 8,2 ]); false with Not_found -> true
*)

let modify_def dfl a f l =
  let f' = function
    | None   -> Some (f dfl)
    | Some b -> Some (f b)
  in
  modify_opt a f' l

(*$= modify_def & ~printer:(IO.to_string (List.print (fun fmt (a,b) -> Printf.fprintf fmt "%d,%d" a b)))
  (modify_def 0 5 succ [ 1,0 ; 5,1 ; 8,2 ]) [ 1,0 ; 5,2 ; 8,2 ]
  (modify_def 0 5 succ [ 1,0 ; 8,2 ]) [ 1,0 ; 8,2 ; 5,1 ]
*)

let sort_unique cmp lst =
  let sorted = List.sort cmp lst in
  let fold first rest = List.fold_left
      (fun (acc, last) elem ->
        if (cmp last elem) = 0 then (acc, elem)
        else (elem::acc, elem)
      )
      ([first], first)
      rest
  in
  match sorted with
  | [] -> []
  | hd::tl ->
    begin
      let rev_result, _ = fold hd tl in
      List.rev rev_result
    end

let group cmp lst =
  let sorted = List.sort cmp lst in
  let fold first rest = List.fold_left
      (fun (acc, agr, last) elem ->
        if (cmp last elem) = 0 then (acc, elem::agr, elem)
        else (agr::acc, [elem], elem)
      )
      ([], [first], first)
      rest
  in
  match sorted with
  | [] -> []
  | hd::tl ->
    begin
      let groups, lastgr, _ = fold hd tl in
      List.rev_map List.rev (lastgr::groups)
    end

(*$T group
  group Pervasives.compare []                 = []
  group Pervasives.compare [1]                = [[1]]
  group Pervasives.compare [2; 2]             = [[2; 2]]
  group Pervasives.compare [5; 4; 4; 2; 1; 6] = [[1]; [2]; [4; 4]; [5]; [6]]
*)

let cartesian_product l1 l2 =
  List.concat (List.map (fun i -> List.map (fun j -> (i,j)) l2) l1)

(*$T cartesian_product as cp
  cp [1;2;3] ['x';'y'] = [1,'x';1,'y';2,'x';2,'y';3,'x';3,'y']
*)

let rec n_cartesian_product = function
  | [] -> [[]]
  | h :: t ->
    let rest = n_cartesian_product t in
    List.concat (List.map (fun i -> List.map (fun r -> i :: r) rest) h)

(*$T n_cartesian_product as ncp
  ncp []               = [[]]
  ncp [[]]             = []
  ncp [[1]; [2]; [3]]  = [[1;2;3]]
  ncp [[1;2;3]]        = [[1]; [2]; [3]]
  ncp [[1;2;3]; []]    = []
  ncp [[1;2;3]; [4;5]] = [[1;4]; [1;5]; [2;4]; [2;5]; [3;4]; [3;5]]
*)

let print ?(first="[") ?(last="]") ?(sep="; ") print_a  out = function
  | []   ->
    BatInnerIO.nwrite out first;
    BatInnerIO.nwrite out last
  | [h]  ->
    BatInnerIO.nwrite out first;
    print_a out h;
    BatInnerIO.nwrite out last
  | h::t ->
    BatInnerIO.nwrite out first;
    print_a out h;
    iter (fun x -> BatInnerIO.nwrite out sep; print_a out x) t;
    BatInnerIO.nwrite out last

let t_printer a_printer _paren out x = print (a_printer false) out x

let reduce f = function [] -> invalid_arg "Empty List"
                      | h::t -> fold_left f h t

let min l = reduce Pervasives.min l
let max l = reduce Pervasives.max l
let sum l = reduce (+) l
let fsum l = reduce (+.) l

let min_max ?cmp:(cmp = Pervasives.compare) = function
  | [] -> invalid_arg "List.min_max: Empty List"
  | x :: xs ->
    fold_left
      (fun (curr_min, curr_max) y ->
         let new_min =
           if cmp curr_min y = 1
           then y
           else curr_min
         in
         let new_max =
           if cmp curr_max y = -1
           then y
           else curr_max
         in
         (new_min, new_max)
      )
      (x, x)
      xs

(*$T min_max
  min_max [1] = (1, 1)
  min_max [1; 1] = (1, 1)
  min_max [1; -2; 3; 4; 5; 60; 7; 8] = (-2, 60)
*)

let unfold b f =
  let acc = Acc.dummy () in
  let rec loop dst v =
    match f v with
    | None -> acc.tl
    | Some (a, v) -> loop (Acc.accum dst a) v
  in loop acc b

(*$T unfold
  unfold 1 (fun x -> None) = []
  unfold 0 (fun x -> if x > 3 then None else Some (x, succ x)) = [0;1;2;3]
*)

module Exceptionless = struct
  let rfind p l =
    try  Some (rfind p l)
    with Not_found -> None

  let find p l =
    try Some (find p l)
    with Not_found -> None

  let findi p l =
    try  Some (findi p l)
    with Not_found -> None

  let split_at n l =
    try   `Ok (split_at n l)
    with Invalid_argument s -> `Invalid_argument s

  let at n l =
    try `Ok (at n l)
    with Invalid_argument s -> `Invalid_argument s

  let assoc e l =
    try Some (assoc e l)
    with Not_found -> None

  let assq e l =
    try Some (assq e l)
    with Not_found -> None

  let assoc_inv e l =
    try Some (assoc_inv e l)
    with Not_found -> None

  let find_map f l =
    try Some(find_map f l)
    with Not_found -> None

  let hd l =
    try Some (hd l)
    with Failure "hd" -> None

  let tl l =
    try Some (tl l)
    with Failure "tl" -> None

  let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: l -> last l
end



module Labels = struct

  type 'a t         = 'a list
  let init i ~f     = init i f
  let make n  x     = make n x
  let iteri ~f l    = iteri f l
  let map ~f l      = map f l
  let mapi ~f l     = mapi f l
  let rfind ~f l    = rfind f l
  let find ~f l     = find f l
  let findi ~f      = findi f
  let find_exn ~f   = find_exn f
  let filter_map ~f = filter_map f
  let remove_if ~f  = remove_if f
  let take_while ~f = take_while f
  let drop_while ~f = drop_while f
  let map2 ~f       = map2 f
  let iter2 ~f      = iter2 f
  let exists2 ~f    = exists2 f
  let fold_left ~f ~init         = fold_left f init
  let fold_right ~f l ~init      = fold_right f l init
  let fold_left2  ~f ~init       = fold_left2 f init
  let fold_right2 ~f l1 l2 ~init = fold_right2 f l1 l2 init
  let filter ~f     = filter f
  let find_all ~f   = find_all f
  let partition ~f  = partition f
  let rev_map ~f    = rev_map f
  let rev_map2 ~f   = rev_map2 f
  let iter ~f       = iter f
  let for_all ~f    = for_all f
  let for_all2 ~f   = for_all2 f
  let exists ~f     = exists f
  let stable_sort ?(cmp=compare)  = stable_sort cmp
  let fast_sort ?(cmp=compare)    = fast_sort cmp
  let sort ?(cmp=compare)         = sort cmp
  let merge ?(cmp=compare)        = merge cmp

  module LExceptionless = struct
    include Exceptionless
    let rfind ~f l = rfind f l
    let find ~f l = find f l
    let findi ~f l = findi f l
  end
end

let ( @ ) = List.append

module Infix = struct
  let ( @ ) = ( @ )
end

open BatOrd

let rec eq eq_elt l1 l2 =
  match l1 with
  | [] -> (match l2 with [] -> true | _ -> false)
  | hd1::tl1 ->
    (match l2 with
     | [] -> false
     | hd2::tl2 -> bin_eq eq_elt hd1 hd2 (eq eq_elt) tl1 tl2)

let rec ord ord_elt l1 l2 =
  match l1 with
  | [] -> (match l2 with [] -> Eq | _::_ -> Lt)
  | hd1::tl1 ->
    (match l2 with
     | [] -> Gt
     | hd2::tl2 -> bin_ord ord_elt hd1 hd2 (ord ord_elt) tl1 tl2)

let rec compare comp_elt l1 l2 =
  match l1 with
  | [] -> (match l2 with [] -> 0 | _::_ -> -1)
  | hd1::tl1 ->
    (match l2 with
     | [] -> 1
     | hd2::tl2 -> bin_comp comp_elt hd1 hd2 (compare comp_elt) tl1 tl2)

module Eq (T : Eq) = struct
  type t = T.t list
  let eq = eq T.eq
end

module Ord (T : Ord) = struct
  type t = T.t list
  let ord = ord T.ord
end

module Comp (T : Comp) = struct
  type t = T.t list
  let compare = compare T.compare
end
