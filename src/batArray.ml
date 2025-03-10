(*
 * BatArray - additional and modified functions for arrays.
 * Copyright (C) 2005 Richard W.M. Jones (rich @ annexia.org)
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


include Array

##V>=5##module Pervasives = Stdlib

(*$inject
##V>=5##module Pervasives = Stdlib
*)

type 'a t = 'a array
type 'a enumerable = 'a t
type 'a mappable = 'a t

##V<4.2##let create_float n = make n 0.
##V<4.2##let make_float = create_float

##V=4.2##external make_float: int -> float array = "caml_make_float_vect"
##V=4.2##external create_float: int -> float array = "caml_make_float_vect"

##V>=5.3##external create_float: int -> float array = "caml_array_create_float"
##V>=4.3####V<5.3##external create_float: int -> float array = "caml_make_float_vect"
##V>=4.3##let make_float = create_float

let singleton x = [|x|]
(*$Q singleton
  Q.int (fun x -> let s = singleton x in s.(0) = x && length s = 1)
*)

let modify f a =
  for i = 0 to length a - 1 do
    unsafe_set a i (f (unsafe_get a i))
  done

let modifyi f a =
  for i = 0 to length a - 1 do
    unsafe_set a i (f i (unsafe_get a i))
  done

(*$T modify
  let a = [|3;2;1|] in modify (fun x -> x + 1) a; a = [|4;3;2|]
*)(*$T modifyi
    let a = [|3;2;1|] in modifyi (fun i x -> i * x) a; a = [|0;2;2|]
  *)

let fold = fold_left

let fold_left_map f init a =
  let n = length a in
  if n = 0 then
    (init, [||])
  else
    let acc = ref init in
    let f' x =
      let acc', y = f !acc x in
      acc := acc';
      y in
    let res = map f' a in
    (!acc, res)

(*$T fold_left_map
    fold_left_map (fun acc x -> (acc + x, x)) 0 [|0;1;2;3|] = (6, [|0;1;2;3|])
    fold_left_map (fun acc x -> (acc + x, x)) 0 [||] = (0, [||])
  *)

let fold_lefti f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r i (unsafe_get a i)
  done;
  !r

(*$T fold_lefti
   fold_lefti (fun a i x -> a + i * x) 1 [|2;4;5|] = 1 + 0 + 4 + 10
   fold_lefti (fun a i x -> a + i * x) 1 [||] = 1
*)

let fold_righti f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f i (unsafe_get a i) !r
  done;
  !r

(*$T fold_righti
   fold_righti (fun i x a -> a + i * x) [|2;4;5|] 1 = 1 + 0 + 4 + 10
   fold_righti (fun i x a -> a + i * x) [||] 1 = 1
*)

let rev_in_place xs =
  let n = length xs in
  let j = ref (n-1) in
  for i = 0 to n/2-1 do
    (* let c = xs.(i) in *)
    let c = unsafe_get xs i in
    (* xs.(i) <- xs.(!j); *)
    unsafe_set xs i (unsafe_get xs !j);
    (* xs.(!j) <- c; *)
    unsafe_set xs !j c;
    decr j
  done

(*$T rev_in_place
   let a = [|1;2;3;4|] in rev_in_place a; a = [|4;3;2;1|]
   let a = [|1;2;3|] in rev_in_place a; a = [|3;2;1|]
   let a = [||] in rev_in_place a; a=[||]
*)

let rev xs =
  let ys = copy xs in
  rev_in_place ys;
  ys

(*$Q rev
   (Q.array Q.int) ~count:5 (fun l -> rev l |> rev = l)
*)

let for_all p xs =
  let n = length xs in
  let rec loop i =
    if i = n then true
    else if p (unsafe_get xs i) then loop (succ i)
    else false
  in
  loop 0

(*$T for_all
   for_all (fun x -> x mod 2 = 0) [|2;4;6|]
   for_all (fun x -> x mod 2 = 0) [|2;3;6|] = false
   for_all (fun _ -> false) [||]
*)

let exists p xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if p (unsafe_get xs i) then true
    else loop (succ i)
  in
  loop 0

(*$T exists
   exists (fun x -> x mod 2 = 0) [|1;4;5|]
   exists (fun x -> x mod 2 = 0) [|1;3;5|] = false
   exists (fun _ -> false) [||] = false
*)


let mem a xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if a = unsafe_get xs i then true
    else loop (succ i)
  in
  loop 0

(*$T mem
   mem 2 [|1;2;3|]
   mem 2 [||] = false
   mem (ref 3) [|ref 1; ref 2; ref 3|]
*)

let memq a xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if a == unsafe_get xs i then true
    else loop (succ i)
  in
  loop 0

(*$T memq
   memq 2 [|1;2;3|]
   memq 2 [||] = false
   memq (ref 3) [|ref 1; ref 2; ref 3|] = false
*)

let findi p xs =
  let n = length xs in
  let rec loop i =
    if i = n then raise Not_found
    else if p (unsafe_get xs i) then i
    else loop (succ i)
  in
  loop 0
(*$Q findi
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.Observable.int Q.bool)) (fun (a, Q.Fun(_,f)) -> \
    try let index = findi f a in \
        let i = ref (-1) in \
        for_all (fun elt -> incr i; \
          if !i < index then not (f elt) \
          else if !i = index then f elt else true)\
        a \
    with Not_found -> for_all (fun elt -> not (f elt)) a)
*)

let find p xs = xs.(findi p xs)
(*$Q find
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.Observable.int Q.bool)) (fun (a, Q.Fun(_,f)) -> \
    let a = map (fun x -> `a x) a in \
    let f (`a x) = f x in\
    try let elt = find f a in \
        let past = ref false in \
        for_all (fun x -> if x == elt then (past := true; f x) \
                 else !past || not (f x)) \
        a \
    with Not_found -> for_all (fun elt -> not (f elt)) a)
*)

let find_opt p a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      let x = unsafe_get a i in
      if p x then Some x
      else loop (succ i)
  in
  loop 0

(*$T find_opt
    find_opt (fun x -> x < 0) [||] = None
    find_opt (fun x -> x < 0) [|0;1;2;3|] = None
    find_opt (fun x -> x >= 3) [|0;1;2;3|] = Some 3
*)

let find_map f a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      match f (unsafe_get a i) with
      | None -> loop (succ i)
      | Some _ as r -> r
  in
  loop 0

(*$T find_map
    find_map (fun x -> if x < 0 then Some x else None) [||] = None
    find_map (fun x -> if x < 0 then Some x else None) [|0;-1;2|] = (Some (-1))
    find_map (fun x -> if x < 0 then Some x else None) [|0;1;-2|] = (Some (-2))
*)

(* Use of BitSet suggested by Brian Hurt. *)
let filter p xs =
  let n = length xs in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p (unsafe_get xs i) then BatBitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BatBitSet.count bs in
  let j = ref 0 in
  init n'
    (fun _ -> match BatBitSet.next_set_bit bs !j with
      | Some i -> j := i+1; unsafe_get xs i
      | None ->
        (* not enough 1 bits - incorrect count? *)
        assert false
    )
(*$Q filter
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.Observable.int Q.bool)) (fun (a, Q.Fun(_,f)) -> \
    let b = to_list (filter f a) in \
    let b' = List.filter f (to_list a) in \
    List.for_all (fun (x,y) -> x = y) (List.combine b b') \
  )
*)

exception End of int

let fold_while p f init xs =
  let acc = ref init in
  try
    let n = length xs in
    for i = 0 to n - 1 do
      let x = unsafe_get xs i in
      if p !acc x then
        acc := f !acc x
      else
        raise (End i)
    done;
    (!acc, n)
  with End i -> (!acc, i)

(*$T fold_while
  fold_while (fun _ x -> x mod 2 = 0) (+) 0 [|1;2|] = (0, 0)
  fold_while (fun _ x -> x mod 2 = 1) (+) 0 [|1;2|] = (1, 1)
  fold_while (fun _ x -> x < 4) (+) 0 [|1;2;3;4|] = (6, 3)
  fold_while (fun _ x -> x < 4) (+) 0 [|1;2;3|] = (6, 3)
  fold_while (fun _ x -> x < 4) (+) 0 [||] = (0, 0)
*)

let count_matching p xs =
  let n = length xs in
  let count = ref 0 in
  for i = 0 to n - 1 do
    if p (unsafe_get xs i) then
      incr count
  done;
  !count

(*$T count_matching
  count_matching (fun _ -> true) [||] = 0
  count_matching (fun x -> x = -1) [|-1|] = 1
  count_matching (fun x -> x = -1) [|-1;0;-1|] = 2
*)

let filteri p xs =
  let n = length xs in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p i (unsafe_get xs i) then BatBitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BatBitSet.count bs in
  let j = ref 0 in
  init n'
    (fun _ -> match BatBitSet.next_set_bit bs !j with
      | Some i -> j := i+1; unsafe_get xs i
      | None ->
        (* not enough 1 bits - incorrect count? *)
        assert false
    )

(*$T filteri
   filteri (fun i x -> (i+x) mod 2 = 0) [|1;2;3;4;0;1;2;3|] = [|0;1;2;3|]
*)

let find_all = filter

(* <=> List.partition *)
let partition p a =
  let n = length a in
  if n = 0 then ([||], [||])
  else
    let ok_count = ref 0 in
    let mask =
      init n (fun i ->
          let pi = p (unsafe_get a i) in
          if pi then incr ok_count;
          pi) in
    let ko_count = n - !ok_count in
    let init = unsafe_get a 0 in
    let ok = make !ok_count init in
    let ko = make ko_count init in
    let j = ref 0 in
    let k = ref 0 in
    for i = 0 to n - 1 do
      let x = unsafe_get a i in
      let px = unsafe_get mask i in
      if px then
        (unsafe_set ok !j x;
         incr j)
      else
        (unsafe_set ko !k x;
         incr k)
    done;
    (ok, ko)
(*$Q partition
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.Observable.int Q.bool)) (fun (a, Q.Fun(_,f)) -> \
    let b1, b2 = partition f a in \
    let b1, b2 = to_list b1, to_list b2 in \
    let b1', b2' = List.partition f (to_list a) in \
    List.for_all (fun (x,y) -> x = y) (List.combine b1 b1') && \
    List.for_all (fun (x,y) -> x = y) (List.combine b2 b2') \
  )
*)

let enum xs =
  let rec make start xs =
    let n = length xs in
    (* inside the loop, as [make] may later be called with another array *)
    BatEnum.make
      ~next:(fun () ->
        if !start < n then
          unsafe_get xs (BatRef.post_incr start)
        else
          raise BatEnum.No_more_elements)
      ~count:(fun () ->
        n - !start)
      ~clone:(fun () ->
        make (BatRef.copy start) xs)
  in
  make (ref 0) xs
(*$Q enum
  (Q.array Q.small_int) (fun a -> \
    let e = enum a in \
    for i = 0 to length a / 2 - 1 do\
      assert (a.(i) = BatEnum.get_exn e)\
    done; \
    let e' = BatEnum.clone e in \
    assert (BatEnum.count e = BatEnum.count e'); \
    for i = length a / 2 to length a - 1 do \
      assert (a.(i) = BatEnum.get_exn e && a.(i) = BatEnum.get_exn e') \
    done; \
    BatEnum.is_empty e && BatEnum.is_empty e' \
  )
*)


let backwards xs =
  let rec make start xs =
    BatEnum.make
      ~next:(fun () ->
        if !start > 0 then
          unsafe_get xs (BatRef.pre_decr start)
        else
          raise BatEnum.No_more_elements)
      ~count:(fun () ->
        !start)
      ~clone:(fun () ->
        make (BatRef.copy start) xs)
  in
  make (ref (length xs)) xs
(*$Q backwards
  (Q.array Q.small_int) (fun a -> \
    let e = backwards a in \
    let n = length a in \
    for i = 0 to length a / 2 - 1 do\
      assert (a.(n - 1 - i) = BatEnum.get_exn e)\
    done; \
    let e' = BatEnum.clone e in \
    assert (BatEnum.count e = BatEnum.count e'); \
    for i = length a / 2 to length a - 1 do \
      assert (a.(n - 1 - i) = BatEnum.get_exn e && \
              a.(n - 1 - i) = BatEnum.get_exn e') \
    done; \
    BatEnum.is_empty e && BatEnum.is_empty e' \
  )
*)

let of_enum e =
  let n = BatEnum.count e in
  (* This assumes, reasonably, that init traverses the array in order. *)
  init n
    (fun _i ->
      match BatEnum.get e with
      | Some x -> x
      | None -> assert false)

let of_backwards e =
  of_list (BatList.of_backwards e)

let range xs = BatEnum.(--^) 0 (length xs)
(*$Q range
  (Q.array Q.small_int) (fun a -> \
    BatEnum.equal (=) (range a) \
     (enum (init (length a) (fun i -> i))))
*)

let filter_map p xs =
  of_enum (BatEnum.filter_map p (enum xs))
(*$Q filter_map
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.Observable.int (Q.option Q.int))) \
  (fun (a, Q.Fun (_,f)) -> \
    let a' = filter (fun elt -> f elt <> None) a in \
    let a' = map (f %> BatOption.get) a' in \
    let a = filter_map f a in \
    a = a' \
  )
*)

let iter2 f a1 a2 =
  if length a1 <> length a2
  then invalid_arg "Array.iter2";
  for i = 0 to length a1 - 1 do
    (* f a1.(i) a2.(i) *)
    f (unsafe_get a1 i) (unsafe_get a2 i)
  done
(*$Q iter2
  (Q.array Q.small_int) (fun a -> \
    let a' = map (fun a -> a + 1) a in \
    let i = ref (-1) in \
    let b = make (length a) (max_int, max_int) in \
    let f x1 x2 = incr i; b.(!i) <- (x1, x2) in \
    let b' = map (fun a -> (a, a + 1)) a in \
    iter2 f a a'; \
    b = b' \
  )
*)
(*$T iter2
  try iter2 (fun _ _ -> ()) [|1|] [|1;2;3|]; false \
    with Invalid_argument _ -> true
  try iter2 (fun _ _ -> ()) [|1|] [||]; false \
    with Invalid_argument _ -> true
*)

let iter2i f a1 a2 =
  if length a1 <> length a2
  then invalid_arg "Array.iter2i";
  for i = 0 to length a1 - 1 do
    (* f i a1.(i) a2.(i) *)
    f i (unsafe_get a1 i) (unsafe_get a2 i)
  done
(*$Q iter2i
  (Q.array Q.small_int) (fun a -> \
    let a' = map (fun a -> a + 1) a in \
    let i = ref (-1) in \
    let b = make (length a) (max_int, max_int) in \
    let f idx x1 x2 = incr i; assert (!i = idx); b.(!i) <- (x1, x2) in \
    let b' = map (fun a -> (a, a + 1)) a in \
    iter2i f a a'; \
    b = b' \
  )
*)
(*$T iter2i
  try iter2i (fun _ _ _ -> ()) [|1|] [|1;2;3|]; false \
    with Invalid_argument _ -> true
  try iter2i (fun _ _ _ -> ()) [|1|] [||]; false \
    with Invalid_argument _ -> true
*)

##V>=4.11##let for_all2 = Array.for_all2
##V<4.11##let for_all2 p xs ys =
##V<4.11##  let n = length xs in
##V<4.11##  if length ys <> n then invalid_arg "Array.for_all2";
##V<4.11##  let rec loop i =
##V<4.11##    if i = n then true
##V<4.11##    else if p (unsafe_get xs i) (unsafe_get ys i) then loop (succ i)
##V<4.11##    else false
##V<4.11##  in
##V<4.11##  loop 0

(*$T for_all2
   for_all2 (=) [|1;2;3|] [|3;2;1|] = false
   for_all2 (=) [|1;2;3|] [|1;2;3|]
   for_all2 (<>) [|1;2;3|] [|3;2;1|] = false
   try ignore (for_all2 (=) [|1;2;3|] [|1;2;3;4|]); false \
     with Invalid_argument _ -> true
   try ignore (for_all2 (=) [|1;2|] [||]); false \
     with Invalid_argument _ -> true
*)

##V>=4.11##let exists2 = Array.exists2
##V<4.11##let exists2 p xs ys =
##V<4.11##  let n = length xs in
##V<4.11##  if length ys <> n then invalid_arg "Array.exists2";
##V<4.11##  let rec loop i =
##V<4.11##    if i = n then false
##V<4.11##    else if p (unsafe_get xs i) (unsafe_get ys i) then true
##V<4.11##    else loop (succ i)
##V<4.11##  in
##V<4.11##  loop 0

(*$T exists2
   exists2 (=) [|1;2;3|] [|3;2;1|]
   exists2 (<>) [|1;2;3|] [|1;2;3|] = false
   try ignore (exists2 (=) [|1;2|] [|3|]); false \
     with Invalid_argument _ -> true
*)

let map2 f xs ys =
  let n = length xs in
  if length ys <> n then invalid_arg "Array.map2";
  init n (fun i -> f (unsafe_get xs i) (unsafe_get ys i))

(*$T map2
   map2 (-) [|1;2;3|] [|6;3;1|] = [|-5;-1;2|]
   map2 (-) [|2;4;6|] [|1;2;3|] = [|1;2;3|]
   try ignore (map2 (-) [|2;4|] [|1;2;3|]); false \
     with Invalid_argument _ -> true
   try ignore (map2 (-) [|2;4|] [|3|]); false \
     with Invalid_argument _ -> true
*)

let cartesian_product a b =
  let na = length a in
  let nb = length b in
  init
    (na * nb)
    (fun j ->
       let i = j / nb in
       (unsafe_get a i, unsafe_get b (j - i * nb)))

(*$T cartesian_product
  let a = cartesian_product [|1;2|] [|"a";"b"|] in \
    sort Legacy.compare a; \
    a = [|1,"a"; 1,"b"; 2,"a"; 2, "b" |]
*)

(*$Q cartesian_product
  (Q.pair (Q.list Q.small_int) (Q.list Q.small_int)) (fun(la,lb) -> \
    let a = of_list (List.take 5 la) and b = of_list (List.take 4 lb) in \
    length (cartesian_product a b) = length a * length b)
*)

let compare cmp a b =
  let length_a = length a in
  let length_b = length b in
  let length   = BatInt.min length_a length_b in
  let rec aux i =
    if i < length then
      let result = cmp (unsafe_get a i) (unsafe_get b i) in
      if result = 0 then aux (i + 1)
      else               result
    else
    if length_a = length_b then 0
    else if length_a < length_b then -1
    else                              1
  in
  aux 0

(*$T compare
   compare Legacy.compare [|1;2;3|] [|1;2|] = 1
   compare Legacy.compare [|1;2|] [|1;2;4|] = -1
   compare Legacy.compare [|1|] [||] = 1
   compare Legacy.compare [||] [||] = 0
   compare Legacy.compare [|1;2|] [|1;2|] = 0
   compare (fun x y -> -(Legacy.compare x y)) [|2;1|] [|1;2|] = -1
*)

let print ?(first="[|") ?(last="|]") ?(sep="; ") print_a  out t =
  match length t with
  | 0 ->
    BatInnerIO.nwrite out first;
    BatInnerIO.nwrite out last
  | n ->
    BatInnerIO.nwrite out first;
    print_a out (unsafe_get t 0);
    for i = 1 to n - 1 do
      BatInnerIO.nwrite out sep;
      print_a out (unsafe_get t i);
    done;
    BatInnerIO.nwrite out last
(*$T
  BatIO.to_string (print ~sep:"," ~first:"[" ~last:"]" BatInt.print) \
    [|2;4;66|] = "[2,4,66]"
  BatIO.to_string (print ~sep:"," ~first:"[" ~last:"]" BatInt.print) \
    [|2|] = "[2]"
  BatIO.to_string (print ~sep:"," ~first:"[" ~last:"]" BatInt.print) \
    [||] = "[]"
*)


let reduce f a =
  if length a = 0 then
    invalid_arg "Array.reduce: empty array"
  else
    let acc = ref (unsafe_get a 0) in
    for i = 1 to length a - 1 do
      acc := f !acc (unsafe_get a i)
    done;
    !acc

(*$T reduce
   reduce (+) [|1;2;3|] = 6
   reduce (fun _ -> assert false) [|1|] = 1
   try reduce (fun _ _ -> ()) [||]; false \
     with Invalid_argument _ -> true
*)

let min a = reduce Pervasives.min a
let max a = reduce Pervasives.max a

(*$T min
  min [|1;2;3|] = 1
  min [|2;3;1|] = 1
*)(*$T max
    max [|1;2;3|] = 3
    max [|2;3;1|] = 3
  *)

let min_max a =
  let n = length a in
  if n = 0 then
    invalid_arg "Array.min_max: empty array"
  else
    let mini = ref (unsafe_get a 0) in
    let maxi = ref (unsafe_get a 0) in
    for i = 1 to n - 1 do
      if (unsafe_get a i) > !maxi then maxi := (unsafe_get a i);
      if (unsafe_get a i) < !mini then mini := (unsafe_get a i)
    done;
    (!mini, !maxi)
(*$T min_max
    min_max [|1|] = (1, 1)
    min_max [|1;-2;10;3|] = (-2, 10)
    try ignore (min_max [||]); false with Invalid_argument _ -> true
*)

let sum = fold_left (+) 0
let fsum = fold_left (+.) 0.

(*$T sum
  sum [|1;2;3|] = 6
  sum [|0|] = 0
  sum [||] = 0
*) (*$T fsum
     fsum [|1.0;2.0;3.0|] = 6.0
     fsum [|0.0|] = 0.0
   *)

let kahan_sum arr =
  let sum = ref 0. in
  let err = ref 0. in
  for i = 0 to length arr - 1 do
    let x = (unsafe_get arr i) -. !err in
    let new_sum = !sum +. x in
    err := (new_sum -. !sum) -. x;
    sum := new_sum +. 0.;
    (* this suspicious +. 0. is added to help
       the hand of the somewhat flaky unboxing optimizer;
       it hopefully won't be necessary anymore
       in a few OCaml versions *)
  done;
  !sum +. 0.

(*$T kahan_sum
   kahan_sum [| |] = 0.
   kahan_sum [| 1.; 2. |] = 3.
   let n, x = 1_000, 1.1 in \
     Float.approx_equal (float n *. x) \
                        (kahan_sum (make n x))
*)

let flength a =
  float_of_int (length a)

let avg a =
  (float_of_int (sum a)) /. (flength a)

let favg a =
  (fsum a) /. (flength a)
;;
(*$T avg
  avg [|1; 2; 3|] = 2.
  avg [|0|] = 0.
*) (*$T favg
     favg [|1.0; 2.0; 3.0|] = 2.0
     favg [|0.0|] = 0.0
   *)

(* meant for tests, don't care about side effects being repeated
   or not failing early *)
let is_sorted_by f xs =
  let ok = ref true in
  for i = 0 to length xs - 2 do
    ok := !ok && (f (unsafe_get xs i)) <= (f (unsafe_get xs (i + 1)))
  done;
  !ok

(* TODO: Investigate whether a second array is better than pairs *)
let decorate_stable_sort f xs =
  let decorated = map (fun x -> (f x, x)) xs in
  let () = stable_sort (fun (i,_) (j,_) -> Pervasives.compare i j) decorated in
  map (fun (_,x) -> x) decorated
(*$T decorate_stable_sort
  decorate_stable_sort fst [|(1,2);(1,3);(0,2);(1,4)|] \
    = [|(0,2);(1,2);(1,3);(1,4)|]
*)
(*$Q decorate_stable_sort
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.Observable.int (Q.option Q.int))) \
    (fun (a, Q.Fun(_,f)) -> is_sorted_by f (decorate_stable_sort f a))
*)

let decorate_fast_sort f xs =
  let decorated = map (fun x -> (f x, x)) xs in
  let () = fast_sort (fun (i,_) (j,_) -> Pervasives.compare i j) decorated in
  map (fun (_,x) -> x) decorated
(*$Q decorate_fast_sort
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.Observable.int (Q.option Q.int))) \
    (fun (a, Q.Fun(_,f)) -> is_sorted_by f (decorate_fast_sort f a))
*)

let bsearch cmp arr x =
  let rec bsearch i j =
    if i > j
      then `Just_after j
      else
        let middle = i + (j - i) / 2 in (* avoid overflow *)
        match cmp x (unsafe_get arr middle) with
        | BatOrd.Eq -> `At middle
        | BatOrd.Lt -> bsearch i (middle - 1)
        | BatOrd.Gt -> bsearch (middle + 1) j
  in
  if length arr = 0 then `Empty
  else match (cmp (unsafe_get arr 0) x,
              cmp (unsafe_get arr (length arr - 1)) x) with
  | BatOrd.Gt, _ -> `All_bigger
  | _, BatOrd.Lt -> `All_lower
  | _ -> bsearch 0 (length arr - 1)

(*$T bsearch
  bsearch BatInt.ord [|1; 2; 2; 3; 4; 10|] 3 = `At 3
  bsearch BatInt.ord [|1; 2; 2; 3; 4; 10|] 5 = `Just_after 4
  bsearch BatInt.ord [|1; 2; 5; 5; 11; 12|] 1 = `At 0
  bsearch BatInt.ord [|1; 2; 5; 5; 11; 12|] 12 = `At 5
  bsearch BatInt.ord [|1; 2; 2; 3; 4; 9|] 10 = `All_lower
  bsearch BatInt.ord [|1; 2; 2; 3; 4; 9|] 0 = `All_bigger
  bsearch BatInt.ord [| |] 3 = `Empty
*)

let pivot_split cmp arr x =
  let open BatOrd in
  let n = length arr in
  (* find left edge between i and j *)
  let rec search_left i j =
    if i > j
    then i
    else
      let middle = i + (j-i)/2 in
      match cmp (unsafe_get arr middle) x with
      | Lt -> search_left (middle+1) j
      | Gt -> search_left i (middle-1)
      | Eq ->
        (* check whether [middle] is the edge, ie the leftmost index
           where arr.(_) = x *)
        let neighbor = middle - 1 in
        if neighbor < 0 || cmp (unsafe_get arr neighbor) x = Lt
          then middle                  (* found! *)
          else search_left i neighbor  (* go further on left *)
  (* find right edge, between i and j *)
  and search_right i j =
    if i > j
    then i
    else
      let middle = i + (j-i)/2 in
      match cmp (unsafe_get arr middle) x with
      | Lt -> search_right (middle+1) j
      | Gt -> search_right i (middle-1)
      | Eq ->
        let neighbor = middle + 1 in
        if neighbor = n || cmp (unsafe_get arr neighbor) x = Gt
          then middle + 1              (* found! *)
          else search_right neighbor j (* go further on right *)
  in
  (search_left 0 (n-1), search_right 0 (n-1))

(*$T pivot_split
  pivot_split BatInt.ord [| |] 1 = (0, 0)
  pivot_split BatInt.ord [|1;2;2;3;3;4;5|] 3 = (3,5)
  pivot_split BatInt.ord [|1;1;1;2;3;3;4;5|] 1 = (0,3)
  pivot_split BatInt.ord [|1;2;2;3;3;4;5|] 10 = (7,7)
  pivot_split BatInt.ord [|1;2;2;3;3;4;5|] 0 = (0,0)
  pivot_split BatInt.ord [|2;2;2|] 2 = (0,3)
  pivot_split BatInt.ord [|1;2;2;4;5|] 3 = (3,3)
*)

let insert xs x i =
  let len = length xs in
  if i < 0 || i > len then
    invalid_arg "Array.insert: offset out of range";
  init (len+1) (fun j ->
    if j < i then
      unsafe_get xs j
    else if j > i then
      unsafe_get xs (j-1)
    else
      x)

(*$T insert
   insert [|1;2;3|] 4 0 = [|4;1;2;3|]
   insert [|1;2;3|] 4 3 = [|1;2;3;4|]
   insert [|1;2;3|] 4 2 = [|1;2;4;3|]
   try ignore (insert [|1;2;3|] 4 100); false \
     with Invalid_argument _ -> true
   try ignore (insert [|1;2;3|] 4 (-40)); false \
     with Invalid_argument _ -> true
*)

let remove_at i src =
  let x = src.(i) in (* keep the bound check in there *)
  let n = length src in
  let dst = make (n - 1) x in
  blit src 0 dst 0 i;
  blit src (i + 1) dst i (n - i - 1);
  dst
(*$T remove_at
    try remove_at 0 [||] = [|1|] \
      with Invalid_argument _ -> true
    remove_at 0 [|1;2;3|] = [|2;3|]
    remove_at 1 [|1;2;3|] = [|1;3|]
    remove_at 2 [|1;2;3|] = [|1;2|]
    try remove_at 3 [|1;2;3|] = [|1|] \
      with Invalid_argument _ -> true
*)

(* helper function; only works for arrays of equal length *)
let eq_elements eq_elt a1 a2 = for_all2 eq_elt a1 a2

(* helper function to compare arrays *)
let rec ord_aux eq_elt i a1 a2 =
  let open BatOrd in
  if i >= length a1 then Eq
  else match eq_elt (unsafe_get a1 i) (unsafe_get a2 i) with
    | (Lt | Gt) as res -> res
    | Eq -> ord_aux eq_elt (i+1) a1 a2

let ord_elements eq_elt a1 a2 = ord_aux eq_elt 0 a1 a2

let equal eq a1 a2 =
  BatOrd.bin_eq
    BatInt.equal (length a1) (length a2)
    (eq_elements eq) a1 a2
(*$T equal
  equal (=) [|1;2;3|] [|1;2;3|]
  not (equal (=) [|1;2;3|] [|1;2;3;4|])
  not (equal (=) [|1;2;3;4|] [|1;2;3|])
  equal (=) [||] [||]
  equal (<>) [|1;2;3|] [|2;3;4|]
  not (equal (<>) [|1;2;3|] [|3;2;1|])
*)

let ord ord_elt a1 a2 =
  BatOrd.bin_ord
    BatInt.ord (length a1) (length a2)
    (ord_elements ord_elt) a1 a2
(*$T ord
  ord BatInt.ord [|2|] [|1;2|] = BatOrd.Lt
  ord BatInt.ord [|1;1|] [|2|] = BatOrd.Gt
  ord BatInt.ord [|1;1;1|] [|1;1;2|] = BatOrd.Lt
  ord BatInt.ord [|1;1;1|] [|1;1;1|] = BatOrd.Eq
*)

let shuffle ?state a =
  BatInnerShuffle.array_shuffle ?state a
(*$T shuffle
  let s = Random.State.make [|11|] in \
  let a = [|1;2;3;4;5;6;7;8;9|] in \
  shuffle ~state:s a; \
  let ocaml_version = int_of_string (String.make 1 Sys.ocaml_version.[0]) in \
  a = if ocaml_version < 5 then \
    [|7; 2; 9; 5; 3; 6; 4; 1; 8|] else \
    [|1; 7; 4; 9; 5; 2; 8; 6; 3|]
  let b = [||] in \
  shuffle b; \
  b = [||]
*)

(* equivalent of List.split *)
let split a =
  let n = length a in
  if n = 0 then ([||], [||])
  else
    let l, r = unsafe_get a 0 in
    let left = make n l in
    let right = make n r in
    for i = 1 to n - 1 do
      let l, r = unsafe_get a i in
      unsafe_set left i l;
      unsafe_set right i r
    done;
    (left, right)
(*$T split
  split [||] = ([||], [||])
  split [|(1,2);(3,4);(5,6)|] = ([|1;3;5|], [|2;4;6|])
*)

let combine a b =
  let m = length a in
  let n = length b in
  if m <> n then invalid_arg "Array.combine";
  map2 (fun x y -> (x, y)) a b

(*$T combine
    combine [||] [||] = [||]
    try combine [|1;2;3|] [||] = [||] with Invalid_argument _ -> true
    combine [|1;2;3|] [|4;5;6|] = [|(1,4);(2,5);(3,6)|]
*)

module Incubator = struct
  module Eq (T : BatOrd.Eq) = struct
    type t = T.t array
    let eq = equal T.eq
  end

  module Ord (T : BatOrd.Ord) = struct
    type t = T.t array
    let ord = ord T.ord
  end
end

let left a len = if len >= length a then a else sub a 0 len
let right a len = let alen = length a in
  if len >= alen then a else sub a (alen - len) len
let head a pos = left a pos
let tail a pos = let alen = length a in
  if pos >= alen then [||] else sub a pos (alen - pos)

(*$= left & ~printer:(IO.to_string (print Int.print))
  (left [|1;2;3|] 1) [|1|]
  (left [|1;2|] 3) [|1;2|]
  (left [|1;2;3|] 3) [|1;2;3|]
  (left [|1;2;3|] 10)[|1;2;3|]
  (left [|1;2;3|] 0) [||]
*) (*$= right & ~printer:(IO.to_string (print Int.print))
     (right [|1;2;3|] 1) [|3|]
     (right [|1;2|] 3) [|1;2|]
     (right [|1;2;3|] 3) [|1;2;3|]
     (right [|1;2;3|] 10) [|1;2;3|]
     (right [|1;2;3|] 0) [||]
   *) (*$= tail & ~printer:(IO.to_string (print Int.print))
        (tail [|1;2;3|] 1) [|2;3|]
        [||] (tail [|1;2;3|] 10)
        (tail [|1;2;3|] 0) [|1;2;3|]
      *) (*$= head & ~printer:(IO.to_string (print Int.print))
        (head [|1;2;3|] 1) [|1|]
        (head [|1;2;3|] 10) [|1;2;3|]
        (head [|1;2;3|] 0) [||]
      *)




module Cap =
struct
  (** Implementation note: in [('a, 'b) t], ['b] serves only as
      a phantom type, to mark which operations are only legitimate on
      readable arrays or writeable arrays.*)
  type ('a, 'b) t = 'a array constraint 'b = [< `Read | `Write]

  external of_array   : 'a array -> ('a, _ ) t                  = "%identity"
  external to_array   : ('a, [`Read | `Write]) t -> 'a array    = "%identity"
  external read_only  : ('a, [>`Read])  t -> ('a, [`Read])  t   = "%identity"
  external write_only : ('a, [>`Write]) t -> ('a, [`Write]) t   = "%identity"
  external length     : ('a, [> ]) t -> int                     = "%array_length"
  external get        : ('a, [> `Read]) t -> int -> 'a          = "%array_safe_get"
  external set        : ('a, [> `Write]) t -> int -> 'a -> unit = "%array_safe_set"
##V>=5.3##  external make       : int -> 'a -> ('a, _) t                  = "caml_array_make"
##V<5.3##  external make       : int -> 'a -> ('a, _) t                  = "caml_make_vect"
##V>=5.3##  external create     : int -> 'a -> ('a, _) t                  = "caml_array_make"
##V<5.3##  external create     : int -> 'a -> ('a, _) t                  = "caml_make_vect"


##V>=5.3##  external make_float: int -> (float, _) t = "caml_array_create_float"
##V>=4.2####V<5.3##  external make_float: int -> (float, _) t = "caml_make_float_vect"
##V<4.2##  let make_float n = make n 0.

  let init         = init
  let make_matrix  = make_matrix
  let create_matrix= make_matrix
  let iter         = iter
  let map          = map
  let filter       = filter
  let filter_map   = filter_map
  let count_matching = count_matching
  let iteri        = iteri
  let mapi         = mapi
  let modify       = modify
  let modifyi      = modifyi
  let fold_left    = fold_left
  let fold         = fold_left
  let fold_left_map = fold_left_map
  let fold_right   = fold_right
  let fold_while   = fold_while
  let iter2        = iter2
  let iter2i       = iter2i
  let for_all      = for_all
  let exists       = exists
  let find         = find
  let find_opt     = find_opt
  let find_map     = find_map
  let mem          = mem
  let memq         = memq
  let findi        = findi
  let find_all     = find_all
  let partition    = partition
  let rev          = rev
  let rev_in_place = rev_in_place
  let append       = append
  let concat       = concat
  let sub          = sub
  let copy         = copy
  let fill         = fill
  let blit         = blit
  let enum         = enum
  let of_enum      = of_enum
  let backwards    = backwards
  let of_backwards = of_backwards
  let to_list      = to_list
  let split        = split
  let combine      = combine
  let pivot_split  = pivot_split
  let of_list      = of_list
  let sort         = sort
  let stable_sort  = stable_sort
  let fast_sort    = fast_sort
  let compare      = compare
  let print        = print
  let ord          = ord
  let equal        = equal
  external unsafe_get : ('a, [> `Read]) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : ('a, [> `Write])t -> int -> 'a -> unit = "%array_unsafe_set"

  module Labels =
  struct
    let init i ~f = init i f
    let create len ~init = create len init
    let make = create
    let make_matrix ~dimx ~dimy x = make_matrix dimx dimy x
    let create_matrix = make_matrix
    let sub a ~pos ~len = sub a pos len
    let fill a ~pos ~len x = fill a pos len x
    let blit ~src ~src_pos ~dst ~dst_pos ~len = blit src src_pos dst dst_pos len
    let iter ~f a = iter f a
    let map ~f a = map  f a
    let iteri ~f a = iteri f a
    let mapi ~f a = mapi f a
    let modify ~f a = modify f a
    let modifyi ~f a = modifyi f a
    let fold_left ~f ~init a = fold_left f init a
    let fold_left_map ~f ~init a = fold_left_map f init a
    let fold = fold_left
    let fold_right ~f a ~init= fold_right f a init
    let fold_while ~p ~f ~init a = fold_while p f init a
    let sort ~cmp a = sort cmp a
    let stable_sort ~cmp a = stable_sort cmp a
    let fast_sort ~cmp a = fast_sort cmp a
    let iter2 ~f a b = iter2 f a b
    let exists ~f a  = exists f a
    let for_all ~f a = for_all f a
    let iter2i  ~f a b = iter2i f a b
    let find ~f a = find f a
    let find_opt ~f a = find_opt f a
    let find_map ~f a = find_map f a
    let filter ~f a = filter f a
    let filter_map ~f a = filter_map f a
    let count_matching ~f a = count_matching f a
  end

  module Exceptionless =
  struct
    let find f e =
      try Some (find f e)
      with Not_found -> None

    let findi f e =
      try Some (findi f e)
      with Not_found -> None
  end
end

module Exceptionless =
struct
  let find f e =
    try Some (find f e)
    with Not_found -> None

  let findi f e =
    try Some (findi f e)
    with Not_found -> None
end

module Labels =
struct
  let init i ~f = init i f
  let create len ~init = make len init
  let make = create
  let make_matrix ~dimx ~dimy x = make_matrix dimx dimy x
  let create_matrix = make_matrix
  let sub a ~pos ~len = sub a pos len
  let fill a ~pos ~len x = fill a pos len x
  let blit ~src ~src_pos ~dst ~dst_pos ~len = blit src src_pos dst dst_pos len
  let iter ~f a = iter f a
  let map  ~f a = map  f a
  let iteri ~f a = iteri f a
  let mapi  ~f a = mapi f a
  let modify ~f a = modify f a
  let modifyi ~f a = modifyi f a
  let fold_left ~f ~init a = fold_left f init a
  let fold_left_map ~f ~init a = fold_left_map f init a
  let fold = fold_left
  let fold_right ~f a ~init= fold_right f a init
  let fold_while ~p ~f ~init a = fold_while p f init a
  let sort ~cmp a = sort cmp a
  let stable_sort ~cmp a = stable_sort cmp a
  let fast_sort ~cmp a = fast_sort cmp a
  let iter2 ~f a b = iter2 f a b
  let exists ~f a  = exists f a
  let for_all ~f a = for_all f a
  let iter2i  ~f a b = iter2i f a b
  let find ~f a = find f a
  let find_opt ~f a = find_opt f a
  let find_map ~f a = find_map f a
  let findi ~f e = findi f e
  let filter ~f a = filter f a
  let filter_map ~f a = filter_map f a
  let count_matching ~f a = count_matching f a
  module LExceptionless = struct
    include Exceptionless
    let find ~f e = find f e
    let findi ~f e = findi f e
  end
end
