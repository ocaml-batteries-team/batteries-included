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


type 'a t = 'a array
type 'a enumerable = 'a t
type 'a mappable = 'a t

include Array

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
    let c = xs.(i) in
    xs.(i) <- xs.(!j);
    xs.(!j) <- c;
    decr j
  done

(*$T rev_in_place
   let a = [|1;2;3;4|] in rev_in_place a; a = [|4;3;2;1|]
   let a = [|1;2;3|] in rev_in_place a; a = [|3;2;1|]
   let a = [||] in rev_in_place a; a=[||]
*)

let rev xs =
  let ys = Array.copy xs in
  rev_in_place ys;
  ys

(*$Q rev
   (Q.array Q.int) ~count:5 (fun l -> rev l |> rev = l)
*)

let for_all p xs =
  let n = length xs in
  let rec loop i =
    if i = n then true
    else if p xs.(i) then loop (succ i)
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
    else if p xs.(i) then true
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
    else if a = xs.(i) then true
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
    else if a == xs.(i) then true
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
    else if p xs.(i) then i
    else loop (succ i)
  in
  loop 0
(*$Q findi
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.small_int Q.bool)) (fun (a, f) -> \
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
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.small_int Q.bool)) (fun (a, f) -> \
    let a = map (fun x -> `a x) a in \
    let f (`a x) = f x in\
    try let elt = find f a in \
        let past = ref false in \
        for_all (fun x -> if x == elt then (past := true; f x) \
                 else !past || not (f x)) \
        a \
    with Not_found -> for_all (fun elt -> not (f elt)) a)
*)

(* Use of BitSet suggested by Brian Hurt. *)
let filter p xs =
  let n = length xs in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p xs.(i) then BatBitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BatBitSet.count bs in
  let j = ref 0 in
  init n'
    (fun _ -> match BatBitSet.next_set_bit bs !j with
      | Some i -> j := i+1; xs.(i)
      | None ->
        (* not enough 1 bits - incorrect count? *)
        assert false (*BISECT-VISIT*)
    )
(*$Q filter
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.small_int Q.bool)) (fun (a, f) -> \
    let b = Array.to_list (filter f a) in \
    let b' = List.filter f (Array.to_list a) in \
    List.for_all (fun (x,y) -> x = y) (List.combine b b') \
  )
*)

let filteri p xs =
  let n = length xs in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p i xs.(i) then BatBitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BatBitSet.count bs in
  let j = ref 0 in
  init n'
    (fun _ -> match BatBitSet.next_set_bit bs !j with
      | Some i -> j := i+1; xs.(i)
      | None ->
        (* not enough 1 bits - incorrect count? *)
        assert false (*BISECT-VISIT*)
    )

(*$T filteri
   filteri (fun i x -> (i+x) mod 2 = 0) [|1;2;3;4;0;1;2;3|] = [|0;1;2;3|]
*)

let find_all = filter

let partition p xs =
  let n = length xs in
  (* Use a bitset to store which elements will be in which final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p xs.(i) then BatBitSet.set bs i
  done;
  (* Allocate the final arrays and copy elements into them. *)
  let n1 = BatBitSet.count bs in
  let n2 = n - n1 in
  let j = ref 0 in
  let xs1 = init n1
      (fun _ ->
        (* Find the next set bit in the BitSet. *)
        while not (BatBitSet.mem bs !j) do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
  let j = ref 0 in
  let xs2 = init n2
      (fun _ ->
        (* Find the next clear bit in the BitSet. *)
        while BatBitSet.mem bs !j do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
  xs1, xs2
(*$Q partition
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.small_int Q.bool)) (fun (a, f) -> \
    let b1, b2 = partition f a in \
    let b1, b2 = Array.to_list b1, Array.to_list b2 in \
    let b1', b2' = List.partition f (Array.to_list a) in \
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
          xs.(BatRef.post_incr start)
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
    for i = 0 to Array.length a / 2 - 1 do\
      assert (a.(i) = BatEnum.get_exn e)\
    done; \
    let e' = BatEnum.clone e in \
    assert (BatEnum.count e = BatEnum.count e'); \
    for i = Array.length a / 2 to Array.length a - 1 do \
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
          xs.(BatRef.pre_decr start)
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
    let n = Array.length a in \
    for i = 0 to Array.length a / 2 - 1 do\
      assert (a.(n - 1 - i) = BatEnum.get_exn e)\
    done; \
    let e' = BatEnum.clone e in \
    assert (BatEnum.count e = BatEnum.count e'); \
    for i = Array.length a / 2 to Array.length a - 1 do \
      assert (a.(n - 1 - i) = BatEnum.get_exn e && \
              a.(n - 1 - i) = BatEnum.get_exn e') \
    done; \
    BatEnum.is_empty e && BatEnum.is_empty e' \
  )
*)

let of_enum e =
  let n = BatEnum.count e in
  (* This assumes, reasonably, that init traverses the array in order. *)
  Array.init n
    (fun _i ->
      match BatEnum.get e with
      | Some x -> x
      | None -> assert false (*BISECT-VISIT*))

let of_backwards e =
  of_list (BatList.of_backwards e)

let range xs = BatEnum.(--^) 0 (Array.length xs)
(*$Q range
  (Q.array Q.small_int) (fun a -> \
    BatEnum.equal (=) (range a) \
     (enum (Array.init (Array.length a) (fun i -> i))))
*)

let filter_map p xs =
  of_enum (BatEnum.filter_map p (enum xs))
(*$Q filter_map
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.small_int (Q.option Q.int))) \
  (fun (a, f) -> \
    let a' = filter (fun elt -> f elt <> None) a in \
    let a' = map (f %> BatOption.get) a' in \
    let a = filter_map f a in \
    a = a' \
  )
*)

let iter2 f a1 a2 =
  if Array.length a1 <> Array.length a2
  then raise (Invalid_argument "Array.iter2");
  for i = 0 to Array.length a1 - 1 do
    f a1.(i) a2.(i);
  done
(*$Q iter2
  (Q.array Q.small_int) (fun a -> \
    let a' = Array.map (fun a -> a + 1) a in \
    let i = ref (-1) in \
    let b = Array.make (Array.length a) (max_int, max_int) in \
    let f x1 x2 = incr i; b.(!i) <- (x1, x2) in \
    let b' = Array.map (fun a -> (a, a + 1)) a in \
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
  if Array.length a1 <> Array.length a2
  then raise (Invalid_argument "Array.iter2i");
  for i = 0 to Array.length a1 - 1 do
    f i a1.(i) a2.(i);
  done
(*$Q iter2i
  (Q.array Q.small_int) (fun a -> \
    let a' = Array.map (fun a -> a + 1) a in \
    let i = ref (-1) in \
    let b = Array.make (Array.length a) (max_int, max_int) in \
    let f idx x1 x2 = incr i; assert (!i = idx); b.(!i) <- (x1, x2) in \
    let b' = Array.map (fun a -> (a, a + 1)) a in \
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

let for_all2 p xs ys =
  let n = length xs in
  if length ys <> n then raise (Invalid_argument "Array.for_all2");
  let rec loop i =
    if i = n then true
    else if p xs.(i) ys.(i) then loop (succ i)
    else false
  in
  loop 0

(*$T for_all2
   for_all2 (=) [|1;2;3|] [|3;2;1|] = false
   for_all2 (=) [|1;2;3|] [|1;2;3|]
   for_all2 (<>) [|1;2;3|] [|3;2;1|] = false
   try ignore (for_all2 (=) [|1;2;3|] [|1;2;3;4|]); false \
     with Invalid_argument _ -> true
   try ignore (for_all2 (=) [|1;2|] [||]); false \
     with Invalid_argument _ -> true
*)

let exists2 p xs ys =
  let n = length xs in
  if length ys <> n then raise (Invalid_argument "Array.exists2");
  let rec loop i =
    if i = n then false
    else if p xs.(i) ys.(i) then true
    else loop (succ i)
  in
  loop 0

(*$T exists2
   exists2 (=) [|1;2;3|] [|3;2;1|]
   exists2 (<>) [|1;2;3|] [|1;2;3|] = false
   try ignore (exists2 (=) [|1;2|] [|3|]); false \
     with Invalid_argument _ -> true
*)

let map2 f xs ys =
  let n = length xs in
  if length ys <> n then raise (Invalid_argument "Array.map2");
  Array.init n (fun i -> f xs.(i) ys.(i))

(*$T map2
   map2 (-) [|1;2;3|] [|6;3;1|] = [|-5;-1;2|]
   map2 (-) [|2;4;6|] [|1;2;3|] = [|1;2;3|]
   try ignore (map2 (-) [|2;4|] [|1;2;3|]); false \
     with Invalid_argument _ -> true
   try ignore (map2 (-) [|2;4|] [|3|]); false \
     with Invalid_argument _ -> true
*)

let compare cmp a b =
  let length_a = Array.length a in
  let length_b = Array.length b in
  let length   = BatInt.min length_a length_b in
  let rec aux i =
    if i < length then
      let result = cmp (unsafe_get a i) (unsafe_get b i) in
      if result = 0 then aux (i + 1)
      else               result
    else
    if length_a = length_b then	0
    else if length_a < length_b then -1
    else                              1
  in
  aux 0

(*$T compare
   compare Pervasives.compare [|1;2;3|] [|1;2|] = 1
   compare Pervasives.compare [|1;2|] [|1;2;4|] = -1
   compare Pervasives.compare [|1|] [||] = 1
   compare Pervasives.compare [||] [||] = 0
   compare Pervasives.compare [|1;2|] [|1;2|] = 0
   compare (fun x y -> -(Pervasives.compare x y)) [|2;1|] [|1;2|] = -1
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
  if Array.length a = 0 then
    invalid_arg "Array.reduce: empty array"
  else (
    let acc = ref a.(0) in
    for i = 1 to Array.length a - 1 do acc := f !acc a.(i) done;
    !acc
  )

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

let sum = reduce (+)
let fsum = reduce (+.)

(*$T sum
  sum [|1;2;3|] = 6
  sum [|0|] = 0
*) (*$T fsum
     fsum [|1.0;2.0;3.0|] = 6.0
     fsum [|0.0|] = 0.0
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
  for i = 0 to Array.length xs - 2 do
    ok := !ok && (f (xs.(i)) <= f (xs.(i + 1)))
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
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.small_int (Q.option Q.int))) \
    (fun (a, f) -> is_sorted_by f (decorate_stable_sort f a))
*)

let decorate_fast_sort f xs =
  let decorated = map (fun x -> (f x, x)) xs in
  let () = fast_sort (fun (i,_) (j,_) -> Pervasives.compare i j) decorated in
  map (fun (_,x) -> x) decorated
(*$Q decorate_fast_sort
  (Q.pair (Q.array Q.small_int) (Q.fun1 Q.small_int (Q.option Q.int))) \
    (fun (a, f) -> is_sorted_by f (decorate_fast_sort f a))
*)

let insert xs x i =
  if i > Array.length xs then invalid_arg "Array.insert: offset out of range";
  Array.init (Array.length xs + 1) (fun j -> if j < i then xs.(j) else if j > i then xs.(j-1) else x)

(*$T insert
   insert [|1;2;3|] 4 0 = [|4;1;2;3|]
   insert [|1;2;3|] 4 3 = [|1;2;3;4|]
   insert [|1;2;3|] 4 2 = [|1;2;4;3|]
   try ignore (insert [|1;2;3|] 4 100); false \
     with Invalid_argument _ -> true
   try ignore (insert [|1;2;3|] 4 (-40)); false \
     with Invalid_argument _ -> true
*)


(* helper function; only works for arrays of equal length *)
let eq_elements eq_elt a1 a2 = for_all2 eq_elt a1 a2

(* helper function to compare arrays *)
let rec ord_aux eq_elt i a1 a2 =
  let open BatOrd in
  if i >= length a1 then Eq
  else match eq_elt a1.(i) a2.(i) with
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

(*$= left & ~printer:(IO.to_string (Array.print Int.print))
  (left [|1;2;3|] 1) [|1|]
  (left [|1;2|] 3) [|1;2|]
  (left [|1;2;3|] 3) [|1;2;3|]
  (left [|1;2;3|] 10)[|1;2;3|]
  (left [|1;2;3|] 0) [||]
*) (*$= right & ~printer:(IO.to_string (Array.print Int.print))
     (right [|1;2;3|] 1) [|3|]
     (right [|1;2|] 3) [|1;2|]
     (right [|1;2;3|] 3) [|1;2;3|]
     (right [|1;2;3|] 10) [|1;2;3|]
     (right [|1;2;3|] 0) [||]
   *) (*$= tail & ~printer:(IO.to_string (Array.print Int.print))
        (tail [|1;2;3|] 1) [|2;3|]
        [||] (tail [|1;2;3|] 10)
        (tail [|1;2;3|] 0) [|1;2;3|]
      *) (*$= head & ~printer:(IO.to_string (Array.print Int.print))
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
  external make       : int -> 'a -> ('a, _) t                  = "caml_make_vect"
  external create     : int -> 'a -> ('a, _) t                  = "caml_make_vect"

  let init         = init
  let make_matrix  = make_matrix
  let create_matrix= create_matrix
  let iter         = iter
  let map          = map
  let filter       = filter
  let filter_map   = filter_map
  let iteri        = iteri
  let mapi         = mapi
  let modify       = modify
  let modifyi      = modifyi
  let fold_left    = fold_left
  let fold_right   = fold_right
  let iter2        = iter2
  let iter2i       = iter2i
  let for_all      = for_all
  let exists       = exists
  let find         = find
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

  (*BISECT-IGNORE-BEGIN*)
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
    let fold_right ~f a ~init= fold_right f a init
    let sort ~cmp a = sort cmp a
    let stable_sort ~cmp a = stable_sort cmp a
    let fast_sort ~cmp a = fast_sort cmp a
    let iter2 ~f a b = iter2 f a b
    let exists ~f a  = exists f a
    let for_all ~f a = for_all f a
    let iter2i  ~f a b = iter2i f a b
    let find ~f a = find f a
    let filter ~f a = filter f a
    let filter_map ~f a = filter_map f a
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
  (*BISECT-IGNORE-END*)
end

(*BISECT-IGNORE-BEGIN*)
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
  let create len ~init = create len init
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
  let fold_right ~f a ~init= fold_right f a init
  let sort ~cmp a = sort cmp a
  let stable_sort ~cmp a = stable_sort cmp a
  let fast_sort ~cmp a = fast_sort cmp a
  let iter2 ~f a b = iter2 f a b
  let exists ~f a  = exists f a
  let for_all ~f a = for_all f a
  let iter2i  ~f a b = iter2i f a b
  let find ~f a = find f a
  let findi ~f e = findi f e
  let filter ~f a = filter f a
  let filter_map ~f a = filter_map f a
  module LExceptionless = struct
    include Exceptionless
    let find ~f e = find f e
    let findi ~f e = findi f e
  end
end
(*BISECT-IGNORE-END*)
