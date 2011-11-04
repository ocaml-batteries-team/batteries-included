(*
 * ExtArray - additional and modified functions for arrays.
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

open Array

let map = map

let modify f a =
  for i = 0 to length a - 1 do
    unsafe_set a i (f (unsafe_get a i))
  done

let modifyi f a =
  for i = 0 to length a - 1 do
    unsafe_set a i (f i (unsafe_get a i))
  done

(**T modify
   let a = [|3;2;1|] in modify (fun x -> x + 1) a; a = [|4;3;2|]
   let a = [|3;2;1|] in modifyi (fun i x -> i * x) a; a = [|0;2;2|]
 **)

let fold_lefti f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r i (unsafe_get a i)
  done;
  !r

(**T fold_left
   fold_lefti (fun a i x -> a + i * x) 1 [|2;4;5|] = 1 + 0 + 4 + 10
   fold_lefti (fun a i x -> a + i * x) 1 [||] = 1
 **)

let rev_in_place xs =
  let n = length xs in
  let j = ref (n-1) in
  for i = 0 to n/2-1 do
    let c = xs.(i) in
    xs.(i) <- xs.(!j);
    xs.(!j) <- c;
    decr j
  done

(**T rev_in_place
   let a = [|1;2;3;4|] in rev_in_place a; a = [|4;3;2;1|]
   let a = [|1;2;3|] in rev_in_place a; a = [|3;2;1|]
   let a = [||] in rev_in_place a; a=[||]
 **)

let rev xs =
  let ys = Array.copy xs in
  rev_in_place ys;
  ys

(**Q rev
   (Q.array Q.int) ~count:5 (fun l -> rev l |> rev = l)
 **)

let for_all p xs =
  let n = length xs in
  let rec loop i =
    if i = n then true
    else if p xs.(i) then loop (succ i)
    else false
  in
  loop 0

(**T for_all
   for_all (fun x -> x mod 2 = 0) [|2;4;6|]
   for_all (fun x -> x mod 2 = 0) [|2;3;6|] = false
   for_all (fun _ -> false) [||]
 **)

let exists p xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if p xs.(i) then true
    else loop (succ i)
  in
  loop 0

(**T for_all
   exists (fun x -> x mod 2 = 0) [|1;4;5|]
   exists (fun x -> x mod 2 = 0) [|1;3;5|] = false
   exists (fun _ -> false) [||] = false
 **)


let mem a xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if a = xs.(i) then true
    else loop (succ i)
  in
  loop 0

(**T mem
   mem 2 [|1;2;3|] 
   mem 2 [||] = false
   mem (ref 3) [|ref 1; ref 2; ref 3|]
 **)

let memq a xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if a == xs.(i) then true
    else loop (succ i)
  in
  loop 0

(**T memq
   memq 2 [|1;2;3|]
   memq 2 [||] = false
   memq (ref 3) [|ref 1; ref 2; ref 3|] = false
 **)

let findi p xs =
  let n = length xs in
  let rec loop i =
    if i = n then raise Not_found
    else if p xs.(i) then i
    else loop (succ i)
  in
  loop 0

let find p xs = xs.(findi p xs)

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
  let xs' = init n'
    (fun _ ->
       (* Find the next set bit in the BitSet. *)
       while not (BatBitSet.is_set bs !j) do incr j done;
       let r = xs.(!j) in
       incr j;
       r) in
  xs'

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
  let xs' = init n'
    (fun _ ->
       (* Find the next set bit in the BitSet. *)
       while not (BatBitSet.is_set bs !j) do incr j done;
       let r = xs.(!j) in
       incr j;
       r) in
  xs'

(**T array_filteri
   filteri (fun i x -> (i+x) mod 2 = 0) [|1;2;3;4;0;1;2;3|] = [|0;1;2;3|]
**)

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
       while not (BatBitSet.is_set bs !j) do incr j done;
       let r = xs.(!j) in
       incr j;
       r) in
  let j = ref 0 in
  let xs2 = init n2
    (fun _ ->
       (* Find the next clear bit in the BitSet. *)
       while BatBitSet.is_set bs !j do incr j done;
       let r = xs.(!j) in
       incr j;
       r) in
  xs1, xs2

let enum xs =
  let rec make start xs =
    let n = length xs in(*Inside the loop, as [make] may later be called with another array*)
    BatEnum.make
      ~next:(fun () ->
	       if !start < n then
		 xs.(BatRef.post_incr start)
	       else
		 raise BatEnum.No_more_elements)
      ~count:(fun () ->
		n - !start)
      ~clone:(fun () ->
		let xs' = Array.sub xs !start (n - !start) in
		make (ref 0) xs')
  in
  make (ref 0) xs

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
		let xs' = Array.sub xs 0 !start in
		make (BatRef.copy start) xs')
  in
  make (ref (length xs)) xs

let of_enum e =
  let n = BatEnum.count e in
  (* This assumes, reasonably, that init traverses the array in order. *)
  Array.init n
    (fun i ->
       match BatEnum.get e with
       | Some x -> x
       | None -> assert false)

let of_backwards e =
  of_list (BatList.of_backwards e)

let range xs = BatEnum.(--^) 0 (Array.length xs)

let filter_map p xs =
  of_enum (BatEnum.filter_map p (enum xs))

let iter2 f a1 a2 =
  if Array.length a1 <> Array.length a2
  then raise (Invalid_argument "Array.iter2");
  for i = 0 to Array.length a1 - 1 do
    f a1.(i) a2.(i);
  done;;

let iter2i f a1 a2 =
  if Array.length a1 <> Array.length a2
  then raise (Invalid_argument "Array.iter2i");
  for i = 0 to Array.length a1 - 1 do
    f i a1.(i) a2.(i);
  done;;

let for_all2 p xs ys =
  let n = length xs in
  if length ys <> n then raise (Invalid_argument "Array.for_all2");
  let rec loop i =
    if i = n then true
    else if p xs.(i) ys.(i) then loop (succ i)
    else false
  in
  loop 0

let exists2 p xs ys =
  let n = length xs in
  if length ys <> n then raise (Invalid_argument "Array.exists2");
  let rec loop i =
    if i = n then false
    else if p xs.(i) ys.(i) then true
    else loop (succ i)
  in
  loop 0

let map2 f xs ys =
  let n = length xs in
  if length ys <> n then raise (Invalid_argument "Array.exists2");
  Array.init n (fun i -> f xs.(i) ys.(i))

let make_compare cmp a b =
  let length_a = Array.length a
  and length_b = Array.length b in
  let length   = min length_a length_b
  in
  let rec aux i = 
    if i < length then
      let result = cmp (unsafe_get a i) (unsafe_get b i) in
	if result = 0 then aux (i + 1)
	else               result
    else
      if length_a < length_b then -1
      else                         1
  in aux 0

let print ?(first="[|") ?(last="|]") ?(sep="; ") print_a  out t =
  match length t with
    | 0 ->
	BatInnerIO.nwrite out first;
	BatInnerIO.nwrite out last
    | 1 ->
	BatInnerIO.Printf.fprintf out "%s%a%s" first print_a (unsafe_get t 0) last
    | n -> 
	BatInnerIO.nwrite out first;
	print_a out (unsafe_get t 0);
	for i = 1 to n - 1 do
	  BatInnerIO.Printf.fprintf out "%s%a" sep print_a (unsafe_get t i) 
	done;
	BatInnerIO.nwrite out last

let t_printer a_printer paren out x = print (a_printer false) out x

let sprint ?(first="[|") ?(last="|]") ?(sep="; ") print_a array =
  BatInnerIO.Printf.sprintf2 "%a" (print ~first ~last ~sep print_a) array
(*  let os = BatInnerIO.output_string  () in
  print ~first ~last ~sep print_a os list;
  BatInnerIO.close_out os (* returns contents *)*)

let reduce f a =
  if Array.length a = 0 then
    invalid_arg "Array.reduce: empty array"
  else (
    let acc = ref a.(0) in
    for i = 1 to Array.length a - 1 do acc := f !acc a.(i) done;
    !acc
  )

let min a = reduce Pervasives.min a
let max a = reduce Pervasives.max a

(* TODO: Investigate whether a second array is better than pairs *)
let decorate_stable_sort f xs = 
  let decorated = map (fun x -> (f x, x)) xs in
  let () = stable_sort (fun (i,_) (j,_) -> compare i j) decorated in
  map (fun (_,x) -> x) decorated

let decorate_fast_sort f xs = 
  let decorated = map (fun x -> (f x, x)) xs in
  let () = fast_sort (fun (i,_) (j,_) -> compare i j) decorated in
  map (fun (_,x) -> x) decorated


let insert xs x i =
  Array.init (Array.length xs + 1) (fun j -> if j < i then xs.(j) else if j > i then xs.(j-1) else x)

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
  let make_compare = make_compare
  let print        = print
  let sprint       = sprint
  external unsafe_get : ('a, [> `Read]) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : ('a, [> `Write])t -> int -> 'a -> unit = "%array_unsafe_set"

  module Labels =
  struct
      let init i ~f = init i f
      let create len ~init = create len init
      let make             = create
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
      let map ~f a = map f a
      let mapi ~f a = mapi f a
      let filter ~f a = filter f a
      let filter_map ~f a = filter_map f a
  end

  module Exceptionless =
  struct
    let find f e =
      try  Some (find f e)
      with Not_found -> None
	
    let findi f e =
      try  Some (findi f e)
      with Not_found -> None
  end
end

module Exceptionless =
struct
  let find f e =
    try  Some (find f e)
    with Not_found -> None

  let findi f e =
    try  Some (findi f e)
    with Not_found -> None
end

module Labels =
struct
  let init i ~f = init i f
  let create len ~init = create len init
  let make             = create
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
  let map ~f a = map f a
  let mapi ~f a = mapi f a
  let filter ~f a = filter f a
  let filter_map ~f a = filter_map f a
  module LExceptionless = struct
    include Exceptionless
    let find ~f e = find f e
    let findi ~f e = findi f e
  end
end



