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


open List

(* Thanks to Jacques Garrigue for suggesting the following structure *)
type 'a mut_list =  {
	hd: 'a; 
	mutable tl: 'a list
}

type 'a t = 'a list
type 'a enumerable = 'a t
type 'a mappable = 'a t

external inj : 'a mut_list -> 'a list = "%identity"


let dummy_node () = { hd = Obj.magic (); tl = [] }

let cons h t = h::t

let is_empty = function
  | [] -> true
  | _  -> false

let nth l index =
	if index < 0 then invalid_arg "Negative index not allowed";
	let rec loop n = function
		| [] -> invalid_arg "Index past end of list";
		| h :: t -> 
			if n = 0 then h else loop (n - 1) t
	in
	loop index l

let at = nth

let append l1 l2 =
	match l1 with
	| [] -> l2
	| h :: t ->
		let rec loop dst = function
		| [] ->
			dst.tl <- l2
		| h :: t ->
			let cell = { hd = h; tl = [] } in
			dst.tl <- inj cell;
			loop cell t
		in
		let r = { hd = h; tl = [] } in
		loop r t;
		inj r

let rec flatten l =
	let rec inner dst = function
		| [] -> dst
		| h :: t ->
			let r = { hd = h; tl = [] } in
			dst.tl <- inj r;
			inner r t
	in
	let rec outer dst = function
		| [] -> ()
		| h :: t -> outer (inner dst h) t
	in
	let r = dummy_node () in
	outer r l;
	r.tl

let concat = flatten

let map f = function
	| [] -> []
	| h :: t ->
		let rec loop dst = function
		| [] -> ()
		| h :: t ->
			let r = { hd = f h; tl = [] } in
			dst.tl <- inj r;
			loop r t
		in
		let r = { hd = f h; tl = [] } in
		loop r t;
		inj r

let rec drop n = function
	| _ :: l when n > 0 -> drop (n-1) l
	| l -> l

let take n l =
	let rec loop n dst = function
		| h :: t when n > 0 ->
			let r = { hd = h; tl = [] } in
			dst.tl <- inj r;
			loop (n-1) r t
		| _ ->
			()
	in
	let dummy = dummy_node() in
	loop n dummy l;
	dummy.tl

(* takewhile and dropwhile by Richard W.M. Jones. *)
let rec take_while f = function
  | [] -> []
  | x :: xs when f x -> x :: take_while f xs
  | _ -> []

let rec drop_while f = function
  | [] -> []
  | x :: xs when f x -> drop_while f xs
  | xs -> xs

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

let make_compare c l1 l2 =
  let rec aux l1 l2 = match (l1, l2) with
    | (h1::t1, h2::t2) -> let result = c h1 h2 in
	if result = 0 then aux t1 t2
	else               result
    | ([],     []    ) -> 0
    | (_,      []    ) -> 1
    | ([],     _     ) -> -1
  in aux l1 l2

let rec unique ?(cmp = ( = )) l =
	let rec loop dst = function
		| [] -> ()
		| h :: t ->
			match exists (cmp h) t with
			| true -> loop dst t
			| false ->
				let r = { hd =  h; tl = [] }  in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node() in
	loop dummy l;
	dummy.tl

let unique_eq ?eq l = unique ?cmp:eq l

let unique_cmp ?(cmp = Pervasives.compare) l =
  let set      = ref (BatMap.create cmp) in
  let should_keep x = 
    if BatMap.mem x !set then false
    else ( set := BatMap.add x true !set; true )
  in
  (* use a stateful filter to remove duplicate elements *)
  filter should_keep l

let filter_map f l =
	let rec loop dst = function
		| [] -> ()
		| h :: t ->
			match f h with
			| None -> loop dst t
			| Some x ->
				let r = { hd = x; tl = [] }  in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node() in
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
				let r = { hd = f h1 h2; tl = [] } in
				dst.tl <- inj r;
				loop r t1 t2
			| _ -> invalid_arg "map2: Different_list_size"
	in
	let dummy = dummy_node () in
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
				let r = { hd = pair; tl = [] } in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node () in
	loop dummy lst;
	dummy.tl

let remove_assq x lst = 
	let rec loop dst = function
		| [] -> ()
		| (a, _ as pair) :: t ->
			if a == x then
				dst.tl <- t
			else
				let r = { hd =  pair; tl = [] } in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node() in
	loop dummy lst;
	dummy.tl

let rfind p l = find p (rev l)

let find_all p l = 
	let rec findnext dst = function
		| [] -> ()
		| h :: t -> 
			if p h then
				let r = { hd = h; tl = [] } in
				dst.tl <- inj r;
				findnext r t
			else
				findnext dst t
	in
	let dummy = dummy_node () in
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
    | h::t when h = e -> Some n
    | _::t            -> loop ( n + 1 ) t
  in loop 0 l

let rec index_ofq e l =
  let rec loop n = function
    | []               -> None
    | h::t when h == e -> Some n
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
	let yesdummy = dummy_node()
	and nodummy = dummy_node()
	in
	loop yesdummy nodummy lst;
	yesdummy.tl, nodummy.tl

let split lst =
	let rec loop adst bdst = function
		| [] -> ()
		| (a, b) :: t -> 
			let x = { hd = a; tl = [] } 
			and y = { hd = b; tl = [] } in
			adst.tl <- inj x;
			bdst.tl <- inj y;
			loop x y t
	in
	let adummy = dummy_node ()
	and bdummy = dummy_node ()
	in
	loop adummy bdummy lst;
	adummy.tl, bdummy.tl

let combine l1 l2 =
	let rec loop dst l1 l2 =
		match l1, l2 with
		| [], [] -> ()
		| h1 :: t1, h2 :: t2 -> 
			let r = { hd = h1, h2; tl = [] } in
			dst.tl <- inj r;
			loop r t1 t2
		| _, _ -> invalid_arg "combine: Different_list_size"
	in
	let dummy = dummy_node () in
	loop dummy l1 l2;
	dummy.tl

let sort ?(cmp=compare) = List.sort cmp

let rec init size f =
	if size = 0 then [] 
	else if size < 0 then invalid_arg "BatList.init"
	else
		let rec loop dst n =
			if n < size then
				let r = { hd = f n; tl = [] } in
				dst.tl <- inj r;
				loop r (n+1)
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
				let r = { hd = f n h; tl = [] } in
				dst.tl <- inj r;
				loop r (n+1) t
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
					let r = { hd =  h; tl = [] } in
					dst.tl <- inj r;
					loop (n-1) r t 
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
				let r = { hd = h; tl = [] } in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node () in
	loop dummy l;
	dummy.tl

let rec remove_if f lst =
	let rec loop dst = function
		| [] -> ()
		| x :: l ->
			if f x then
				dst.tl <- l
			else
				let r = { hd = x; tl = [] } in
				dst.tl <- inj r;
				loop r l
	in
	let dummy = dummy_node () in
	loop dummy lst;
	dummy.tl

let rec remove_all l x =
	let rec loop dst = function
		| [] -> ()
		| h :: t ->
			if x = h then
				loop dst t
			else
				let r = { hd = h; tl = [] } in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node () in
	loop dummy l;
	dummy.tl

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
	let h = dummy_node() in
	let _ = BatEnum.fold (fun acc x ->
		let r = { hd = x; tl = [] }  in
		acc.tl <- inj r;
		r) h e in
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
  | (a,b)::t when b = e -> a
  | _::t                -> aux t
  in aux l

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

let cartesian_product l1 l2 =
   List.concat (List.map (fun i -> List.map (fun j -> (i,j)) l2) l1)


let rec n_cartesian_product = function [] -> assert false
  | [l] -> List.map (fun i -> [i]) l
  | h :: t ->
      let rest = n_cartesian_product t in
      List.concat (List.map (fun i -> List.map (fun r -> i :: r) rest) h)



let print ?(first="[") ?(last="]") ?(sep="; ") print_a  out = function
  | []   ->
      BatInnerIO.nwrite out first;
      BatInnerIO.nwrite out last
  | [h]  ->
      BatInnerIO.Printf.fprintf out "%s%a%s" first print_a h last
  | h::t -> 
      BatInnerIO.nwrite out first;
      print_a out h;
      iter (BatInnerIO.Printf.fprintf out "%s%a" sep print_a) t;
      BatInnerIO.nwrite out last

let t_printer a_printer paren out x = print (a_printer false) out x

let sprint ?(first="[") ?(last="]") ?(sep="; ") print_a list =
  BatPrintf.sprintf2 "%a" (print ~first ~last ~sep print_a) list
(*  let os = BatInnerIO.output_string  () in
  print ~first ~last ~sep print_a os list;
  BatInnerIO.close_out os (* returns contents *)*)

let reduce f = function [] -> invalid_arg "Empty List"
  | h::t -> fold_left f h t

let min l = reduce Pervasives.min l
let max l = reduce Pervasives.max l
let sum l = reduce (+) l
let fsum l = reduce (+.) l

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
  let merge ~cmp         = merge cmp

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
