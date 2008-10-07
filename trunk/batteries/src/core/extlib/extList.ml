(*
 * ExtList - additional and modified functions for lists.
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 Red Hat Inc.
 * Copyright (C) 2008 David Teller
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

open Sexplib
TYPE_CONV_PATH "Batteries.Data.Persistent" (*For Sexplib, Bin-prot...*)

module List = struct

exception Empty_list
exception Invalid_index of int
exception Different_list_size of string

include List

(* Thanks to Jacques Garrigue for suggesting the following structure *)
type 'a mut_list =  {
	hd: 'a; 
	mutable tl: 'a list
}
type 'a t = 'a list
external inj : 'a mut_list -> 'a list = "%identity"


let dummy_node () = { hd = Obj.magic (); tl = [] }

let cons h t = h::t

let is_empty = function
  | [] -> true
  | _  -> false

let hd = function
	| [] -> raise Empty_list
	| h :: t -> h

let tl = function
	| [] -> raise Empty_list
	| h :: t -> t

let nth l index =
	if index < 0 then raise (Invalid_index index);
	let rec loop n = function
		| [] -> raise (Invalid_index index);
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
			| _ -> raise (Different_list_size "map2")
	in
	let dummy = dummy_node () in
	loop dummy l1 l2;
	dummy.tl

let rec iter2 f l1 l2 =
	match l1, l2 with
	| [], [] -> ()
	| h1 :: t1, h2 :: t2 -> f h1 h2; iter2 f t1 t2
	| _ -> raise (Different_list_size "iter2")

let rec fold_left2 f accum l1 l2 =
	match l1, l2 with
	| [], [] -> accum
	| h1 :: t1, h2 :: t2 -> fold_left2 f (f accum h1 h2) t1 t2
	| _ -> raise (Different_list_size "fold_left2")

let fold_right2 f l1 l2 init =
	let rec tail_loop acc l1 l2 =
		match l1, l2 with
		| [] , [] -> acc
		| h1 :: t1 , h2 :: t2 -> tail_loop (f h1 h2 acc) t1 t2
		| _ -> raise (Different_list_size "fold_right2")
	in
	let rec loop n l1 l2 =
		match l1, l2 with
		| [], [] -> init
		| h1 :: t1, h2 :: t2 ->
			if n < fold_right_max then
				f h1 h2 (loop (n+1) t1 t2)
			else
				f h1 h2 (tail_loop init (rev t1) (rev t2))
		| _ -> raise (Different_list_size "fold_right2")
	in
	loop 0 l1 l2

let for_all2 p l1 l2 =
	let rec loop l1 l2 =
		match l1, l2 with
		| [], [] -> true
		| h1 :: t1, h2 :: t2 -> if p h1 h2 then loop t1 t2 else false
		| _ -> raise (Different_list_size "for_all2")
	in
	loop l1 l2

let exists2 p l1 l2 =
	let rec loop l1 l2 =
		match l1, l2 with
			| [], [] -> false
			| h1 :: t1, h2 :: t2 -> if p h1 h2 then true else loop t1 t2
			| _ -> raise (Different_list_size "exists2")
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
		| _, _ -> raise (Different_list_size "combine")
	in
	let dummy = dummy_node () in
	loop dummy l1 l2;
	dummy.tl

let sort ?(cmp=compare) = List.sort cmp

let rec init size f =
	if size = 0 then [] 
	else if size < 0 then invalid_arg "ExtList.init"
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

(* make by Richard W.M. Jones. *)
let make i x =
  if i < 0 then invalid_arg "ExtList.List.make";
  let rec make' x = function
    | 0 -> []
    | i -> x :: make' x (i-1)
  in
  make' x i

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
	| [] -> raise Empty_list
	| h :: [] -> h
	| _ :: t -> last t

let split_nth index = function
	| [] -> if index = 0 then [],[] else raise (Invalid_index index)
	| (h :: t as l) ->
		if index = 0 then [],l
		else if index < 0 then raise (Invalid_index index)
		else
			let rec loop n dst l =
				if n = 0 then l else
				match l with
				| [] -> raise (Invalid_index index)
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
		Enum.make
			~next:(fun () ->
				match !lr with
				| [] -> raise Enum.No_more_elements
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
	let _ = Enum.fold (fun x acc ->
		let r = { hd = x; tl = [] }  in
		acc.tl <- inj r;
		r) h e in
	h.tl

let backwards l = enum (rev l)

let of_backwards e =
  let rec aux acc = match Enum.get e with
    | Some h -> aux (h::acc)
    | None   -> acc
  in aux []

let assoc_inv e l =
  let rec aux = function
  | []                  -> raise Not_found
  | (a,b)::t when b = e -> a
  | _::t                -> aux t
  in aux l

let sexp_of_t = Conv.sexp_of_list
let t_of_sexp = Conv.list_of_sexp

module ExceptionLess = struct
  let rfind p l =
    try  Some (rfind p l)
    with Not_found -> None

  let findi p l =
    try  Some (findi p l)
    with Not_found -> None
      
  let split_at n l =
    try   `Ok (split_at n l)
    with  Invalid_index i -> `Invalid_index i

  let at n l =
    try `Ok (at n l)
    with Invalid_index i -> `Invalid_index i

  let assoc e l =
    try Some (assoc e l)
    with Not_found -> None

  let assq e l =
    try Some (assq e l)
    with Not_found -> None

  let assoc_inv e l =
    try Some (assoc_inv e l)
    with Not_found -> None


end

end

module ListLabels = struct
  exception Empty_list          = List.Empty_list
  exception Invalid_index       = List.Invalid_index
  exception Different_list_size = List.Different_list_size

  let init i ~f     = List.init i f
  let make n  x     = List.make n x
  let iteri ~f l    = List.iteri f l
  let map ~f l      = List.map f l
  let mapi ~f l     = List.mapi f l
  let rfind ~f l    = List.rfind f l
  let find ~f l     = List.find f l
  let findi ~f      = List.findi f
  let find_exn ~f   = List.find_exn f 
  let filter_map ~f = List.filter_map f
  let remove_if ~f  = List.remove_if f
  let take_while ~f = List.take_while f
  let drop_while ~f = List.drop_while f
  let map2 ~f       = List.map2 f
  let iter2 ~f      = List.iter2 f
  let exists2 ~f    = List.exists2 f
  let fold_left ~f ~init         = List.fold_left f init
  let fold_right ~f l ~init      = List.fold_right f l init
  let fold_left2  ~f ~init       = List.fold_left2 f init
  let fold_right2 ~f l1 l2 ~init = List.fold_right2 f l1 l2 init
  let filter ~f     = List.filter f
  let find_all ~f   = List.find_all f
  let partition ~f  = List.partition f
  let rev_map ~f    = List.rev_map f
  let rev_map2 ~f   = List.rev_map2 f
  let iter ~f       = List.iter f
  let for_all ~f    = List.for_all f
  let for_all2 ~f   = List.for_all2 f
  let exists ~f     = List.exists f
  let stable_sort ?(cmp=compare)  = List.stable_sort cmp
  let fast_sort ?(cmp=compare)    = List.fast_sort cmp
  let merge ~cmp         = List.merge cmp
  let make_compare  = List.make_compare

  let mem           = List.mem
  let memq          = List.memq
  let assoc         = List.assoc
  let assoc_inv     = List.assoc_inv
  let assq          = List.assq
  let mem_assoc     = List.mem_assoc
  let mem_assq      = List.mem_assq


  let takewhile     = take_while
  let dropwhile     = drop_while
  let interleave    = List.interleave
  let combine       = List.combine
  let append        = List.append
  let concat        = List.concat
  let flatten       = List.flatten
  let remove_assoc  = List.remove_assoc
  let remove_assq   = List.remove_assq
  let split         = List.split
  let length        = List.length
  let rev_append    = List.rev_append
  let rev           = List.rev

  let enum          = List.enum
  let of_enum       = List.of_enum
  let backwards     = List.backwards
  let of_backwards  = List.of_backwards
  let hd            = List.hd
  let tl            = List.tl
  let nth           = List.nth
  let sort          = List.sort
  let index_of      = List.index_of
  let index_ofq     = List.index_ofq
  let rindex_of     = List.rindex_of
  let rindex_ofq    = List.rindex_ofq
  let unique        = List.unique
  let split_at      = List.split_at
  let split_nth     = List.split_nth
  let remove        = List.remove
  let remove_all    = List.remove_all
  let take          = List.take
  let drop          = List.drop
  let is_empty      = List.is_empty
  let cons          = List.cons
  let first         = List.first
  let last          = List.last
  let at            = List.at

  let sexp_of_t = Conv.sexp_of_list
  let t_of_sexp = Conv.list_of_sexp
  module ExceptionLess = struct
    let rfind ~f    = List.ExceptionLess.rfind f
    let findi ~f    = List.ExceptionLess.findi f
    let split_at    = List.ExceptionLess.split_at
    let at          = List.ExceptionLess.at
    let assoc       = List.ExceptionLess.assoc
    let assoc_inv   = List.ExceptionLess.assoc_inv
    let assq        = List.ExceptionLess.assq
  end
end

let ( @ ) = List.append
