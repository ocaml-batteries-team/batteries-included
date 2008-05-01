(* 
 * LazyListLabels - lazily-computed lists
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
(**  Lazy lists of elements.

     Lazy lists are similar to lists, with the exception that their contents are
     only computed whenever requested. This makes them particularly useful in
     contexts where streams of data are to be handled. 

     {b Note} Using the companion syntax extension for lazy lists is strongly suggested.

     {b Note} Enumerations (as featured in module [Enum]) and lazy lists (as featured
     in this module) are quite similar in purpose. Lazy lists are slightly higher level,
     insofar as no cloning is required to get them to work, which makes them slightly
     more useful in contexts where backtracking is common. Enumerations, on the other
     hand, are closer to traditional stream processing, require a bit more low-level
     marking whenever backtracking is required, but may be faster and more memory-efficient
     when used properly. Either choice is recommended over OCaml's [Stream].
*)

exception No_more_elements

exception Empty_list
  (** [Empty_list] is raised when an operation applied on an empty list
      is invalid : [hd] for example. *)
  
exception Invalid_index of int
  (** [Invalid_index] is raised when an indexed access on a list is
      out of list bounds. *)
  
exception Different_list_size of string
  (** [Different_list_size] is raised when applying functions such as
      [iter2] on two lists having different size. *)

  
(**
   {6  Lazyness}
*)
(**
   The type of an item in the list.
*)
type 'a node_t =
  | Nil 
  | Cons of 'a * 'a t
and 
(**
   The type of a lazy list.
*)
  'a t =
  ('a node_t) Lazy.t

(**
   The empty list.
*)
let nil = lazy Nil

let next l = Lazy.force l

(**
   {6 Constructors}
*)
let from_while f =
  let rec aux = lazy (
    match f () with
      | None   -> Nil
      | Some x -> Cons (x, aux) ) in aux

let from f = 
  let f' () =
    try  Some (f ())
    with No_more_elements -> None
  in from_while f'

let from_loop_while (data:'b) (next: 'b -> ('a * 'b) option) =
  let rec aux data =
    match next data with
      | Some(a,b) -> lazy (Cons(a, aux b))
      | None      -> nil
  in aux data


let from_loop (data:'b) (next:'b -> ('a * 'b)) : 'a t=
  let f' data =
    try  Some (next data)
    with No_more_elements -> None
  in from_loop_while data f'

let init n f = 
  let rec aux i = 
    if i < n then lazy (Cons (f i, aux ( i + 1 ) ) )  
    else          nil
  in if n < 0 then raise (Invalid_argument "LazyList.init")
     else          aux 0

let make n x =
  let rec aux i =
    if i < n then lazy (Cons (x, aux ( i + 1 ) ) )
    else          nil
  in if n < 0 then raise (Invalid_argument "LazyList.make")
     else          aux 0

(**
   {6  Iterators}
*)
(**
   Eeager iteration

   [LazyList.iter f [^ a1; ...; an ^]] applies function [f] in turn to [a1; ...; an]. 
   It is equivalent to [begin f a1; f a2; ...; f an; () end]. In particular, it
   causes all the elements of the list to be evaluated.
*)
let iter f l =
  let rec aux l =
    match next l with | Cons (x, t) -> (f x; aux t) | _ -> ()
  in aux l
  
(**
   Eeager iteration, with indices

   [LazyList.iteri f [^ a1; ...; an ^]] applies function [f] in turn to [a1 0; ...; an (n - 1)]. 
   It is equivalent to [begin f a1 0; f a2 0; ...; f an (n-1); () end]. In particular, it
   causes all the elements of the list to be evaluated.
*)
let iteri f l =
  let rec aux i l =
    match next l with | Cons (x, t) -> (f i x; aux (i + 1) t) | _ -> ()
  in aux 0 l
  
(**
   Lazy map

   [LazyList.map f [^ a1; ...; an ^]] applies function [f] to [a1, ..., an], and builds the list 
   [^ f a1; ...; f an ^]  with the results returned by [f]. Not tail-recursive. Evaluations
   of [f] take place only when the contents of the list are forced.
*)
let map f l =
  let rec (aux : 'a t -> 'b t) =
    fun rest ->
      match next rest with
      | Cons (x, (t : 'a t)) -> lazy (Cons (f x, aux t))
      | Nil -> lazy Nil
  in aux l
  
let mapi f l =
  let rec aux rest i =
    match next rest with
      | Cons (x, (t : 'a t)) -> lazy (Cons (f i x, aux t ( i + 1 ) ))
      | Nil -> lazy Nil
  in aux l 0

let may_find p l =
  let rec aux l =
    match next l with
      | Nil         -> None
      | Cons (x, t) -> if p x then Some x else aux t
  in aux l 

let may_rfind p l =
  let rec aux l acc =
    match next l with
      | Nil         -> acc
      | Cons (x, t) -> aux t (if p x then Some x else acc)
  in aux l None

let may_findi p l =
  let rec aux l i =
    match next l with
      | Nil                    -> None
      | Cons (x, _) when p i x -> Some (i, x)
      | Cons (_, t)            -> aux t (i+1)
  in aux l 0

let may_rfindi p l =
  let rec aux l acc i =
    match next l with
      | Nil         -> acc
      | Cons (x, t) -> aux t (if p i x then Some (i, x) else acc) (i+1)
  in aux l None 0

let find_exn p e l =
  Option.get_exn (may_find p l) e

let rfind_exn p e l =
  Option.get_exn (may_rfind p l) e

let find  p l = find_exn p Not_found l

let rfind p l = rfind_exn p Not_found l

let findi p l =
  Option.get_exn (may_findi p l) Not_found

let rfindi p l =
  Option.get_exn (may_rfindi p l) Not_found

let index_of e l =
  match may_findi (fun _ x -> e = x) l with
    | None        -> None
    | Some (i, _) -> Some i

let rindex_of e l =
  match may_rfindi (fun _ x -> e = x) l with
    | None        -> None
    | Some (i, _) -> Some i

let index_ofq e l =
  match may_findi (fun _ x -> e == x) l with
    | None        -> None
    | Some (i, _) -> Some i

let rindex_ofq e l =
  match may_rfindi (fun _ x -> e == x) l with
    | None        -> None
    | Some (i, _) -> Some i
(**
   Eager fold_left

   [LazyList.fold_left f a [^ b1; ...; bn ^]] is [f (... (f (f a b1) b2) ...) bn]. This causes
   evaluation of all the elements of the list.
*)
let fold_left f init l =
  let rec aux acc rest =
    match next rest with 
      | Cons (x, t) -> aux (f acc x) t 
      | _           -> acc
  in aux init l
  

(**
   Eager fold_right

   [LazyList.fold_left f a [^ b1; ...; bn ^]] is [f ( f (... (f (f a bn) ...) b2) b1]. This causes
   evaluation of all the elements of the list. Not tail-recursive.
*)
let fold_right f init l =
  let rec aux rest =
    match next rest with | Cons (x, t) -> f x (aux t) | _ -> init
  in aux l
  
(**
   {6  Common functions}
*)
(**
   Return the length (number of elements) of the given list.

   Causes the evaluation of all the elements of the list.
*)
let length l = fold_left (fun n _ -> n + 1) 0 l
  
(**
   Return the first element of the given list. Raise [Empty_list] if the list is empty.
*)
let hd list =
  match next list with
  | Cons (x, _) -> x
  | _ -> raise Empty_list
  
let first = hd

let last l =
  let rec aux acc l = match next l with
    | Nil        -> acc
    | Cons(x, t) -> aux (Some x) t
  in match aux None l with
    | None   -> raise Empty_list
    | Some x -> x

(**
   Return the given list without its first element. Raise [Empty_list] if the list is empty.
*)
let tl list =
  match next list with
  | Cons (_, t) -> t
  | _ -> raise Empty_list
  
(**
   Return the nth element of the list. The first element is numbered 0.
   [nth l i] raises [LazyListFailure "nth"] if [i < 0] or [i >= length l].
*)
let nth list n =
  let rec aux list i =
    match ((next list), i) with
    | (Cons (x, _), 0) -> x
    | (Cons (_, t), _) -> aux t (i - 1)
    | _ -> raise (Invalid_index n)
  in if n < 0 then raise (Invalid_index n) else aux list n
  
let at list n =
  let rec aux list i =
    match ((next list), i) with
    | (Cons (x, _), 0) -> x
    | (Cons (_, t), _) -> aux t (i - 1)
    | _ -> raise (Invalid_index n)
  in if n < 0 then raise (Invalid_index n) else aux list n

(**
   Eager list reversal.
*)
let rev list = fold_right (fun x acc -> lazy (Cons (x, acc))) nil list
  
(** Utility function.*)
let rev_of_list (list:'a list) = List.fold_right (fun x acc -> lazy (Cons (x, acc))) list nil

(**
   Evaluate a list and append another list after this one.

   Cost is linear in the length of the first list, not tail-recursive.
*)
let eager_append (l1 : 'a t) (l2 : 'a t) =
  let rec aux list =
    match next list with
    | Cons (x, t) -> lazy (Cons (x, aux t))
    | _ -> l2
  in aux l1
  
(**
   Eager reverse-and-append

   Cost is linear in the length of the first list, tail-recursive.
*)
let rev_append (l1 : 'a t) (l2 : 'a t) =
  let rec aux list acc =
    match next list with
    | Cons (x, t) -> aux t (lazy (Cons (x, acc)))
    | _ -> acc
  in aux l1 l2
  
let rev_append_of_list (l1 : 'a list) (l2 : 'a t) : 'a t =
  let rec aux list acc = match list with
    | []   -> acc
    | h::t -> aux t (lazy (Cons (h, acc)))
  in aux l1 l2
(**
   Lazy append

   Cost is constant. All evaluation is delayed until the contents
   of the list are actually read. Reading itself is delayed by
   a constant factor.
*)
let append (l1 : 'a t) (l2 : 'a t) =
  let rec (aux : 'a t -> 'a t) =
    fun rest_of_l1 ->
      match next rest_of_l1 with
      | Cons (x, (t : 'a t)) -> lazy (Cons (x, aux t))
      | _ -> l2
  in aux l1
  
let ( ^@^ ) = append
  
let flatten (lol : ('a t) list) =
  ListLabels.fold_left ~init: nil ~f: append lol
  
let concat  (lol : ('a t) t) = fold_left append nil lol
  
(**
   {6  Conversions}
*)
(**
   Eager conversion to string.
*)
let to_list l = fold_right (fun x acc -> x :: acc) [] l
  
(**
   Lazy conversion to stream.
*)
let to_stream l =
  let rec aux rest =
    match next rest with
    | Cons (x, t) -> Stream.icons x (Stream.slazy (fun _ -> aux t))
    | _ -> Stream.sempty
  in aux l
  
(**
   Eager conversion to array.
*)
let to_array l = Array.of_list (to_list l)

let enum l =
  let reference = ref l in
    Enum.from (fun () -> match next !reference with
		 | Cons(x,t) -> reference := t; x
		 | _         -> raise Enum.No_more_elements )
  
(**
   Lazy conversion from lists

   Albeit slower than eager conversion, this is the default mechanism for converting from regular 
   lists to lazy lists.  This for two reasons :
   * if you're using lazy lists, total speed probably isn't as much an issue as start-up speed
   * this will let you convert regular infinite lists to lazy lists.
*)
let of_list l =
  let rec aux rest =
    match rest with | [] -> nil | h :: t -> lazy (Cons (h, aux t))
  in aux l
  
(**
   Lazy conversion from stream.
*)
let of_stream s =
  let rec aux s =
    let (__strm : _ Stream.t) = s
    in
      match Stream.peek __strm with
      | Some h -> (Stream.junk __strm; lazy (Cons (h, aux s)))
      | _ -> nil
  in aux s
  
(**
   Eager conversion from lists
*)
let eager_of_list l =
  ListLabels.fold_right ~init: nil ~f: (fun x acc -> lazy (Cons (x, acc))) l
  
(**
   Lazy conversion from array
*)
let of_array l =
  ArrayLabels.fold_right ~init: nil ~f: (fun x acc -> lazy (Cons (x, acc))) l

(**
   Lazy conversion from enum
*)
let of_enum e =
  let rec aux () =
    match Enum.get e with
      |	Some x -> lazy (Cons (x, aux () ) )
      | _      -> nil
  in
    aux ()
  
(**
   {6  Predicates}
*)
(**
   Lazy filtering.

   [filter p l] returns all the elements of the list [l]  that satisfy the predicate [p]. 
   The order of the elements in the input list is preserved.
*)
let filter f l =
  let rec aux rest =
    match next rest with
    | Cons (x, t) when f x -> lazy (Cons (x, aux t))
    | Cons (_, t) -> aux t
    | Nil -> nil
  in aux l
  
(**
   Eager existential.

   [exists p [^ a1; ...; an ^]] checks if at least one element of the list satisfies the predicate [p]. 
   That is, it returns [ (p a1) || (p a2) || ... || (p an) ].
*)
let exists f l =
  let rec aux rest =
    match next rest with
    | Cons (x, t) when f x -> true
    | Cons (_, t) -> aux t
    | Nil -> false
  in aux l
  
(**
   Eager universal.

   [for_all p [^ a1; ...; an ^]] checks if all elements of the list satisfy the predicate [p]. 
   That is, it returns [(p a1) && (p a2) && ... && (p an)].
*)
let for_all f l =
  let rec aux rest =
    match next rest with
    | Cons (x, t) when f x -> aux t
    | Cons (_, t) -> false
    | Nil -> true
  in aux l
  

let range a b =
  let rec increasing lo hi =
    if lo > hi then nil else lazy (Cons (lo, increasing (lo + 1) hi))
  in
    (*      and     decreasing lo hi = if lo > hi then 
	nil
      else 
	lazy (Cons hi (decreasing lo (hi - 1)))*)
    if b >= a then increasing a b else (*decreasing b a*) nil

let split_at n l =
  let rec aux acc l i =
    if i = 0 then (rev_of_list acc, l)
    else match next l with
      | Nil        -> raise (Invalid_index n)
      | Cons(h, t) -> aux (h::acc) t (i - 1)
  in aux [] l n

let split_nth = split_at

let mem   e = exists (( = ) e)
 
let memq  e = exists (( == ) e )

let assoc e l = snd (find (fun (a,_) -> a = e) l)

let assq  e l = snd (find (fun (a,_) -> a == e) l)

let mem_assoc e l = Option.is_some (may_find (fun (a, _) -> a = e) l)

let mem_assq e l = Option.is_some (may_find (fun (a, _) -> a == e) l)

let filter_map f l =
  let rec aux rest =
    match next rest with
    | Cons (h, t) ->
        (match f h with | None -> aux t | Some x -> lazy (Cons (x, aux t)))
    | Nil -> nil
  in aux l

let unique ?(cmp = compare) l =
  let set      = ref (PMap.create cmp) in
  let filter x = if PMap.mem x !set then None
                 else                  ( set := PMap.add x true !set; Some x )
  in
    filter_map filter l

let remove_all x l =
  filter_map (fun y -> if x = y then Some x else None) l

let remove_if p l =
  let rec aux acc l = match next l with
    | Nil                -> rev_of_list acc
    | Cons(h,t) when p h -> rev_append_of_list acc t
    | Cons(h,t)          -> aux (h::acc) t
  in aux [] l

let remove x l =
  remove_if ( ( = ) x ) l


(** An infinite list of nothing *)
let rec eternity = lazy (Cons ((), eternity))
  
let take n l = fst (split_at n l)

let drop n l = 
  let rec aux l i =
    if i = 0 then l
    else match next l with
      | Nil        -> raise (Invalid_index n)
      | Cons(_, t) -> aux t (i - 1)
  in aux l n

let drop_while p =
  let rec aux l = match next l with
    | Nil                -> nil
    | Cons(h,t) when p h -> aux t
    | Cons(_,t)          -> t
  in aux

let take_while p =
  let rec aux acc l = match next l with
    | Cons(h,t) when p h -> aux (h::acc) t
    | _                  -> rev_of_list acc
  in aux []

let sort ?cmp l = of_list (ExtList.List.sort ?cmp (to_list l))

let stable_sort cmp l = of_list (ExtList.List.stable_sort cmp (to_list l))

let map2 f l1 l2 =
  let rec aux l1 l2 =
      match (next l1, next l2) with
	| (Cons (h1, t1), Cons(h2, t2)) -> lazy (Cons (f h1 h2, aux t1 t2))
	| (Nil, Nil)                    -> nil
	| _                             -> raise (Different_list_size "LazyList.map2")
  in aux l1 l2

let iter2 f l1 l2 =
  let rec aux l1 l2 =
      match (next l1, next l2) with
	| (Cons (h1, t1), Cons(h2, t2)) -> f h1 h2; aux t1 t2
	| (Nil, Nil)                    -> ()
	| _                             -> raise (Different_list_size "LazyList.iter2")
  in aux l1 l2

let fold_left2 f acc l1 l2 = 
  let rec aux acc l1 l2 = 
      match (next l1, next l2) with
	| (Cons (h1, t1), Cons(h2, t2)) -> aux (f acc h1 h2) t1 t2
	| (Nil, Nil)                    -> acc
	| _                             -> raise (Different_list_size "LazyList.fold_left2")
  in aux acc l1 l2

let fold_right2 f l1 l2 acc =
  let rec aux l1 l2 =
      match (next l1, next l2) with
	| (Cons (h1, t1), Cons(h2, t2)) -> f h1 h2 (aux t1 t2)
	| (Nil, Nil)                    -> acc
	| _                             -> raise (Different_list_size "LazyList.fold_right2")
  in aux l1 l2

let for_all2 p l1 l2 =
  let rec aux l1 l2 =
      match (next l1, next l2) with
	| (Cons (h1, t1), Cons(h2, t2)) -> p h1 h2 && (aux t1 t2)
	| (Nil, Nil)                    -> true
	| _                             -> raise (Different_list_size "LazyList.for_all2")
  in aux l1 l2

let exists2 p l1 l2 =
  let rec aux l1 l2 =
      match (next l1, next l2) with
	| (Cons (h1, t1), Cons(h2, t2)) -> p h1 h2 || (aux t1 t2)
	| (Nil, Nil)                    -> false
	| _                             -> raise (Different_list_size "LazyList.exists2")
  in aux l1 l2

let combine l1 l2 =
  let rec aux l1 l2 = match (next l1, next l2) with
    | (Cons(h1, t1), Cons(h2, t2)) -> lazy (Cons ((h1, h2), ( aux t1 t2 )))
    | (Nil,          Nil         ) -> nil
    | _                            -> raise (Different_list_size "LazyList.combine")
  in aux l1 l2

module ExceptionLess = struct
  (** Exceptionless counterparts for error-raising operations*)

  let find   = may_find
  let rfind  = may_rfind
  let findi  = may_findi
  let rfindi = may_rfindi
    
  let at list n =
    let rec aux list i =
      match (next list, i) with
	| (Cons (x, _), 0) -> Std.Ok x
	| (Cons (_, t), _) -> aux t (i - 1)
	| _ -> Std.Error (`Invalid_index n)
    in if n < 0 then Std.Error (`Invalid_index n) else aux list n

  let assoc a (l:'a t) =
    try  Some (assoc a l)
    with Not_found -> None

  let assq a l =
    try  Some (assq a l)
    with Not_found -> None

  let split_at n l =
    try  Std.Ok (split_at n l)
    with Not_found -> Std.Error (`Invalid_index n)
end
