(* 
 * ExtSet - Extended operations on sets
 * Copyright (C) 1996 Xavier Leroy
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


module type OrderedType = BatInterfaces.OrderedType
(** Input signature of the functor {!Set.Make}. *)

module type S =
  sig
    type elt

    type t

    val empty: t

    val is_empty: t -> bool

    val mem: elt -> t -> bool

    val add: elt -> t -> t

    val singleton: elt -> t

    val remove: elt -> t -> t

    val union: t -> t -> t

    val inter: t -> t -> t

    val diff: t -> t -> t

    val compare: t -> t -> int

    val equal: t -> t -> bool

    val subset: t -> t -> bool

    val compare_subset: t -> t -> int

    val iter: (elt -> unit) -> t -> unit

    val map: (elt -> elt) -> t -> t

    val filter: (elt -> bool) -> t -> t

    val filter_map: (elt -> elt option) -> t -> t

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a

    val for_all: (elt -> bool) -> t -> bool

    val exists: (elt -> bool) -> t -> bool

    val partition: (elt -> bool) -> t -> t * t

    val cardinal: t -> int

    val elements: t -> elt list

    val min_elt: t -> elt

    val max_elt: t -> elt

    val choose: t -> elt

    val split: elt -> t -> t * bool * t

    val enum: t -> elt BatEnum.t

    val backwards: t -> elt BatEnum.t

    val of_enum: elt BatEnum.t -> t


    (** {6 Boilerplate code}*)

    (** {7 Printing}*)
      
    val print :  ?first:string -> ?last:string -> ?sep:string -> 
      ('a BatInnerIO.output -> elt -> unit) -> 
      'a BatInnerIO.output -> t -> unit

      (** {6 Override modules}*)
      
    (** Operations on {!Set} without exceptions.*)
    module Exceptionless : sig
      val min_elt: t -> elt option
      val max_elt: t -> elt option
      val choose:  t -> elt option
    end
      
      
    (** Operations on {!Set} with labels. *)
    module Labels : sig
      val iter : f:(elt -> unit) -> t -> unit
      val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
      val for_all : f:(elt -> bool) -> t -> bool
      val exists : f:(elt -> bool) -> t -> bool
      val map: f:(elt -> elt) -> t -> t
      val filter : f:(elt -> bool) -> t -> t
      val filter_map: f:(elt -> elt option) -> t -> t
      val partition : f:(elt -> bool) -> t -> t * t
    end
      
  end
    (** Output signature of the functor {!Set.Make}. *)

  module Make (Ord : OrderedType) = 
  struct
    include Set.Make(Ord)

    (*Breaking the abstraction*)

    type implementation = Empty | Node of implementation * elt * implementation * int
    external impl_of_t : t -> implementation = "%identity"
    external t_of_impl : implementation -> t = "%identity"

    open Printf
    (* s1 in s2 -> -1, s2 in s1 -> 1, neither a subset -> min_int, eq -> 0 *)
    let rec compare_subset s1 s2 =
      match (s1, impl_of_t s2) with
	  (Empty, Empty) -> 0
	| (Empty, t2) -> -1
	| (t1, Empty) -> 1
	| (Node(l1, v1, r1, _), t2) ->
            match split v1 (t_of_impl t2) with
		(l2, true, r2) -> (* v1 in both s1 and s2 *)
		  ( match compare_subset l1 l2, compare_subset r1 r2 with
		      | -1, -1 | -1, 0 | 0, -1 -> -1
		      | 0, 0 -> 0
		      | 1, 1 | 1, 0 | 0, 1 -> 1
		      | _ -> min_int
		  )
              | (l2, false, r2) -> (* v1 in s1, but not in s2 *)
		  if (compare_subset l1 l2) >= 0 && (compare_subset r1 r2) >= 0
		  then 1 else min_int

    let compare_subset s1 s2 = compare_subset (impl_of_t s1) s2

    type iter = E | C of elt * implementation * iter

    let rec cons_iter s t = match s with
        Empty -> t
      | Node (l, e, r, _) -> cons_iter l (C (e, r, t))

    let rec rev_cons_iter s t = match s with
        Empty -> t
      | Node (l, e, r, _) -> rev_cons_iter r (C (e, l, t))

    let rec enum_next l () = match !l with
        E -> raise BatEnum.No_more_elements
      | C (e, s, t) -> l := cons_iter s t; e

    let rec enum_backwards_next l () = match !l with
        E -> raise BatEnum.No_more_elements
      | C (e, s, t) -> l := rev_cons_iter s t; e

    let rec enum_count l () =
      let rec aux n = function
          E -> n
        | C (e, s, t) -> aux (n + 1 + cardinal (t_of_impl s)) t
      in aux 0 !l

    let enum t =
      let rec make l =
        let l = ref l in
        let clone() = make !l in
          BatEnum.make ~next:(enum_next l) ~count:(enum_count l) ~clone
      in make (cons_iter (impl_of_t t) E)

    let backwards t =
      let rec make l =
        let l = ref l in
        let clone() = make !l in
          BatEnum.make ~next:(enum_backwards_next l) ~count:(enum_count l) ~clone
      in make (rev_cons_iter (impl_of_t t) E)

    let of_enum e = 
      BatEnum.fold (fun acc elem -> add elem acc) empty e

    let map f e = fold (fun x acc -> add (f x) acc) e empty
	
    let filter f e = fold (fun x acc -> if f x then add x acc else acc) e empty

    let filter_map f e = fold (fun x acc -> match f x with Some v -> add v acc | _ -> acc) e empty

    let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_elt out t =
      BatEnum.print ~first ~last ~sep print_elt out (enum t)

	
    module Exceptionless =
    struct
      let min_elt t = try Some (min_elt t) with Not_found -> None
      let max_elt t = try Some (max_elt t) with Not_found -> None
      let choose  t = try Some (choose t)  with Not_found -> None
    end

    module Labels =
    struct
      let iter ~f t = iter f t
      let fold ~f t ~init = fold f t init
      let for_all ~f t    = for_all f t
      let exists ~f t     = exists f t
      let map    ~f t     = map f t
      let filter ~f t     = filter f t
      let filter_map ~f t = filter_map f t
      let partition ~f t  = partition f t
    end
  end


  module StringSet  = Make(String)
  module IStringSet = Make(BatString.IString)
  module NumStringSet = Make(BatString.NumString)
  module RopeSet    = Make(BatRope)
  module IRopeSet   = Make(BatRope.IRope)
  module IntSet     = Make(BatInt)
  module CharSet    = Make(Char)

(*
 * PMap - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 * Copyright (C)      2008 David Teller
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


type 'a t = ('a, unit) BatMap.t

type 'a enumerable = 'a t
type 'a mappable = 'a t

let empty    = BatMap.empty

let create   = BatMap.create

let singleton ?cmp x = BatMap.singleton ?cmp x ()

let is_empty = BatMap.is_empty

let mem      = BatMap.mem

let add e t  = BatMap.add e () t

let remove   = BatMap.remove

let for_map f = fun x _ -> f x

let iter f   = BatMap.iter (for_map f)

let fold f   = BatMap.foldi (for_map f)

let map f e  = BatMap.foldi (fun k _ acc -> add (f k) acc) e empty

let filter f = BatMap.filteri (for_map f)

let filter_map f e = BatMap.foldi (fun k _ acc -> match f k with None -> acc | Some v -> add v acc) e empty

let exists f t = BatReturn.label (fun label ->
			       iter (fun k -> if f k then BatReturn.return label true) t;
			       false)

let cardinal t =
  fold (fun _ acc -> acc + 1) t 0

let choose t = fst (BatMap.choose t)

let min_elt t = fst (BatMap.min_binding t)

let max_elt t = fst (BatMap.max_binding t)

let enum t =
  BatEnum.map (fun (k, _) -> k) (BatMap.enum t)

let of_enum t =
  BatEnum.fold (fun acc elem -> add elem acc) empty t

let of_enum_cmp ~cmp t =
  BatEnum.fold (fun acc elem -> add elem acc) (create cmp) t

let of_list l = List.fold_left (fun a x -> add x a) empty l

let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_elt out t =
  BatEnum.print ~first ~last ~sep print_elt out (enum t)

let for_all f t = BatMap.for_all (fun k _ -> f k) t

let partition f t = BatMap.partition (fun k _ -> f k) t

let filter f t = BatMap.filteri (fun k _ -> f k) t

let pop t = let (k, _), m = BatMap.pop t in k, m

let union m n = BatMap.union m n

let diff m n = BatMap.diff m n

let intersect m n = BatMap.intersect (fun _ x -> x) m n

