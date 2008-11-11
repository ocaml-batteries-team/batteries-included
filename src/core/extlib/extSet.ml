(* 
 * ExtSet - Extended operations on sets
 * Copyright (C) 1996 Xavier Leroy
 *               2008 David Teller, LIFO, Universite d'Orleans
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

module Set =
struct
  module type OrderedType =
  sig
    type t
      (** The type of the set elements. *)
    val compare : t -> t -> int
      (** A total ordering function over the set elements.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the elements [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. *)
  end
(** Input signature of the functor {!Set.Make}. *)

module type S =
  sig
    type elt
    (** The type of the set elements. *)

    type t
    (** The type of sets. *)

    val empty: t
    (** The empty set. *)

    val is_empty: t -> bool
    (** Test whether a set is empty or not. *)

    val mem: elt -> t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

    val add: elt -> t -> t
    (** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

    val singleton: elt -> t
    (** [singleton x] returns the one-element set containing only [x]. *)

    val remove: elt -> t -> t
    (** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged. *)

    val union: t -> t -> t
    (** Set union. *)

    val inter: t -> t -> t
    (** Set intersection. *)

    (** Set difference. *)
    val diff: t -> t -> t

    val compare: t -> t -> int
    (** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

    val equal: t -> t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)

    val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

    val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
       The elements of [s] are presented to [f] in increasing order
       with respect to the ordering over the type of the elements. *)

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s], in increasing order. *)

    val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

    val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. *)

    val partition: (elt -> bool) -> t -> t * t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

    val cardinal: t -> int
    (** Return the number of elements of a set. *)

    val elements: t -> elt list
    (** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

    val min_elt: t -> elt
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the set is empty. *)

    val max_elt: t -> elt
    (** Same as {!Set.S.min_elt}, but returns the largest element of the
       given set. *)

    val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. *)

    val split: elt -> t -> t * bool * t
    (** [split x s] returns a triple [(l, present, r)], where
          [l] is the set of elements of [s] that are
          strictly less than [x];
          [r] is the set of elements of [s] that are
          strictly greater than [x];
          [present] is [false] if [s] contains no element equal to [x],
          or [true] if [s] contains an element equal to [x]. *)

    val enum: t -> elt Enum.t
      (** Return an enumeration of all elements of the given set.
	  The returned enumeration is sorted in increasing order with respect
	  to the ordering [Ord.compare], where [Ord] is the argument
	  given to {!Set.Make}. *)

    val of_enum: elt Enum.t -> t


    (** {6 Boilerplate code}*)
    (** {7 S-Expressions}*)
      
    val t_of_sexp : (Sexplib.Sexp.t -> elt) -> Sexplib.Sexp.t -> t
    val sexp_of_t : (elt -> Sexplib.Sexp.t) -> t -> Sexplib.Sexp.t


    (** {7 Printing}*)
      
    val print :  ?first:string -> ?last:string -> ?sep:string -> 
      ('a InnerIO.output -> elt -> unit) -> 
      'a InnerIO.output -> t -> unit

  end
    (** Output signature of the functor {!Set.Make}. *)

  module Make (Ord : OrderedType) = 
  struct
    include Set.Make(Ord)

    (*Breaking the abstraction*)

    type implementation = Empty | Node of implementation * elt * implementation * int
    external impl_of_t : t -> implementation = "%identity"
    external t_of_impl : implementation -> t = "%identity"

    let enum t =
      let queue = Queue.create () in
      let rec next () = 
	match 
	  try  Some (Queue.pop queue) 
	  with Queue.Empty -> None
	with None       -> raise Enum.No_more_elements
	  |  Some Empty -> next ()
	  | Some Node (l, e, r, _) -> 
	      Queue.push l queue;
	      Queue.push r queue;
	      e
      in Queue.add (impl_of_t t) queue;
	Enum.from next

    let of_enum e = 
      Enum.fold add empty e
	
    let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_elt out t =
      Enum.print ~first ~last ~sep print_elt out (enum t)

	
    (*The following is a hack to make sure that we're letting sexplib
      do the conversion automatically, hence that we're following sexplib protocol.*)
	
    let t_of_sexp elt_of_sexp(*added implicitly by sexplib*) sexp = 
      let module Local =
	  struct
	    let sexp_of_elt _ = assert false
	      
	    type t = implementation = 
			| Empty
			| Node of t * elt * t * int
	    with sexp
		
	    let result = t_of_sexp sexp
	  end in t_of_impl (Local.result)
	    
    let sexp_of_t sexp_of_elt(*added implicitly by sexplib*) t = 
      let module Local =
	  struct
	    let elt_of_sexp _ = assert false
	      
	    type t = implementation = 
			| Empty
			| Node of t * elt * t * int
	    with sexp
		
	    let result = sexp_of_t (impl_of_t t)
	  end in Local.result


  end

end
