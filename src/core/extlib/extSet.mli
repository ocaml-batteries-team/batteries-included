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

(** Sets over ordered types.

   This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance.

    @author Xavier Leroy (Base module)
    @author David Teller

    @documents Set
*)
module Set:
sig

module type OrderedType = Interfaces.OrderedType
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

    val diff: t -> t -> t
    (** Set difference. *)

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

    val map: (elt -> elt) -> t -> t
      (** [map f x] creates a new set with elements [f a0],
	  [f a1]... [f an], where [a1], ..., [an] are the
	  values contained in [x]*)

    val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. *)

    val filter_map: (elt -> elt option) -> t -> t
      (** [filter_map f m] combines the features of [filter] and
	  [map].  It calls calls [f a0], [f a1], [f an] where [a0..an]
	  are the elements of [m] and returns the set of pairs [bi]
	  such as [f ai = Some bi] (when [f] returns [None], the
	  corresponding element of [m] is discarded). *)

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
      (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
	  where [x1 ... xN] are the elements of [s], in increasing order. *)

    val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

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
       (with respect to the [Ord.compare] ordering).

	@raise Not_found if the set is empty. *)

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

    val backwards: t -> elt Enum.t
      (** Return an enumeration of all elements of the given set.
	  The returned enumeration is sorted in decreasing order with respect
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


      (** {6 Override modules}*)

    (**
       The following modules replace functions defined in {!Set} with functions
       behaving slightly differently but having the same name. This is by design:
       the functions meant to override the corresponding functions of {!Set}.
       
       To take advantage of these overrides, you probably want to
       {{:../extensions.html#multiopen}{open several modules in one
       operation}} or {{:../extensions.html#multialias}{alias several
       modules to one name}}. For instance, to open a version of {!Set}
       with exceptionless error management, you may write {v open Set,
       ExceptionLess v}. To locally replace module {!Set} with a module of
       the same name but with exceptionless error management, you may
       write {v module Set = Set include ExceptionLess v}.
       
    *)
      
    (** Operations on {!Set} without exceptions.*)
    module ExceptionLess : sig
      val min_elt: t -> elt option
      val max_elt: t -> elt option
      val choose:  t -> elt option
    end
      
      
    (** Operations on {!Set} with labels.
	
	This module overrides a number of functions of {!Set} by
	functions in which some arguments require labels. These labels are
	there to improve readability and safety and to let you change the
	order of arguments to functions. In every case, the behavior of the
	function is identical to that of the corresponding function of {!Set}.
    *)
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

module Make (Ord : OrderedType) : S with type elt = Ord.t
(** Functor building an implementation of the set structure
   given a totally ordered type. 

    @documents Set.Make
*)
end
