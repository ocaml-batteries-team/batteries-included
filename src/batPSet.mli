(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 * Copyright (C)      2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

(**
   Polymorphic sets of elements.

   This module defines a type of sets, a functional representation of
   sets of elements. The base operations are adding an element to a
   set or removing an element from a set. This implementation is
   functional insofar as the act of adding or substracting an element
   to/from a set does not modify the existing set, rather producing a
   new set.  The implementation uses balanced binary trees, and is
   therefore reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance.

   {b Note} OCaml, Batteries Included, provides two implementations of
   sets: polymorphic sets (this module) and functorized sets (module
   {!Set}). Module {!Set} offers a more complex and slightly poorer
   set of features but stronger type-safety. Module {!PSet} is easier
   to use and has a few more powerful features but makes it easier to
   shoot yourself in the foot. In case of doubt, use {!Set}.


   @author Xavier Leroy
   @author Nicolas Cannasse
   @author Markus Mottl
   @author David Rajchenbach-Teller
*)

type 'a t
  (** The type of sets. *)

include BatEnum.Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t

val empty: 'a t
  (** The empty set, using [compare] as comparison function *)

val create : ('a -> 'a -> int) -> 'a t
  (** Creates a new empty set, using the provided function for key comparison.*)

val is_empty: 'a t -> bool
  (** Test whether a set is empty or not. *)
  
val mem: 'a -> 'a t -> bool
  (** [mem x s] tests whether [x] belongs to the set [s]. *)
  
val add: 'a -> 'a t -> 'a t
  (** [add x s] returns a set containing all elements of [s],
      plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
  
val remove: 'a -> 'a t -> 'a t
  (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned unchanged. *)
  
  
val iter: ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] in turn to all elements of [s].
      The elements of [s] are presented to [f] in increasing order
      with respect to the ordering over the type of the elements. *)

val map: ('a -> 'b) -> 'a t -> 'b t
  (** [map f x] creates a new set with elements [f a0],
      [f a1]... [f an], where [a1], ..., [an] are the
      values contained in [x]*)
  
val filter: ('a -> bool) -> 'a t -> 'a t
  (** [filter p s] returns the set of all elements in [s]
      that satisfy predicate [p]. *)
  
val filter_map: ('a -> 'b option) -> 'a t -> 'b t
  (** [filter_map f m] combines the features of [filter] and
      [map].  It calls calls [f a0], [f a1], [f an] where [a0..an]
      are the elements of [m] and returns the set of pairs [bi]
      such as [f ai = Some bi] (when [f] returns [None], the
      corresponding element of [m] is discarded). *)
  
val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
      where [x1 ... xN] are the elements of [s], in increasing order. *)
  
val exists: ('a -> bool) -> 'a t -> bool
  (** [exists p s] checks if at least one element of
      the set satisfies the predicate [p]. *)
  
  
val cardinal: 'a t -> int
  (** Return the number of elements of a set. *)


val choose : 'a t -> 'a
  (** returns one binding of the given map, deterministically.  Raises
      [Invalid_argument] if given an empty set. *)

val min_elt : 'a t -> 'a
  (** returns the binding with the smallest key. Raises
      [Invalid_argument] if given an empty set. *)

val max_elt : 'a t -> 'a
  (** returns the binding with the largest key. Raises
      [Invalid_argument] if given an empty set.*)
  
val enum: 'a t -> 'a BatEnum.t
  (** Return an enumeration of all elements of the given set.
      The returned enumeration is sorted in increasing order with respect
      to the ordering of this set.*)
  
val of_enum: 'a BatEnum.t -> 'a t

val for_all : ('a -> bool) -> 'a t -> bool
(** Returns whether the given predicate applies to all elements in the set *)


val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** returns two disjoint subsets, those that satisfy the given
      predicate and those that don't *)

val filter : ('a -> bool) -> 'a t -> 'a t
  (** returns the subset of items satisfying the given predicate *)

val pop : 'a t -> 'a * 'a t
  (** returns one element of the set and the set without that element.
      Raises [Not_found] if given an empty set *)

(** {6 Boilerplate code}*)


(** {7 Printing}*)
  
val print :  ?first:string -> ?last:string -> ?sep:string -> 
  ('a BatInnerIO.output -> 'c -> unit) -> 
  'a BatInnerIO.output -> 'c t -> unit
