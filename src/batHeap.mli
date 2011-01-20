(*
 * Heap -- binomial heaps
 * Copyright (C) 2011  Batteries Included Development Team
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

(** Functional heaps over ordered types *)

type +'a t
  (** Heap of elements that are compared with [Pervasives.compare]. *)

val empty : 'a t
  (** The empty heap. *)

val size : 'a t -> int
  (** Number of elements in the heap. O(1) *)

val add : 'a t -> 'a -> 'a t
  (** Add an element to the heap. Duplicates are kept. O(log m) *)

val insert : 'a -> 'a t -> 'a t
  (** [insert x h] is the same as [add h x]. This function is intended
      to be used with [fold_right]. *)

val merge : 'a t -> 'a t -> 'a t
  (** Merge two heaps. O(log m) *)

val find_min : 'a t -> 'a
  (** Find the minimal element of the heap. O(1) *)

val del_min : 'a t -> 'a t
  (** Delete the minimal element of the heap. O(log n) *)

val elems : 'a t -> 'a list
  (** Enumerate the elements of the heap. O(n log n) *)

module type H =
sig
  module Ord : Set.OrderedType
  type t
  val empty : t
  val size : t -> int
  val add : t -> Ord.t -> t
  val insert : Ord.t -> t -> t
  val merge : t -> t -> t
  val find_min : t -> Ord.t
  val del_min : t -> t
  val elems : t -> Ord.t list
end

module Make (Ord : Set.OrderedType) : H with module Ord = Ord
  (** Functorized heaps over arbitrary orderings. All the functions have
      the same complexity as the non-functorized versions. *)
