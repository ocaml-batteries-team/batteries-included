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

(** Functional heaps over ordered types

    Ascribes to:

    [BatEnum.Enumerable with type 'a enumerable = 'a t]
*)

type +'a t
  (** Heap of elements that are compared with [Pervasives.compare]. *)

val size : 'a t -> int
  (** Number of elements in the heap. O(1) *)

(** {6 Construction} *)

val empty : 'a t
  (** The empty heap. *)

val add : 'a t -> 'a -> 'a t
  (** Add an element to the heap. Duplicates are kept. O(log m) *)

val insert : 'a -> 'a t -> 'a t
  (** [insert x h] is the same as [add h x]. This function is intended
      to be used with [fold_right]. *)

(** {6 Operations} *)

val merge : 'a t -> 'a t -> 'a t
  (** Merge two heaps. O(log m) *)

val find_min : 'a t -> 'a
  (** Find the minimal element of the heap. O(1) *)

val del_min : 'a t -> 'a t
  (** Delete the minimal element of the heap. O(log n) *)

(** {6 Transformation} *)

val of_list : 'a list -> 'a t
  (** Build a heap from a given list. O(n log n) *)

val to_list : 'a t -> 'a list
  (** Enumerate the elements of the heap. O(n log n) *)

val elems : 'a t -> 'a list
  (** @deprecated Same as [to_list]. *)

val of_enum : 'a BatEnum.t -> 'a t
  (** Build a heap from an enumeration. Consumes the enumeration.
      O(n log n) *)

val enum : 'a t -> 'a BatEnum.t
  (** Enumerate the elements of the heap in heap order. O(log n) per
      {!BatEnum.get}. *)

module type H =
sig
  type elem
  type t
  val empty    : t
  val size     : t -> int
  val add      : t -> elem -> t
  val insert   : elem -> t -> t
  val merge    : t -> t -> t
  val find_min : t -> elem
  val del_min  : t -> t
  val of_list  : elem list -> t
  val to_list  : t -> elem list
  val elems    : t -> elem list
    (** @deprecated Same as [to_list]. *)
  val of_enum  : elem BatEnum.t -> t
  val enum     : t -> elem BatEnum.t
end

module Make (Ord : Set.OrderedType) : H with type elem = Ord.t
  (** Functorized heaps over arbitrary orderings. All the functions have
      the same complexity as the non-functorized versions. *)
