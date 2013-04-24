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

val insert : 'a t -> 'a -> 'a t
(** Insert an element into the heap. Duplicates are kept. O(log m) *)

val add : 'a -> 'a t -> 'a t
(** [add x h] is the same as [insert h x]. This function is intended
    to be used with [fold_right]. *)

(** {6 Operations} *)

val merge : 'a t -> 'a t -> 'a t
(** Merge two heaps. O(log m) *)

val find_min : 'a t -> 'a
(** Find the minimal element of the heap. O(1)
    @raise Invalid_argument ["find_min"] if the heap is empty *)

val del_min : 'a t -> 'a t
(** Delete the minimal element of the heap. O(log n)
    @raise Invalid_argument ["del_min"] if the heap is empty *)

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

(** {6 Printing} *)

val print :  ?first:string -> ?last:string -> ?sep:string
  -> ('a, 'b) BatIO.printer -> ('a t, 'b) BatIO.printer
(** Print the contents of the heap in heap order. O(n log n) *)


(** {6 Functorized version} *)

(** The result of {!Make} *)
module type H =
sig
  type elem
  (** Type of elements of the heap *)
  type t
  (** Type of the heap *)
  val empty     : t
  (** See {!BatHeap.empty}. *)
  val size      : t -> int
  (** See {!BatHeap.size}. *)
  val insert    : t -> elem -> t
  (** See {!BatHeap.add}. *)
  val add       : elem -> t -> t
  (** See {!BatHeap.insert}. *)
  val merge     : t -> t -> t
  (** See {!BatHeap.merge}. *)
  val find_min  : t -> elem
  (** See {!BatHeap.find_min}. *)
  val del_min   : t -> t
  (** See {!BatHeap.del_min}. *)
  val of_list   : elem list -> t
  (** See {!BatHeap.of_list}. *)
  val to_list   : t -> elem list
  (** See {!BatHeap.to_list}. *)
  val elems     : t -> elem list
  (** @deprecated Same as [to_list]. *)
  val of_enum   : elem BatEnum.t -> t
  (** See {!BatHeap.of_enum}. *)
  val enum      : t -> elem BatEnum.t
  (** See {!BatHeap.enum}. *)
  val print     :  ?first:string -> ?last:string -> ?sep:string
    -> (elem, 'a) BatIO.printer -> (t, 'a) BatIO.printer
      (** See {!BatHeap.print}. *)
end

module Make (Ord : BatInterfaces.OrderedType) : H with type elem = Ord.t
  (** Functorized heaps over arbitrary orderings. All the functions have
      the same complexity as the non-functorized versions. *)
