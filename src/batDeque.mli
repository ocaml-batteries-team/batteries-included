(*
 * Deque -- functional double-ended queues
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

(** Functional double-ended queues *)

type +'a dq
(** The type of double-ended queues *)

type 'a t = 'a dq
(** A synonym for convenience *)

include BatEnum.Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t

val size : 'a dq -> int
(** [size dq] is the number of elements in the [dq]. O(1) *)

(** {6 Construction} *)

val empty : 'a dq
(** The empty deque. *)

val cons : 'a -> 'a dq -> 'a dq
(** [cons x dq] adds [x] to the front of [dq]. O(1) *)

val snoc : 'a dq -> 'a -> 'a dq
(** [snoc x dq] adds [x] to the rear of [dq]. O(1) *)

(** {6 Deconstruction} *)

val front : 'a dq -> ('a * 'a dq) option
(** [front dq] returns [Some (x, dq')] iff [x] is at the front of
    [dq] and [dq'] is the rest of [dq] excluding [x], and [None] if
    [dq] has no elements. O(1) amortized, O(n) worst case *)

val rear : 'a dq -> ('a dq * 'a) option
(** [rear dq] returns [Some (dq', x)] iff [x] is at the rear of [dq]
    and [dq'] is the rest of [dq] excluding [x], and [None] if [dq]
    has no elements. O(1) amortized, O(n) worst case *)

(** {6 Basic operations} *)

val rev : 'a dq -> 'a dq
(** [rev dq] reverses [dq]. O(1) *)

val is_empty : 'a dq -> bool
(** [is_empty dq] returns [false] iff [dq] has no elements. O(1) *)

val at : ?backwards:bool -> 'a dq -> int -> 'a option
(** [at ~backwards dq k] returns the [k]th element of [dq], from
    the front if [backwards] is false, and from the rear if
    [backwards] is true. By default, [backwards = false]. O(n) *)

val map : ('a -> 'b) -> 'a dq -> 'b dq
(** [map f dq] returns a deque where every element [x] of [dq] has
    been replaces with [f x]. O(n) *)

val mapi : (int -> 'a -> 'b) -> 'a dq -> 'b dq
(** [map f dq] returns a deque where every element [x] of [dq] has
    been replaces with [f n x], where [n] is the position of [x]
    from the front of [dq]. O(n) *)

val iter : ('a -> unit) -> 'a dq -> unit
(** [iter f dq] calls [f x] on each element [x] of [dq]. O(n) *)

val iteri  : (int -> 'a -> unit) -> 'a dq -> unit
(** [iteri f dq] calls [f n x] on each element [x] of [dq]. The first
    argument to [f] is the position of the element from the front of
    [dq]. O(n) *)

val find : ?backwards:bool -> ('a -> bool) -> 'a dq -> (int * 'a) option
(** [find ~backwards f dq] returns [Some (n, x)] if [x] at position
    [n] is such that [f x] is true, or [None] if there is no such
    element. The position [n] is from the rear if [backwards] is
    true, and from the front if [backwards] is [false]. By default,
    [backwards] is [false]. O(n) *)

val fold_left  : ('acc -> 'a -> 'acc) -> 'acc -> 'a dq -> 'acc
(** [fold_left f acc dq] is equivalent to [List.fold_left f acc
    (to_list dq)], but more efficient. O(n) *)

val fold_right : ('a -> 'acc -> 'acc) -> 'a dq -> 'acc -> 'acc
(** [fold_right f dq acc] is equivalent to [List.fold_right f
    (to_list dq) acc], but more efficient. O(n) *)

val append : 'a dq -> 'a dq -> 'a dq
(** [append dq1 dq2] represents the concatenateion of [dq1] and
    [dq2]. O(min(m, n))*)

val append_list  : 'a dq -> 'a list -> 'a dq
(** [append_list dq l] is equivalent to [append dq (of_list l)], but
    more efficient. O(min(m, n)) *)

val prepend_list : 'a list -> 'a dq -> 'a dq
(** [prepent_list l dq] is equivalent to [append (of_list l) dq],
    but more efficient. O(min(m, n)) *)

(** {6 Transformation} *)

val of_list : 'a list -> 'a dq
(** [of_list l] is a deque representation of the elements of [l].
    O(n) *)

val to_list : 'a dq -> 'a list
(** [to_list dq] is a list representation of the elements of [dq].
    O(n) *)

val of_enum : 'a BatEnum.t -> 'a dq
(** [of_enum e] is a deque representation of the elements of [e].
    Consumes the enumeration [e]. O(n) *)

val enum : 'a dq -> 'a BatEnum.t
(** [enum dq] is an enumeration of the elements of [dq] from the
    front to the rear.
    This function is O(1), but generating each element of the enumeration
    is amortized O(1), and O(n) worst case.
*)

(** {6 Printing} *)

val print : ?first:string -> ?last:string -> ?sep:string
  -> ('a, 'b) BatIO.printer -> ('a dq, 'b) BatIO.printer
(** Print the contents of the deque. O(n) *)

(**/**)
val invariants : _ t -> unit
  (**/**)
