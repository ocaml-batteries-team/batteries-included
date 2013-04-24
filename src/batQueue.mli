(*
 * BatQueue - Extended operations on queues
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

(** First-in first-out queues.

    This module implements queues (FIFOs), with in-place modification.

    @author Xavier Leroy (Base module)
    @author David Teller
*)

type 'a t = 'a Queue.t
(** The type of queues containing elements of type ['a]. *)


exception Empty
(** Raised when {!Queue.take} or {!Queue.peek} is applied to an empty queue. *)


val create : unit -> 'a t
(** Return a new queue, initially empty. *)

val add : 'a -> 'a t -> unit
(** [add x q] adds the element [x] at the end of the queue [q]. *)

val push : 'a -> 'a t -> unit
(** [push] is a synonym for [add]. *)

val take : 'a t -> 'a
(** [take q] removes and returns the first element in queue [q],
    or raises [Empty] if the queue is empty. *)

val pop : 'a t -> 'a
(** [pop] is a synonym for [take]. *)

val peek : 'a t -> 'a
(** [peek q] returns the first element in queue [q], without removing
    it from the queue, or raises [Empty] if the queue is empty. *)

val top : 'a t -> 'a
(** [top] is a synonym for [peek]. *)

val clear : 'a t -> unit
(** Discard all elements from a queue. *)

val copy : 'a t -> 'a t
(** Return a copy of the given queue. *)

val is_empty : 'a t -> bool
(** Return [true] if the given queue is empty, [false] otherwise. *)

val length : 'a t -> int
(** Return the number of elements in a queue. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f q] applies [f] in turn to all elements of [q],
    from the least recently entered to the most recently entered.
    The queue itself is unchanged. *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold f accu q] is equivalent to [List.fold_left f accu l],
    where [l] is the list of [q]'s elements. The queue remains
    unchanged. *)

val transfer : 'a t -> 'a t -> unit
(** [transfer q1 q2] adds all of [q1]'s elements at the end of
    the queue [q2], then clears [q1]. It is equivalent to the
    sequence [iter (fun x -> add x q2) q1; clear q1], but runs
    in constant time. *)


type 'a enumerable = 'a t

val enum : 'a t -> 'a BatEnum.t
(** [enum q] returns a destructive enumeration of the elements of queue
    [q], from the least recently entered to the most recently entered.
    Reading the enumeration will progressively empty [q].*)

val of_enum : 'a BatEnum.t -> 'a t
(** [of_enum e] returns a new queue containing all the elements of [e].
    This is equivalent to calling [push] with the first element of the
    enumeration, then with the second, etc.*)

(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b t -> unit

val compare : 'a BatOrd.comp -> 'a t BatOrd.comp
val equal : 'a BatOrd.eq -> 'a t BatOrd.eq

module Exceptionless : sig
  val take : 'a t -> 'a option
  val peek : 'a t -> 'a option
end
