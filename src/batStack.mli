(* 
 * ExtQueue - Extended operations on queues
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


(** Last-in first-out stacks.

    This module implements stacks (LIFOs), with in-place modification.
    
    @author Xavier Leroy (Base module)
    @author David Teller
*)
module Stack:
sig

type 'a t
(** The type of stacks containing elements of type ['a]. *)

include Enum.Enumerable with type 'a enumerable = 'a t

exception Empty
(** Raised when {!Stack.pop} or {!Stack.top} is applied to an empty stack. *)


val create : unit -> 'a t
(** Return a new stack, initially empty. *)

val push : 'a -> 'a t -> unit
(** [push x s] adds the element [x] at the top of stack [s]. *)

val pop : 'a t -> 'a
(** [pop s] removes and returns the topmost element in stack [s],
   or raises [Empty] if the stack is empty. *)

val top : 'a t -> 'a
(** [top s] returns the topmost element in stack [s],
   or raises [Empty] if the stack is empty. *)

val clear : 'a t -> unit
(** Discard all elements from a stack. *)

val copy : 'a t -> 'a t
(** Return a copy of the given stack. *)

val is_empty : 'a t -> bool
(** Return [true] if the given stack is empty, [false] otherwise. *)

val length : 'a t -> int
(** Return the number of elements in a stack. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f s] applies [f] in turn to all elements of [s],
   from the element at the top of the stack to the element at the
   bottom of the stack. The stack itself is unchanged. *)

val enum : 'a t -> 'a Enum.t
(** [enum s] returns a destructive enumeration of the elements of stack 
    [s], from the most recently entered to the least recently entered.
    Reading the enumeration will progressively empty [s].*)

val of_enum : 'a Enum.t -> 'a t
(** [of_enum e] returns a new stack containing all the elements of [e].
    This is equivalent to calling [push] with the first element of the
    enumeration, then with the second, etc.*)

	(** {6 Boilerplate code}*)
  
(** {7 Printing}*)
  
val print : ?first:string -> ?last:string -> ?sep:string -> ('a InnerIO.output -> 'b -> unit) ->  'a InnerIO.output -> 'b t -> unit

end

