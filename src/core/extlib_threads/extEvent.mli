(*
 * ExtEvent - Additional functions for Events
 * Copyright (C) 1996 Xavier Leroy
 *               1996 David Nowak
 *               2009 David Rajchenbach-Teller
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

module Event :
sig
open Extlib
open Enum

(** First-class synchronous communication.

   This module implements synchronous inter-thread communications over
   channels. As in John Reppy's Concurrent ML system, the communication 
   events are first-class values: they can be built and combined
   independently before being offered for communication. 

    @documents Event

    @author Xavier Leroy (Base library)
    @author David Nowak (Base library)
    @author David Rajchenbach-Teller
*)

type 'a channel = 'a Event.channel
(** The type of communication channels carrying values of type ['a]. *)

val new_channel : unit -> 'a channel
(** Return a new channel. *)

type +'a event = 'a Event.event 
(** The type of communication events returning a result of type ['a]. *)

(** [send ch v] returns the event consisting in sending the value [v]
   over the channel [ch]. The result value of this event is [()]. *) 
val send : 'a channel -> 'a -> unit event

(** [receive ch] returns the event consisting in receiving a value
   from the channel [ch]. The result value of this event is the
   value received. *) 
val receive : 'a channel -> 'a event

val always : 'a -> 'a event
(** [always v] returns an event that is always ready for
   synchronization.  The result value of this event is [v]. *)

val choose : 'a event list -> 'a event
(** [choose evl] returns the event that is the alternative of
   all the events in the list [evl]. *)

val wrap : 'a event -> ('a -> 'b) -> 'b event
(** [wrap ev fn] returns the event that performs the same communications
   as [ev], then applies the post-processing function [fn]
   on the return value. *)

val wrap_abort : 'a event -> (unit -> unit) -> 'a event
(** [wrap_abort ev fn] returns the event that performs
   the same communications as [ev], but if it is not selected
   the function [fn] is called after the synchronization. *)

val guard : (unit -> 'a event) -> 'a event
(** [guard fn] returns the event that, when synchronized, computes
   [fn()] and behaves as the resulting event. This allows to
   compute events with side-effects at the time of the synchronization
   operation. *)

val sync : 'a event -> 'a
(** ``Synchronize'' on an event: offer all the communication 
   possibilities specified in the event to the outside world,
   and block until one of the communications succeed. The result
   value of that communication is returned. *)

val select : 'a event list -> 'a
(** ``Synchronize'' on an alternative of events.
   [select evl] is shorthand for [sync(choose evl)]. *)

val poll : 'a event -> 'a option
(** Non-blocking version of {!Event.sync}: offer all the communication 
   possibilities specified in the event to the outside world,
   and if one can take place immediately, perform it and return
   [Some r] where [r] is the result value of that communication.
   Otherwise, return [None] without blocking. *)

val enum: 'a channel -> 'a Enum.t
(** Enumerate all the future messages sent on a channel.

    The resulting enumeration is infinite.*)

val of_enum: 'a Enum.t -> 'a channel
(** Access the contents of an enumeration from a channel.

    This causes the creation of a thread for writing to the channel.*)

end
