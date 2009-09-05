(*
 * ExtMutex - Additional functions for Mutexes
 * Copyright (C) 1996 Xavier Leroy
 *               1996 Damien Doligez
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

module Condition :
sig
(** Condition variables to synchronize between threads.

    Condition variables are used when one thread wants to wait until another
    thread has finished doing something: the former thread ``waits'' on the
    condition variable, the latter thread ``signals'' the condition when it
    is done. Condition variables should always be protected by a mutex.


    @documents Condition
*)

type 'a t

val create: 'a -> Mutex.t -> 'a t

val wait : 'a t -> ?pred:('a -> bool) -> action:('a -> 'b) -> 'b
(** 
    Wait until some predicate is verified, then perform an action.

    At the end of the action, you should probably verify if the
    predicate is valid and, if so, invoke {signal}.
*)


val signal : 'a t -> unit
(** [signal c] restarts one of the processes waiting on the 
   condition variable [c]. *)

val broadcast : 'a t -> unit
(** [broadcast c] restarts all processes waiting on the 
   condition variable [c]. *)

end
