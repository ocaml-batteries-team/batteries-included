(*
 * ExtMutex - Additional functions for Mutexes
 * Copyright (C) 1996 Xavier Leroy
 *               1996 Damien Doligez
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

(** Locks for mutual exclusion.

   Mutexes (mutual-exclusion locks) are used to implement critical sections
   and protect shared mutable data structures against concurrent accesses.
   The typical use is (if [m] is the mutex associated with the data structure
   [D]):
    {[
     Mutex.synchronize ~lock:m (fun () ->
        (* Critical section that operates over D *);
     ) ()
    ]}

    This module implements {!Control.Concurrency.Common}

    @author Xavier Leroy (Base module)
    @author Damien Doligez (Base module)
    @author David Teller
*)

open Mutex

val synchronize : ?lock:t -> ('a -> 'b) -> 'a -> 'b
(** Protect a function.

    [synchronize f] returns a new function [f'] with the same behavior
    as [f] but such that concurrent calls to [f'] are queued if
    necessary to avoid races.

    [synchronize ~lock:l f] behaves as [synchronize f] but uses a
    user-specified lock [l], which may be useful to share a lock
    between several function. This is necessary in particular when
    the lock is specific to a data structure rather than to a
    function.

    In either case, the lock is acquired when entering the function
    and released when the function call ends, whether this is due
    to normal termination or to some exception being raised.
*)

val make : unit -> BatConcurrent.lock
(**
   Create a new abstract lock based on Mutexes.
*)

(**/**)
module DebugMutex:
sig

type t
(** The type of mutexes. *)

val create : unit -> t
(** Return a new mutex. *)

val lock : t -> unit
  (** Lock the given mutex. Only one thread can have the mutex locked
      at any time. A thread that attempts to lock a mutex already locked
      will suspend until the other mutex is unlocked.

      {b Note} attempting to lock a mutex you already have locked from
      the same thread will also suspend your thread, possibly forever.
      If this is not what you want, take a look at module {!RMutex}.
*)

val try_lock : t -> bool
(** Same as {!Mutex.lock}, but does not suspend the calling thread if
    the mutex is already locked: just return [false] immediately
    in that case. If the mutex is unlocked, lock it and
    return [true]. *)

val unlock : t -> unit
(** Unlock the given mutex. Other threads suspended trying to lock
    the mutex will restart. If the mutex wasn't locked, nothing happens.*)

val synchronize : ?lock:t -> ('a -> 'b) -> 'a -> 'b
(** Protect a function.

    [synchronize f] returns a new function [f'] with the same behavior
    as [f] but such that concurrenty calls to [f'] are queued if
    necessary to avoid races.

    [synchronize ~lock:l f] behaves as [synchronize f] but uses a
    user-specified lock [l], which may be useful to share a lock
    between several function. This is necessary in particular when
    the lock is specific to a data structure rather than to a
    function.

    In either case, the lock is acquired when entering the function
    and released when the function call ends, whether this is due
    to normal termination or to some exception being raised.
*)

val make : unit -> BatConcurrent.lock
(**
   Create a new abstract lock based on Mutexes.
*)

end
(**/**)
