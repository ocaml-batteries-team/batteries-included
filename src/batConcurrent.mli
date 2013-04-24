(*
 * Concurrent - Generic interface for concurrent operations
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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
   Definition of concurrency primitives.

   @author David Teller
*)

type lock
(** The light-weight type of a lock, i.e. a construction which may be
    used to guarantee that a section will not be interrupted by
    another thread.

    This light-weight type is independent of the underlying locking
    mechanism and can be used if you do not know whether your code
    will run with vmthreads, Posix threads, coThreads, etc.
*)

val create: enter:(unit -> unit) -> leave:(unit -> unit)  -> lock
(** Create a lock from a pair of locking/unlocking functions

    @param enter Enter critical section.
    @param leave Leave critical section.
    .*)

val nolock : lock
(** A lock which does nothing.*)

val synchronize: (unit -> lock) -> ('a -> 'b) -> 'a -> 'b
(**
   [synchronize locker f] returns a function [f'] which behaves as
   [f] but whose executions are protected by one lock obtained from
   [locker].  The same lock will be reused for all subsequent uses of
   [f'].

   For instance,
   [synchronize Mutex.make f] is a new function whose executions
   will by synchronized by a new lock. Conversely,
   [synchronize (const my_lock) f] is a new function whose executions
   will be synchronized by an existing lock [my_lock].
*)

val sync:lock -> ('a -> 'b) -> 'a -> 'b
(**
   Specialized version of [synchronized].

   [sync lock f] behaves as [synchronize (const lock) f] but slightly faster
*)

val compose : lock -> lock -> lock
(**
   Compose two lock systems into a third lock system.
*)



(** A signature for modules which implement locking.*)
module type BaseLock =
sig
  type t(**The type of a lock.*)

  val create:unit -> t
  val lock : t -> unit
  val unlock:t -> unit
  val try_lock:t -> bool
end


module type Lock =
sig
  type t(**The type of a lock.*)

  val create: unit -> t
  val lock  : t -> unit
  val unlock: t -> unit
  val try_lock:t -> bool

  val synchronize: ?lock:t -> ('a -> 'b) -> 'a -> 'b

  val make  : unit -> lock
end

module MakeLock(M:BaseLock) : Lock with type t = M.t

module NoLock : Lock

