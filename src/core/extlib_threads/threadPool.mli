(*
 * Threadpool - Threading with bounded number of threads
 * Copyright (C) 2009 David Rajchenbach-Teller
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
    Parallelism with a bound on the number of threads used.

    @author David Rajchenbach-Teller
*)

type t
(** The type of a pool of threads.*)

type 'a thread
(** The type of a thread with a return type of ['a]. *)

val create: int -> t
(**Create a new pool of thread, to manage a given number of threads.*)

(** {6 Thread creation and termination} *)

val spawn: t -> ('a -> 'b) -> 'a -> 'b thread

val join : 'a thread -> 'a
(** [join th] suspends the execution of the calling thread
   until the thread [th] has terminated. *)
(*
val id : _ thread -> int
(** Return the identifier of the given thread. A thread identifier
   is an integer that identifies uniquely the thread.
   It can be used to build data structures indexed by threads. *)

val exit:  'a thread -> 'a -> unit

val kill : 'a thread -> unit
(** Terminate prematurely the thread whose handle is given. *)

(** {6 Suspending threads} *)

val delay: float -> unit
(** [delay d] suspends the execution of the calling thread for
   [d] seconds. The other program threads continue to run during
   this time. *)



val wait_read : Unix.file_descr -> unit
(** See {!Thread.wait_write}.*)

val wait_write : Unix.file_descr -> unit
(** This function does nothing in this implementation. *)

val wait_timed_read : Unix.file_descr -> float -> bool
(** See {!Thread.wait_timed_read}.*)

val wait_timed_write : Unix.file_descr -> float -> bool
(** Suspend the execution of the calling thread until at least
   one character is available for reading ([wait_read]) or
   one character can be written without blocking ([wait_write])
   on the given Unix file descriptor. Wait for at most
   the amount of time given as second argument (in seconds).
   Return [true] if the file descriptor is ready for input/output
   and [false] if the timeout expired. 

   These functions return immediately [true] in the Win32
   implementation. *)

val select :
  Unix.file_descr list -> Unix.file_descr list ->
  Unix.file_descr list -> float ->
    Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
(** Suspend the execution of the calling thead until input/output
   becomes possible on the given Unix file descriptors.
   The arguments and results have the same meaning as for
   [Unix.select].
   This function is not implemented yet under Win32. *)

val wait_pid : int -> int * Unix.process_status
(** [wait_pid p] suspends the execution of the calling thread
   until the process specified by the process identifier [p]
   terminates. Returns the pid of the child caught and
   its termination status, as per [Unix.wait].
   This function is not implemented under MacOS. *)

val yield : unit -> unit
(** Re-schedule the calling thread without suspending it.
   This function can be used to give scheduling hints,
   telling the scheduler that now is a good time to
   switch to other threads. *)

(** {6 Management of signals} *)

(** Signal handling follows the POSIX thread model: signals generated
  by a thread are delivered to that thread; signals generated externally
  are delivered to one of the threads that does not block it.
  Each thread possesses a set of blocked signals, which can be modified
  using {!Thread.sigmask}.  This set is inherited at thread creation time.
  Per-thread signal masks are supported only by the system thread library
  under Unix, but not under Win32, nor by the VM thread library. *)

val sigmask : Unix.sigprocmask_command -> int list -> int list
(** [sigmask cmd sigs] changes the set of blocked signals for the
   calling thread.
   If [cmd] is [SIG_SETMASK], blocked signals are set to those in
   the list [sigs].
   If [cmd] is [SIG_BLOCK], the signals in [sigs] are added to
   the set of blocked signals.
   If [cmd] is [SIG_UNBLOCK], the signals in [sigs] are removed
   from the set of blocked signals.
   [sigmask] returns the set of previously blocked signals for the thread. *)


val wait_signal : int list -> int
(** [wait_signal sigs] suspends the execution of the calling thread
   until the process receives one of the signals specified in the
   list [sigs].  It then returns the number of the signal received.
   Signal handlers attached to the signals in [sigs] will not
   be invoked.  The signals [sigs] are expected to be blocked before
   calling [wait_signal]. *)

*)
