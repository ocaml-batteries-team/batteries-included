(*
 * RMutex - Reentrant mutexes
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
 *               2011 Edgar Friendly <thelema314@gmail.com>
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

module BaseRMutex =
struct

type owner =
  {
    thread : int;       (**Identity of the latest owner (possibly the current owner)*)
    mutable depth : int (**Number of times the current owner owns the lock.*)
  }

type t =
    {
      primitive : Mutex.t; (**A low-level mutex, used to protect access to [ownership]*)
      wait      : Condition.t; (** a condition to wait on when the lock is locked *)
      mutable ownership : owner option;
    }


let create () =
  {
    primitive = Mutex.create ();
    wait      = Condition.create ();
    ownership = None
  }

(**
   Attempt to acquire the mutex.

   @param hurry If true, in case the mutex cannot be acquired yet, just return [false],
   without waiting. Otherwise, wait.
*)
let lock_either hurry m = (*Stuff shared by [lock] and [try_lock]*)
  let id = Thread.id (Thread.self ()) in
  let rec aux () = match m.ownership with
    | None ->
      (*Lock belongs to nobody, I can take it. *)
      m.ownership <- Some {thread = id; depth = 1};
      true
    | Some s when s.thread = id ->
      (*Lock already belongs to me, I can keep it. *)
      s.depth <- s.depth + 1;
      true
    | _                        -> (*Lock belongs to someone else. *)
      if hurry then false (* give up *)
      else (* wait until someone releases the lock, then try again *)
	(Condition.wait m.wait m.primitive; aux())
  in
  Mutex.lock m.primitive;     (******Critical section begins*)
  let r = aux() in
  Mutex.unlock m.primitive; (******Critical section begins*)
  r

let lock     m = ignore (lock_either false m)
let try_lock m = lock_either true m

let unlock m =
  let id = Thread.id (Thread.self ()) in
    Mutex.lock m.primitive;     (******Critical section begins*)
    (match m.ownership with
       | Some s ->
	   assert (s.thread = id); (*If I'm not the owner, we have a consistency issue.*)
	   if s.depth > 1 then s.depth <- s.depth - 1 (*release one depth but we're still the owner*)
	   else
	     begin
	       m.ownership <- None;  (*release once and for all*)
	       Condition.signal m.wait   (*wake up waiting threads *)
	     end
       | _ -> assert false);
      Mutex.unlock m.primitive   (******Critical section ends  *)

end

module Lock = BatConcurrent.MakeLock(BaseRMutex)

include BaseRMutex

let make = Lock.make
let synchronize = Lock.synchronize

(*let synchronize ?lock:(l=create ()) f = fun x ->
  lock l;
  try
    let result = f x
    in lock l;
      result
  with e ->
    lock l;
    raise e*)

(*$R create; lock; unlock

   let l = create () in
   let num_threads = 30 in
   let count = ref 0 in
   let worker n = for i = 1 to num_threads - n do
     lock l; lock l; Thread.delay 0.001; incr count;
     unlock l; Thread.delay 0.0001; unlock l;
   done in
   let children = Array.init 30 (Thread.create worker) in
   Array.iter Thread.join children;

*)
