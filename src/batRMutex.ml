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
     Attempt to acquire the mutex, waiting indefinitely
  *)
  let lock m =
    let id = Thread.id (Thread.self ()) in
    Mutex.lock m.primitive; (******Critical section begins*)
    (
      match m.ownership with
      | None -> (*Lock belongs to nobody, I can take it. *)
        m.ownership <- Some {thread = id; depth = 1}
      | Some s when s.thread = id -> (*Lock already belongs to me, I can keep it. *)
        s.depth <- s.depth + 1
      | _ -> (*Lock belongs to someone else. *)
        while not (m.ownership = None) do
          Condition.wait m.wait m.primitive
        done;
        m.ownership <- Some {thread = id; depth = 1}
    );
    Mutex.unlock m.primitive (******Critical section ends*)

  (** Attempt to acquire the mutex, returning true if successful.  If
      waiting would be required, return false instead.
  *)
  let try_lock m =
    let id = Thread.id (Thread.self ()) in
    Mutex.lock m.primitive;     (******Critical section begins*)
    let r =
      match m.ownership with
      | None -> (*Lock belongs to nobody, I can take it. *)
        m.ownership <- Some {thread = id; depth = 1};
        true
      | Some s when s.thread = id -> (*Lock already belongs to me, I can keep it. *)
        s.depth <- s.depth + 1;
        true
      | _ -> (*Lock belongs to someone else. *)
        false (* give up *)
    in
    Mutex.unlock m.primitive; (******Critical section ends*)
    r


  (** Unlock the mutex; this function checks that the thread calling
      unlock is the owner and raises an assertion failure if this is not
      the case. It will also raise an assertion failure if the mutex is
      not locked. *)
  let unlock m =
    let id = Thread.id (Thread.self ()) in
    Mutex.lock m.primitive; (******Critical section begins*)
    (match m.ownership with
     | Some s ->
       assert (s.thread = id); (*If I'm not the owner, we have a consistency issue.*)
       if s.depth > 1 then
         s.depth <- s.depth - 1 (*release one depth but we're still the owner*)
       else
         begin
           m.ownership <- None;  (*release once and for all*)
           Condition.signal m.wait   (*wake up waiting threads *)
         end
     | _ -> assert false
    );
    Mutex.unlock m.primitive (******Critical section ends  *)

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

    let test num_threads work_per_thread =
     let l = create () in
     let count = ref 0 in
     let worker n = for i = 1 to work_per_thread do
       lock l; lock l; Thread.delay 0.001; incr count;
       unlock l; Thread.delay 0.0001; unlock l;
     done in
     let children = Array.init num_threads (Thread.create worker) in
     Array.iter Thread.join children;
     !count
    in
    assert_equal (30*30) (test 30 30) ~printer:string_of_int

  *)
