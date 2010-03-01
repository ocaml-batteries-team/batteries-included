(*
 * RMutex - Reentrant mutexes
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
      wait      : Mutex.t;
      mutable ownership : owner option;
    }
      
      
let create () =
  {
    primitive = Mutex.create ();
    wait      = Mutex.create ();
    ownership = None
  }
    
(**
   Attempt to acquire the mutex.
   
   @param hurry If true, in case the mutex cannot be acquired yet, just return [false],
   without waiting. Otherwise, wait.
*)
let lock_either hurry m = (*Stuff shared by [lock] and [try_lock]*)
  let id = Thread.id (Thread.self ()) in
  let rec aux () =
    let wait = ref false in
      Mutex.lock m.primitive;     (******Critical section begins*)
      (match m.ownership with
	 | None                     -> (*Lock belongs to nobody, I can take it.      *)
	     m.ownership <- Some {thread = id; depth = 1};
	 | Some s when s.thread = id -> (*Lock already belongs to me, I can keep it.  *)
	     s.depth <- s.depth + 1
	 | _                        -> (*Lock belongs to someone else. I should wait.*)
	     wait := true);
      Mutex.unlock m.primitive;  (******Critical section ends*)
      if !wait then
	if hurry then false
	else
	  begin
	    Mutex.lock m.wait;        (*Get in line and try again*)
	    aux ()
	  end
      else true
  in aux()
       
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
	       Mutex.unlock m.wait   (*wake up waiting threads *)
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
