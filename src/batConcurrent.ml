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


type lock = {execute : 'a 'b. ('a -> 'b) -> 'a -> 'b}

let nolock= {execute = (fun f x -> f x)}

let sync lock = lock.execute

let synchronize locker f x =
  sync (locker ()) f x

let compose {execute = a} {execute = b} =
  {
    execute = (fun f x -> b (a f) x)
  }

let create ~enter ~leave =
  {
    execute = (fun f x -> 
		 enter ();
		 try 
		   let result = f x in
		     leave ();
		     result
		 with e -> 
		   leave ();
		   raise e
	      )
  }


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

let base_create = create

module MakeLock(M:BaseLock) : Lock with type t = M.t =
struct
  type t = M.t
  let create = M.create
  let lock   = M.lock
  let unlock = M.unlock
  let try_lock=M.try_lock
  let synchronize ?(lock=M.create ()) f x =
    try 
      M.lock lock;
      let result = f x in
	M.unlock lock;
	result
    with e -> M.unlock lock;
      raise e

  let make () = 
    let lock = M.create () in
      base_create 
	~enter:(fun () -> M.lock lock)
	~leave:(fun () -> M.unlock lock)

end

module BaseNoLock = struct
  type t = unit
  external create: unit -> t = "%ignore"
  external lock  : t -> unit = "%ignore"
  external unlock: t -> unit = "%ignore"
  let try_lock t = true
end
module NoLock = MakeLock(BaseNoLock)
