(*
 * BatMutex - Additional functions for Mutexes
 * Copyright (C) 1996 Xavier Leroy
 *               1996 Damien Doligez
 *               2008 David Teller
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

module DebugMutex =
struct
  module M =
  struct
    type t =
      { mutex : Mutex.t;
        id    : int }

    let unique =
      let counter = ref 0
      and mutex   = Mutex.create ()
      in
      Mutex.lock mutex;
      let result = !counter in
      incr counter;
      Mutex.unlock mutex;
      result

    let create () =
      { mutex = Mutex.create () ;
        id    = unique }

    let lock t =
      Printf.eprintf "[Mutex] Attempting to lock mutex %d\n" t.id;
      Mutex.lock t.mutex;
      Printf.eprintf "[Mutex] Mutex %d locked\n" t.id

    let unlock t =
      Printf.eprintf "[Mutex] Attempting to unlock mutex %d\n" t.id;
      Mutex.unlock t.mutex;
      Printf.eprintf "[Mutex] Mutex %d unlocked\n" t.id

    let try_lock t =
      Printf.eprintf "[Mutex] Attempting to trylock mutex %d\n" t.id;
      let result = Mutex.try_lock t.mutex in
      Printf.eprintf "[Mutex] Mutex %d trylocked\n" t.id;
      result
  end

  include M
  module Lock = BatConcurrent.MakeLock(M)
  let make        = Lock.make
  let synchronize = Lock.synchronize
end

module Lock = BatConcurrent.MakeLock(Mutex)
let make        = Lock.make
let synchronize = Lock.synchronize
