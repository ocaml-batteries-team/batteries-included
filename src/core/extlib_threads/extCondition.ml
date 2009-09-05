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

module Condition = struct
  open Extlib
  open Std
  type 'a t =
      { data : 'a;
	cond : Condition.t;
	mutex: Mutex.t}

  let create data mutex = 
    {data = data;
     cond = Condition.create ();
     mutex= mutex}

  let wait t ?(pred=const true) ~action =
    Mutex.lock t.mutex;
    Std.finally 
      (fun () -> Mutex.unlock t.mutex)
      (fun () ->
	 while not (pred t.data) do 
	   Condition.wait t.cond t.mutex
	 done;
	 action t.data) ()

  let signal t = 
    Mutex.lock t.mutex;
    Condition.signal t.cond;
    Mutex.unlock t.mutex

  let broadcast t =
    Mutex.lock t.mutex;
    Condition.broadcast t.cond;
    Mutex.unlock t.mutex
end
