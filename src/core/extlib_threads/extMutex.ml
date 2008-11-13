(*
 * ExtMutex - Additional functions for Mutexes
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

module Mutex =
struct
  include Mutex
    
(*  let with_lock ?(lock = create ()) f =
    try f lock
    with e -> *)

  let synchronize ?(lock = create ()) f = fun x ->
    Mutex.lock lock;
    try
      let result = f x
      in Mutex.unlock lock;
	result
    with e ->
      Mutex.unlock lock;
      raise e
end
