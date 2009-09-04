(*
 * Cell - Concurrent cells
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

type 'a cell =
    {
      mutex:  Mutex.t;
      ready : Condition.t;
      result: 'a option ref
    }
let make () =
  { mutex = Mutex.create ();
    ready = Condition.create ();
    result= ref None}

let post: 'a cell -> 'a -> unit = fun cell x ->
  Mutex.lock cell.mutex;
  cell.result := Some x;
  Condition.signal cell.ready;
  Mutex.unlock cell.mutex

let get:   'a cell -> 'a = fun cell ->
  Mutex.lock cell.mutex;
  let rec aux () =
    match !(cell.result) with
      | None   -> Condition.wait cell.ready cell.mutex; aux ()
      | Some x -> x
  in let result = aux() in
    Mutex.unlock cell.mutex;
    result
