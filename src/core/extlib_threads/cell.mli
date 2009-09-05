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

(**
   Concurrent cells, i.e. a data structure placed to asynchronously 
   put {e one} message or synchronously wait for a message.
*)

type 'a cell
(** The type of a concurrent cell*)

val make: unit    -> 'a cell

val post: 'a cell -> 'a -> unit

val get:  'a cell -> 'a
(**
   Return the value or the exception stored on the cell.
*)

val fail: 'a cell -> exn -> unit
