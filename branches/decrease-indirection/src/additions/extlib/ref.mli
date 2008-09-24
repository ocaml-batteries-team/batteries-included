(* 
 * Ref - Operations on references
 * Copyright (C) 2008 David Teller (contributor)
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

val pre : 'a ref -> ( 'a -> 'a ) -> 'a
  (** Perform an operation on a reference and return the
      previous value of that reference. 

      For instance, if [x] is a reference to [1],
      [pre x ( ( + ) 1) ] returns [1] and sets [x] to [2].*)

val post: 'a ref -> ('a -> 'a) -> 'a
  (** Perform an operation on a reference and return the
      new value of that reference. 

      For instance, if [x] is a reference to [1],
      [pre x ( ( + ) 1)] returns [2] and sets [x] to [2].*)

val swap: 'a ref -> 'a ref -> unit
  (**[swap a b] puts [!b] in [a] and [!a] in [b]*)

val pre_incr : int ref -> int
  (**Increment an integer, return the old value.*)

val pre_decr : int ref -> int
  (**Decrement an integer, return the old value.*)

val post_incr: int ref -> int
  (**Increment an integer, return the new value.*)

val post_decr: int ref -> int
  (**Increment an integer, return the new value.*)
