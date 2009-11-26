(*
 * Pair - functions for pairs of values
 * Copyright (C) 2009 Edgar Friendly
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

(** Functions for pairs of values

    @author Edgar Friendly
*)

type ('a,'b) t = 'a * 'b

(** map a function across both values in a pair *)
val map : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)

(** Compare two pairs in lexicographic order, possibly using custom comparators for the two fields *)
val compare : ?c1:('a -> 'a -> int) -> ?c2:('b -> 'b -> int) -> ('a * 'b) -> ('a * 'b) -> int

(** builds a two-element enum of a pair *)
val enum : ('a * 'a) -> 'a Enum.t

(** builds a pair out of the first two values of an enum.  Raises [Failure] if insufficient elements *)
val of_enum : 'a Enum.t -> ('a * 'a)

(** Prints a pair using given printing functions *)
val print : ('o IO.output -> 'a -> unit) -> ('o IO.output -> 'b -> unit) -> 'o IO.output -> ('a * 'b) -> unit

