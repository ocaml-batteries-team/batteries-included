(*
 * BatCounter
 * Copyright (C) 2011 Edgar Friendly <thelema314@gmail.com>
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

(* Inspired by Coda Hale's Metrics *)

(** Counter metrics are for keeping track of how many of something
    there are, being incremented and decremented (and cleared,
    occasionally). This implementation guarantees a range of values no
    smaller than the ocaml [int] type.

    Thread Safety is planned with the introduction of an AtomicInt
    datatype.
 *)


(** The type of a counter metric *)
type t

(** Make a new counter *)
val make : unit -> t

(** Increment and add an integer to the counter *)
val inc : t -> unit
val add : t -> int -> unit

(* Decrement and subtract an integer from the counter *)
val dec : t -> unit
val sub : t -> int -> unit

(* get the current counter value *)
val count : t -> int
(* clear the current counter value *)
val clear : t -> unit
(* Atomically clear the counter and return its old value *)
val get_and_clear : t -> int
