(*
 * BatBufferIO - Circular byte buffer
 * Copyright (C) 2014 Simon Cruanes
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

(** Circular Byte Buffer for IO *)

type t = private {
  mutable start : int;
  mutable stop : int; (* excluded *)
  mutable buf : string;
}

exception Empty

val create : int -> t
(** [create size] creates a new buffer with given size *)

val copy : t -> t
(** fresh copy of the buffer *)

val of_string : string -> t
(** build a buffer from an initial string. The string is copied.
    Use {!String.blit_from} if you want more control. *)

val capacity : t -> int
(** length of the inner string buffer *)

val length : t -> int
(** number of bytes currently stored in the buffer *)

val blit_from : t -> string -> int -> int -> unit
(** [blit_from buf s o len] copies the slice [o, ... o + len - 1] from
    the string [s] to the end of the buffer.
    @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

val blit_into : t -> string -> int -> int -> int
(** [blit_into buf s o len] copies at most [len] bytes from [buf]
    into [s], starting at offset [o] in [s].
    @return the number of bytes actually copied ([min len (length buf)]).
    @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

val add_string : t -> string -> unit
(** [add_string buf s] adds [s] at the end of [buf]. *)

val to_string : t -> string
(** extract the current content into a string *)

val clear : t -> unit
(** clear the content of the buffer. Doesn't actually destroy the content. *)

val reset : t -> unit
(** clear the content of the buffer, and also resize it to a default size *)

val is_empty : t -> bool
(** is the buffer empty (i.e. contains no byte)? *)

val next : t -> char
(** obtain next char (the first one of the buffer)
    @raise Empty if the buffer is empty *)

val pop : t -> char
(** obtain and remove next char (the first one)
    @raise Empty if the buffer is empty *)

val junk : t -> unit
(** Drop next element.
    @raise Empty if the buffer is already empty *)

val skip : t -> int -> unit
(** [skip b len] removes [len] elements from [b].
    @raise Invalid_argument if [len > length b]. *)

val iteri : t -> (int -> char -> unit) -> unit
(** [iteri b f] calls [f i c] for each char [c] in [buf], with [i]
    being its relative index within [buf]. *)

val get : t -> int -> char
(** [get buf i] returns the [i]-th character of [buf], ie the one that
    is returned by [next buf] after [i-1] calls to [junk buf].
    @raise Invalid_argument if the index is invalid (> [length buf]) *)
