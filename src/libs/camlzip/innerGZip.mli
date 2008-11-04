(* Batteries Included - (De)Compression modules
 * 
 * Copyright (C) 2001 Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *           (C) 2008 Stefano Zacchiroli <zack@upsilon.cc>
 * 
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of the
 * License, or (at your option) any later version, with the special
 * exception on linking described in file LICENSE.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA *)

(** This module mimics the part of Camlzip's [Gzip] API about
    channels, but working on Batteries' input/output channels instead
    of [Pervasives]'s channels.

    @author Stefano Zacchiroli *)

open Extlib

type in_channel
val open_input: IO.input-> in_channel
val input_char: in_channel -> char
val input_byte: in_channel -> int
val input: in_channel -> string -> int -> int -> int
val really_input: in_channel -> string -> int -> int -> unit
val close_in: in_channel -> unit
val dispose: in_channel -> unit

type 'a out_channel
val open_output: ?level:int -> 'a IO.output -> 'a out_channel
val output_char: 'a out_channel -> char -> unit
val output_byte: 'a out_channel -> int -> unit
val output: 'a out_channel -> string -> int -> int -> int
val close_out: 'a out_channel -> 'a
val flush: 'a out_channel -> unit
