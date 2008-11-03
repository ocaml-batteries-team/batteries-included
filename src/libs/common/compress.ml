(* Batteries Included - (De)Compression modules
 * 
 * Copyright (C) 2008 Stefano Zacchiroli <zack@upsilon.cc>
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

open Extlib

exception Compress_error of string * exn option
  (** Error while compressing/decompressing.

      First argument is a human-readable error explanation.  Second
      argument is the low-level exception raised by the underlying
      (de)compression library, if any.*)

module type Decompressor =
sig

  val uncompress: IO.input -> IO.input
    (** Wrap an input channel, decompressing transparently data when
	reading from it.

	Operations performed on the returned channel can raise, in
	addition to their usual exceptions, [Error]. *)

  val open_in: ?mode:File.open_in_flag list -> ?perm:File.permission ->
    string ->
    IO.input
      (** Shorthand: directly open a compressed file to read from it
	  See [File.open_in] *)

end

module type Compressor =
sig

  val compress: 'a IO.output -> 'a IO.output
    (** wrap an output channel, compressing transparently data when
	writing to it.
	
	Operations performed on the returned channel can raise, in
	addition to their usual exceptions, [Error]. *)

  val open_out: ?mode:File.open_out_flag list -> ?perm:File.permission ->
    string ->
    unit IO.output
      (** Shorthand: directly open a compressed file to write to it.
	  See [File.open_out] *)

end
