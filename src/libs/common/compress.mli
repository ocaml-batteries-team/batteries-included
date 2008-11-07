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

(** Common compression/decompression interfaces.

    This module provides abstract interfaces for manipulating
    compressed data. The interfaces are abstract in the sense that
    they are not specific of any underlying (de)compression
    libraries. Each of such library integrated with Batteries is
    required to implement the abstract interfaces. This way, switching
    from one compression library to another should be as easy as
    switching module name.

    Library-specific features can be provided by offering additional
    functions with respect to the common abstract interfaces.

    @author Stefano Zacchiroli
*)

open Extlib

exception Compression_error of string * exn option
  (** Error while compressing/decompressing.

      First argument is a human-readable error explanation.  Second
      argument is the low-level exception raised by the underlying
      (de)compression library, if any. *)

(** Common interface for decompressing (i.e., inflating) data. *)
module type Decompressor =
sig

  val uncompress: IO.input -> IO.input
    (** Wrap an input channel, decompressing transparently data when
	reading from it.

	Operations performed on the returned channel can raise, in
	addition to their usual exceptions,
	{!Common.Compress.Compression_error}. *)

  val open_in: ?mode:File.open_in_flag list -> ?perm:File.permission ->
    string ->
    IO.input
      (** Shorthand: directly open a compressed file to read from it
	  See {!File.open_in} *)

      val with_in: IO.input -> (IO.input -> 'a) -> 'a
  (** [with_in input f] creates a new input [input'] which will
      transparently decompress data from [input], then invokes [f
      input'] to process that new input. Once [f] has returned or
      triggered an exception, the [input'] is closed before
      proceeding. *)
	
end

(** Common interface for compressing (i.e., deflating) data. *)
module type Compressor =
sig

  val compress: 'a IO.output -> 'a IO.output
    (** Wrap an output channel, compressing transparently data when
	writing to it.
	
	Operations performed on the returned channel can raise, in
	addition to their usual exceptions,
	{!Common.Compress.Compression_error}. *)

  val open_out: ?mode:File.open_out_flag list -> ?perm:File.permission ->
    string ->
    unit IO.output
      (** Shorthand: directly open a compressed file to write to it.
	  See {!File.open_out} *)

  val with_out: unit IO.output -> (unit IO.output -> 'a) -> 'a
    (** [with_out output f] first creates a new output [output'] which will
	transparently compress data to [output] and then invokes [f output'].
	
	Once [f output'] has returned or triggered an exception,
	[output'] is closed before proceeding. *)

end
