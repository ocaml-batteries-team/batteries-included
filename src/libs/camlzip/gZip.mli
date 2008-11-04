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

(** GZip - compression/decompression interface.

    This module provide access to GZip compression and decompression
    functionalities. Both the common (de)compression interface
    (implemented by all compression libraries available via Batteries)
    and GZip-specific functionalities are accessible using this
    module.

    @author Stefano Zacchiroli
*)

open Extlib

(** {6 Common decompression interface} *)

(* XXX this should be a module include, but currently it does not work
   well with ocamldoc. Hence for the moment we copy&paste from
   Compress' .mli :-(
   Try with CamlP4's INCLUDE macro. *)
(* include Common.Compress.Decompressor *)

val uncompress: IO.input -> IO.input
  (** Wrap an input channel, decompressing transparently data when
      reading from it.
      
      Operations performed on the returned channel can raise, in
      addition to their usual exceptions, [Error]. *)

val open_in: ?mode:File.open_in_flag list -> ?perm:File.permission ->
  string ->
  IO.input
    (** Shorthand: directly open a compressed file to read from it See
	[File.open_in] *)

(** {6 Common compression interface} *)

(* XXX this should be a module include, see above *)
(* include Common.Compress.Compressor *)

val compress: 'a IO.output -> 'a IO.output
  (** Wrap an output channel, compressing transparently data when
      writing to it.
      
      Operations performed on the returned channel can raise, in
      addition to their usual exceptions, [Error]. *)

val open_out: ?mode:File.open_out_flag list -> ?perm:File.permission ->
  string ->
  unit IO.output
    (** Shorthand: directly open a compressed file to write to it.
	See [File.open_out] *)

(** {6 GZip-specific features}

    Provide acces to GZip-specific features.
*)

val gzip_compress: ?level:int -> 'a IO.output -> 'a IO.output
  (** gzip-specific compression function, same as [GZip.compress], but
      enable to specifiy gzip-specific compression parameters

      @param level compression level (an integer between 1 and 9),
      with 1 being the weakest (but fastest) compression and 9 being
      the strongest (but slowest) compression. Default: 6 *)
