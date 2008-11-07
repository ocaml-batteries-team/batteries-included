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

open Common
open Extlib
open IO
  

(* XXX: it has to be checked how costly is wrapping each of the
arguments of IO.create_{in,out} with exception handlers is. Currently,
this is done for the sake of being sure that Compression_error is
always raised, and this is the only way to do that staying at the
OCaml level, because Zlib.Error is also raised from C bindings.

The only alternative is adding hooks into IO.create_{in,out} which let
specifying extensible exception handlers. *)

let uncompress input =
  let error exn =
    raise (Compress.Compression_error ("zlib uncompression error", Some exn)) in
  let camlzip_in = InnerGZip.open_input input in
  let read () =
    try InnerGZip.input_char camlzip_in
    with Zlib.Error _ as exn -> error exn in
  let input buf pos len =
    try InnerGZip.input camlzip_in buf pos len
    with Zlib.Error _ as exn -> error exn in
  let close () =
    try InnerGZip.close_input camlzip_in
    with Zlib.Error _ as exn -> error exn
  in
    IO.create_in ~read ~input ~close

let gzip_compress ?level out =
  let error exn =
    raise (Compress.Compression_error ("zlib compression error", Some exn)) in
  let camlzip_out = InnerGZip.open_output ?level (cast_output out) in
  let write c =
    try InnerGZip.output_char camlzip_out c
    with Zlib.Error _ as exn -> error exn in
  let output buf pos len =
    try InnerGZip.output camlzip_out buf pos len
    with Zlib.Error _ as exn -> error exn in
  let flush = ignore in
    (* Rationale: gzip format is CRC-terminated, flushing is
    meaningless, flushed files w/o CRC are not readable anyhow. *)
  let close () =
    try InnerGZip.close_output camlzip_out
    with Zlib.Error _ as exn -> error exn
  in
    IO.wrap_out ~write ~output ~flush ~close ~underlying:[out]

let compress output = gzip_compress ?level:None output

let open_in  ?mode ?perm fname = uncompress (File.open_in ?mode ?perm fname)
let open_out ?mode ?perm fname = compress (File.open_out ?mode ?perm fname)

let with_in inp f =
  let input = uncompress inp in
  Std.finally (fun () -> close_in input)
    f input

let with_out out f =
  let output = compress out in
  Std.finally (fun () -> close_out output)
    f output
