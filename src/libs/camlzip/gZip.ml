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
open IO
let uncompress input =
  let camlzip_in = InnerGZip.open_input input in
    IO.create_in
      ~read:(fun () -> InnerGZip.input_char camlzip_in)
      ~input:(InnerGZip.input camlzip_in)
      ~close:(fun () -> InnerGZip.close_in camlzip_in)

let gzip_compress ?level output =
  let camlzip_out = InnerGZip.open_output ?level output in
    IO.create_out
      ~write:(InnerGZip.output_char camlzip_out)
      ~output:(InnerGZip.output camlzip_out)
      ~flush:(fun () -> InnerGZip.flush camlzip_out)
      ~close:(fun () -> InnerGZip.close_out camlzip_out)

let compress output = gzip_compress ?level:None output

let open_in  ?mode ?perm fname = uncompress (File.open_in ?mode ?perm fname)
let open_out ?mode ?perm fname = compress (File.open_out ?mode ?perm fname)

let with_in inp f =
  let input = uncompress inp in
  Std.finally (fun () -> close_in input)
    f input

let with_out out f =
  let output = compress out in
  (*Std.finally (fun () -> close_out output)*)
    Std.finally (fun () -> flush output)
    f output
