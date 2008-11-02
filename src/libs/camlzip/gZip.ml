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

let uncompress input =
  let camlzip_in = ExtGzip.open_input input in
    IO.create_in
      ~read:(fun () -> ExtGzip.input_char camlzip_in)
      ~input:(ExtGzip.input camlzip_in)
      ~close:(fun () -> ExtGzip.close_in camlzip_in)

let gzip_compress ?level output =
  let camlzip_out = ExtGzip.open_output ?level output in
    IO.create_out
      ~write:(ExtGzip.output_char camlzip_out)
      ~output:(ExtGzip.output camlzip_out)
      ~flush:(fun () -> ExtGzip.flush camlzip_out)
      ~close:(fun () -> ExtGzip.close_out camlzip_out)

let compress output = gzip_compress ?level:None output

let open_in ?mode ?perm fname = uncompress (File.open_in ?mode ?perm fname)
let open_out ?mode ?perm fname = compress (File.open_out ?mode ?perm fname)

