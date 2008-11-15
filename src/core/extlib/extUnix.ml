(*
 * ExtUnix - additional and modified functions for Unix and Unix-compatible systems.
 * Copyright (C) 1996 Xavier Leroy
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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

TYPE_CONV_PATH "Batteries.System" (*For Sexplib, Bin-prot...*)

module Unix =
struct
  include Unix
  open InnerIO
  open Std

  (**
     {6 Thread-safety internals}
  *)
  let lock = ref Concurrent.nolock

  (**
     {6 Tracking additional information on inputs/outputs}

     {b Note} Having [input]/[output] as objects would have made this
     easier. Here, we need to maintain an external weak hashtable to
     track low-level information on our [input]s/[output]s.
  *)

  module Wrapped_in = InnerWeaktbl.Make(Input) (*input  -> in_channel *)
  module Wrapped_out= InnerWeaktbl.Make(Output)(*output -> out_channel*)
  let wrapped_in    = Wrapped_in.create 16
  let wrapped_out   = Wrapped_out.create 16

  let input_add k v =
    Concurrent.sync !lock (Wrapped_in.add wrapped_in k) v
      
  let input_get k =
    Concurrent.sync !lock (Wrapped_in.find wrapped_in) k

  let output_add k v =
    Concurrent.sync !lock (Wrapped_out.add wrapped_out k) v
      
  let output_get k =
    Concurrent.sync !lock (Wrapped_out.find wrapped_out) k

  let wrap_in ?autoclose cin =
    let input = InnerIO.input_channel ?autoclose cin in
      Concurrent.sync !lock (Wrapped_in.add wrapped_in input) cin;
      input

  let wrap_out cout =
    let output = cast_output (InnerIO.output_channel cout) in
      Concurrent.sync !lock (Wrapped_out.add wrapped_out output) cout;
      output

  (**
     {6 File descriptors}
  *)

  let input_of_descr ?autoclose fd =
    wrap_in ?autoclose (in_channel_of_descr fd)

  let descr_of_input cin =
    try  descr_of_in_channel (input_get cin)
    with Not_found -> raise (Invalid_argument "Unix.descr_of_in_channel")

  let output_of_descr fd =
    wrap_out (out_channel_of_descr fd)

  let descr_of_output cout =
    try  descr_of_out_channel (output_get (cast_output cout))
    with Not_found -> raise (Invalid_argument "Unix.descr_of_out_channel")

  let in_channel_of_descr fd    = input_of_descr fd
  let descr_of_in_channel       = descr_of_input
  let out_channel_of_descr      = output_of_descr
  let descr_of_out_channel      = descr_of_output


  (**
     {6 Processes}
  *)

  let open_process_in ?autoclose s =
    wrap_in ?autoclose (open_process_in s)

  let open_process_out s =
    wrap_out (open_process_out s)

  let open_process ?autoclose s =
    let (cin, cout) = open_process s in
      (wrap_in ?autoclose cin, wrap_out cout)

  let open_process_full ?autoclose s args =
    let (a,b,c) = open_process_full s args in
      (wrap_in ?autoclose a, wrap_out b, wrap_in ?autoclose c)

  let close_process_in cin =
    try close_process_in (input_get cin)
    with Not_found -> raise (Unix_error(EBADF, "close_process_in", ""))

  let close_process_out cout =
    try close_process_out (output_get cout)
    with Not_found -> raise (Unix_error(EBADF, "close_process_out", ""))

  let close_process (cin, cout) =
    try close_process (input_get cin, output_get cout)
    with Not_found -> raise (Unix_error(EBADF, "close_process", ""))

  let close_process_full (cin, cout, cin2) =
    try close_process_full (input_get cin, output_get cout, input_get cin2)
    with Not_found -> raise (Unix_error(EBADF, "close_process_full", ""))

(**
   {6 Network}
*)
      
  let open_connection ?autoclose addr =
    wrap_in ?autoclose *** wrap_out  <| open_connection addr

  let shutdown_connection cin = 
    try shutdown_connection (input_get cin)
    with Not_found -> raise (Invalid_argument "Unix.descr_of_in_channel")

  let establish_server ?autoclose f addr =
    let f' cin cout = f (wrap_in ?autoclose cin) (wrap_out cout) in
      establish_server f' addr

  let is_directory fn = (lstat fn).st_kind = S_DIR

      
end
