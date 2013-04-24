(*
 * BatUnix - additional and modified functions for Unix and Unix-compatible systems.
 * Copyright (C) 1996 Xavier Leroy
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
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


include Unix
open BatInnerIO


(**
   {6 Thread-safety internals}
*)
let lock = ref BatConcurrent.nolock

(**
   {6 Tracking additional information on inputs/outputs}

   {b Note} Having [input]/[output] as objects would have made this
   easier. Here, we need to maintain an external weak hashtable to
   track low-level information on our [input]s/[output]s.
*)

module Wrapped_in  = BatInnerWeaktbl.Make(Input) (*input  -> in_channel *)
module Wrapped_out = BatInnerWeaktbl.Make(Output)(*output -> out_channel*)
let wrapped_in     = Wrapped_in.create 16
let wrapped_out    = Wrapped_out.create 16

let input_add k v =
  BatConcurrent.sync !lock (Wrapped_in.add wrapped_in k) v

let input_get k =
  BatConcurrent.sync !lock (Wrapped_in.find wrapped_in) k

let output_add k v =
  BatConcurrent.sync !lock (Wrapped_out.add wrapped_out k) v

let output_get k =
  BatConcurrent.sync !lock (Wrapped_out.find wrapped_out) k

let wrap_in ?autoclose ?cleanup cin =
  let input = BatInnerIO.input_channel ?autoclose ?cleanup cin in
  BatConcurrent.sync !lock (Wrapped_in.add wrapped_in input) cin;
  input

let wrap_out ?cleanup cout =
  let output = cast_output (BatInnerIO.output_channel ?cleanup cout) in
  BatConcurrent.sync !lock (Wrapped_out.add wrapped_out output) cout;
  output

let _ =
  input_add stdin Pervasives.stdin;
  output_add stdout Pervasives.stdout;
  output_add stderr Pervasives.stderr

(**
   {6 File descriptors}
*)

let input_of_descr ?autoclose ?cleanup fd =
  wrap_in ?autoclose ?cleanup (in_channel_of_descr fd)

let descr_of_input cin =
  try  descr_of_in_channel (input_get cin)
  with Not_found -> raise (Invalid_argument "Unix.descr_of_in_channel")

let output_of_descr ?cleanup fd =
  wrap_out ?cleanup (out_channel_of_descr fd)

let descr_of_output cout =
  try  descr_of_out_channel (output_get (cast_output cout))
  with Not_found -> raise (Invalid_argument "Unix.descr_of_out_channel")

let in_channel_of_descr fd = input_of_descr ~autoclose:false ~cleanup:true fd
let descr_of_in_channel = descr_of_input
let out_channel_of_descr fd = output_of_descr  ~cleanup:true fd
let descr_of_out_channel = descr_of_output


(**
   {6 Processes}
*)

let open_process_in ?autoclose ?(cleanup=true) s =
  wrap_in ?autoclose ~cleanup (open_process_in s)

let open_process_out ?(cleanup=true) s =
  wrap_out ~cleanup (open_process_out s)

let open_process ?autoclose ?(cleanup=true) s =
  let (cin, cout) = open_process s in
  (wrap_in ?autoclose cin, wrap_out ~cleanup cout)

(*$T open_process
  let s = "hello world" in let r,w = open_process "cat" in \
  Printf.fprintf w "%s\n" s; IO.close_out w; \
  IO.read_line r = s

  try \
    let r,w = open_process "cat" in \
      Printf.fprintf w "hello world\n"; \
      IO.close_out w; \
      while true do ignore (input_char r) done; false \
  with e -> e=IO.No_more_input || e=End_of_file
*)


let open_process_full ?autoclose ?(cleanup=true) s args =
  let (a,b,c) = open_process_full s args in
  (wrap_in ?autoclose ~cleanup a, wrap_out ~cleanup b, wrap_in ?autoclose ~cleanup c)

(**@TODO in a future version, [close_process_in] should also work
   on processes opened with [open_process] or [open_process_full].
   Same thing for [close_process_out].*)

let close_process_in cin =
  try close_process_in (input_get cin)
  with Not_found -> raise (Unix_error(EBADF, "close_process_in", ""))

let close_process_out cout =
  try close_process_out (output_get cout)
  with Not_found -> raise (Unix_error(EBADF, "close_process_out", ""))

let close_process (cin, cout) =
  try
    let pin = input_get cin
    and pout= output_get cout in
    close_process (pin, pout)
  with Not_found -> raise (Unix_error(EBADF, "close_process", ""))

let close_process_full (cin, cout, cin2) =
  try close_process_full (input_get cin, output_get cout, input_get cin2)
  with Not_found -> raise (Unix_error(EBADF, "close_process_full", ""))

(**
   {6 Network}
*)

let shutdown_connection cin =
  try shutdown_connection (input_get cin)
  with Not_found -> raise (Invalid_argument "Unix.descr_of_in_channel")

let open_connection ?autoclose addr =
  let (cin, cout) = open_connection addr in
  let (cin',cout')= (wrap_in ?autoclose ~cleanup:true cin, wrap_out ~cleanup:true cout) in
  let close ()    = shutdown_connection cin' in
  (inherit_in  cin' ~close, inherit_out cout' ~close)

let establish_server ?autoclose ?cleanup f addr =
  let f' cin cout = f (wrap_in ?autoclose ?cleanup cin) (wrap_out cout) in
  establish_server f' addr

(**
   {6 Tools}
*)

let is_directory fn = (lstat fn).st_kind = S_DIR

let rec restart_on_EINTR f x =
  try f x
  with Unix_error(EINTR, _, _) -> restart_on_EINTR f x
