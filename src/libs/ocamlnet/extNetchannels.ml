(* Batteries Included - Netchannels integration
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
 * USA
 *)

open Extlib
TYPE_CONV_PATH ""

module Netchannels =
struct

include Netchannels

class type ['a] acc_out_channel = object
  inherit rec_out_channel
  method accumulator : 'a option
end

class channel_of_input inp =
object

  method input buf pos len =
    try
      IO.input inp buf pos len
    with IO.No_more_input -> raise End_of_file

  method close_in () = IO.close_in inp
    
end

class ['a] channel_of_output (out: 'a IO.output) =
object
  val mutable _acc = None
  method output = IO.output out
  method flush () = IO.flush out
  method close_out () = _acc <- Some (IO.close_out out)
  method accumulator = _acc
end

let input_of_channel netic =
  let minibuf = String.create 1 in
  let read () =
    try
      let bytes = netic # input minibuf 0 1 in
	if bytes = 0 then
	  raise Sys_blocked_io
	else begin
	  assert (bytes = 1);
	  minibuf.[0]
	end
    with End_of_file | Netchannels.Closed_channel ->
      raise IO.No_more_input in
  let input buf pos len =
    try
      let bytes = netic # input buf pos len in
	if bytes = 0 then raise Sys_blocked_io;
	bytes
    with End_of_file | Netchannels.Closed_channel ->
      raise IO.No_more_input in
  let close () = netic # close_in () in
    IO.create_in ~read ~input ~close

let output_of_channel, output_of_acc_channel =
  let net_write netoc ch =
    try
      let bytes = netoc # output (String.make 1 ch) 0 1 in
	if bytes = 0 then raise Sys_blocked_io
    with Netchannels.Closed_channel -> raise IO.Output_closed in
  let net_output netoc buf pos len  =
    try
      netoc # output buf pos len
    with Netchannels.Closed_channel -> raise IO.Output_closed in
  let net_flush netoc = netoc # flush in
  let net_close netoc () =
    try
      netoc # close_out ()
    with Netchannels.Closed_channel -> () in
  let output_of_channel netoc =
    IO.create_out ~write:(net_write netoc) ~output:(net_output netoc)
      ~flush:(net_flush netoc) ~close:(net_close netoc)
  and output_of_acc_channel accoc =
    let close () =
      (try accoc # close_out () with Netchannels.Closed_channel -> ());
      Option.get (accoc # accumulator)
    in
      IO.create_out ~write:(net_write accoc) ~output:(net_output accoc)
	~flush:(net_flush accoc) ~close
  in
    (output_of_channel, output_of_acc_channel)

end
