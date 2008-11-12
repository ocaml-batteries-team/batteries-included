(* Batteries Included - Netchannels integration
 *
 * Copyright (C) 2008 Stefano Zacchiroli <zack@upsilon.cc>
 *           (C) 2006 Gerd Stolpmann
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

open ExtString

exception Input_not_available
exception Output_not_available

class type rec_in_channel = object
  method input : string -> int -> int -> int
  method close_in : unit -> unit
end

class type rec_out_channel = object
  method output : string -> int -> int -> int
  method flush : unit -> unit
  method close_out : unit -> unit
end

class type ['a] acc_out_channel = object
  inherit rec_out_channel
  method acc : 'a option
end

open Extlib

class netchannel_of_input inp =
object

  method input buf pos len =
    try
      IO.input inp buf pos len
    with IO.No_more_input -> raise End_of_file

  method close_in () = IO.close_in inp
    
end

class ['a] netchannel_of_output (out: 'a IO.output) =
object
  val mutable _acc = None
  method output = IO.output out
  method flush () = IO.flush out
  method close_out () = _acc <- Some (IO.close_out out)
  method acc = _acc
end

let buffer_size = 1024
  (** internal buffer used while reading from netchannels and
      delivering to Batteries channel *)

let input_of_netchannel netic =
  let minibuf = String.create 1 in
  let read () =
    try
      let bytes = netic # input minibuf 0 1 in
	if bytes = 0 then
	  raise Input_not_available
	else begin
	  assert (bytes = 1);
	  minibuf.[0]
	end
    with End_of_file (* | Netchannels.Closed_channel *) ->
      raise IO.No_more_input in
  let input buf pos len =
    try
      let bytes = netic # input buf pos len in
	if bytes = 0 then raise Input_not_available;
	bytes
    with End_of_file (* | Netchannels.Closed_channel *) ->
      raise IO.No_more_input in
  let close () = netic # close_in () in
    IO.create_in ~read ~input ~close

let output_of_netchannel netoc =
  let write ch =
    (* try *)
      let bytes = netoc # output (String.make 1 ch) 0 1 in
        if bytes = 0 then raise Output_not_available
    (* with Netchannels.Closed_channel -> raise Output_closed *)
  in
  let output buf pos len  =
    (* try *)
      netoc # output buf pos len
    (* with Netchannels.Closed_channel -> raise Output_closed *)
  in
  let flush = netoc # flush in
  let close () =
    (* try *)
      netoc # close_out ()
    (* with Netchannels.Closed_channel -> () *)
  in
    IO.create_out ~write ~output ~flush ~close

