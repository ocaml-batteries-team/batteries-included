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

(* let io_of_input rec_ic = *)
(*   IO.create_in ~read ~input ~close *)

(* val io_of_output : rec_out_channel -> IO.input *)

