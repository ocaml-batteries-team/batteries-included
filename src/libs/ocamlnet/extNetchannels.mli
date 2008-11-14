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

(** TODO understand what to do with the .mli of this module. It
    extends ocamlnet's Netchannels, should we inline all the .mli of
    Netchannels or do we have a better solution for that? *)

open Extlib

module Netchannels :
sig

include Netchannels_modtype.Netchannels

(** {6 Basic I/O class types for library interoperability} *)

(** TODO document this

    This class can be coerced to a [rec_out_channel] using [acc_oc :>
    rec_out_channel], where [acc_oc] is an [acc_out_channel]
    instance. *)
class type ['a] acc_out_channel = object
  inherit rec_out_channel

  (** @return [Some acc] where [acc] is the accumulator data collected
      when the {!System.IO.output} channel used to create this
      Netchannel was closed; return [None] if the channel hasn't been
      closed yet. *)
  method acc : 'a option
end

(** {6 Integration among IO channels and Netchannels} *)

(** TODO document this *)
class netchannel_of_input : IO.input -> rec_in_channel

(** TODO document this *)
class ['a] netchannel_of_output : 'a IO.output -> ['a] acc_out_channel
  
(** TODO document this *)
val input_of_netchannel : rec_in_channel -> IO.input

(** TODO document this *)
val output_of_netchannel : rec_out_channel -> unit IO.output

(** TODO document this

    @raise Data.Persistent.Option.No_value upon [close] if the
    underlying channel did not set the value accumulated during
    output. See {!acc_out_channel}. *)
val output_of_acc_channel : 'a acc_out_channel -> 'a IO.output

end
