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

exception Input_not_available
  (** raised by input channels created from non-blocking Netchannels
      to indicate that no input is {e currently} available (it might
      become available later though) *)

exception Output_not_available
  (** raised by output channels created from non-blocking Netchannels
      to indicate that no output can {e currently} be output (it might
      be possible to output it later though) *)

open Extlib
open IO

(** {6 Basic I/O class types for library interoperability} *)


(** This class type is defined in
    "{{:http://www.ocaml-programming.de/rec/IO-Classes.html}Basic I/O
    class types}" as collaborative effort of several library
    creators.  *)
class type rec_in_channel = object

  method input : string -> int -> int -> int
    (** Reads octets from the channel and puts them into the string.

	The first [int] argument is the position of the substring, and
	the second [int] argument is the length of the substring where
	the data are stored. The method returns the number of octets
	actually read and stored.
	
	When the end of the channel is reached and there is no further
	octet to read, the exception [End_of_file] will be raised. ({b
	Note}: this is in contrast with {!System.IO.No_more_input}
	which is used elsewhere in Batteries. The rationale for the
	difference is that netchannels produced from Batteries are
	meant to be consumed by other libraries, which expect
	[End_of_file] as documented in the Basic I/O standard.)
	
	When the channel is non-blocking, and there are currently no
	bytes to read, the number 0 will be returned.
	
	When the channel is closed, the exception
	{!System.IO.Input_closed} will be raised. ({b Note}: this is
	specific to Batteries, the Basic I/O recommendation does not
	have a standard for that.) *)

  method close_in : unit -> unit
    (** Closes the channel for input.

	In case the netchannel has been created out of a
	{!System.IO.input} channel, that channel gets closed as well,
	and can no longer be used.

	When the channel is closed, no exception will be raised (i.e.,
	[close_in] is idempotent). ({b Note}: this is specific to
	Batteries, the Basic I/O recommendation does not have a
	standard for that.) *)

end

(** This class type is defined in
    "{{:http://www.ocaml-programming.de/rec/IO-Classes.html}Basic I/O
    class types}" as collaborative effort of several library
    creators.  *)
class type rec_out_channel = object

  method output : string -> int -> int -> int
    (** Takes octets from the string and writes them into the
	channel. The first [int] argument is the position of the
	substring, and the second [int] argument is the length of the
	substring where the data can be found. The method returns the
	number of octets actually written.
	
	The implementation may choose to collect written octets in a
	buffer before they actually delivered to the underlying
	resource.
	
	When the channel is non-blocking, and there are currently no
	bytes to write, the number 0 will be returned.
	
	When the channel is closed, the exception [Closed_channel]
	will be raised if an ocamlnet implementation is used. For
	implementations of other libraries there is no standard for
	this case.  *)


  method flush : unit -> unit
    (** If there is a write buffer, it will be flushed. Otherwise,
	nothing happens *)

  method close_out : unit -> unit
    (** Flushes the buffer, if any, and closes the channel for output.
	
	When the channel is already closed, the exception
	[Closed_channel] will be raised if an ocamlnet implementation
	is used. For implementations of other libraries there is no
	standard for this case.  *)

end

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

(** TODO document this *)
(* val output_of_acc_channel : 'a acc_out_channel -> 'a IO.output *)

