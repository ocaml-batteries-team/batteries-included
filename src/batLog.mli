(* 
 * BatLog - Simple Logging module
 * Copyright (C) 2011 The Batteries Included Team
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

(** Simple logging 
    @author Edgar Friendly
*)
open BatIO

(** Get and set the default output channel for simple logging.
    Defaults to stderr *)
val get_output : unit -> unit output
val set_output : unit output -> unit

(** Get and set the text printed before each log message.  Defaults to
    the empty string. *)
val get_prefix : unit -> string
val set_prefix : string -> unit

(** Get and set the output flags.  These flags control how the log
    messages are output.  The default is [`Date; `Time] and log
    messages are printed as:
    
    2011/0628 01:23:45: prefixmessage
 *)
type flag = [
| `Date (* Print the current date as 2011/0628 *)
| `Time (* Print the current time as 01:23:45 *)
| `Filepos (* Print the file and position of this log command (UNIMPLEMENTED) *)
]
val get_flags : unit -> flag list
val set_flags : flag list -> unit    

val print : string -> unit
(** [print s] logs the message s, returning unit. *)

val printf: ('a, unit output, unit) Pervasives.format -> 'a
(** As [Printf.printf], only the message is printed to the logging
    output and prefixed with status information per the current flags and
    the currently set prefix. *)

val fatal : string -> 'a
(** [fatal s] logs the message [s] and then calls [exit 1].  This
    exits the program with return code 1.  *)

val fatalf: ('a, unit output, unit) Pervasives.format -> 'a
(** [fatalf] allows a format string (as [Printf.printf])and the
    arguments to that format string to build the logging message.
    Exits the program with return code 1. *)


module type S = sig
  val out: 'a output
  val prefix: string
  val flags: flag list
end

module Make (S:S) : sig
  val print : string -> unit
  (** [print s] logs the message s, returning unit. *)

  val printf: ('a, 'b output, unit) Pervasives.format -> 'a
  (** As [Printf.printf], only the message is printed to the logging
      output and prefixed with status information per the current flags and
      the currently set prefix. *)

  val fatal : string -> 'a
  (** [fatal s] logs the message [s] and then calls [exit 1].  This
      exits the program with return code 1.  *)

  val fatalf: ('a, 'a output, unit) Pervasives.format -> 'a
(** [fatalf] allows a format string (as [Printf.printf])and the
    arguments to that format string to build the logging message.
    Exits the program with return code 1. *)

end

type 'a logger = {
  print : string -> unit;
  printf : 'b. ('b, 'a output, unit) Pervasives.format -> 'b;
  fatal: string -> 'a;
  fatalf: 'b. ('b, 'a output, unit) Pervasives.format -> 'b;
}

val make_logger : 'a output -> string -> flag list -> 'a logger
