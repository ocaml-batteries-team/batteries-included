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
| `Date (** Print the current date as 2011/0628 *)
| `Time (** Print the current time as 01:23:45 *)
| `Filepos (** Print the file and position of this log command (UNIMPLEMENTED) *)
| `Custom of unit -> string (** Print the results of running the given closure *)
]
val get_flags : unit -> flag list
val set_flags : flag list -> unit

val print : ?fp:string -> string -> unit
(** [print s] logs the message s, returning unit. *)

val printf: ?fp:string -> ('a, unit output, unit) Pervasives.format -> 'a
(** As [Printf.printf], only the message is printed to the logging
    output and prefixed with status information per the current flags and
    the currently set prefix. *)

val fatal : ?fp:string -> string -> 'a
(** [fatal s] logs the message [s] and then calls [exit 1].  This
    exits the program with return code 1.  *)

val fatalf: ?fp:string -> ('a, unit output, unit) Pervasives.format -> 'a
(** [fatalf] allows a format string (as [Printf.printf])and the
    arguments to that format string to build the logging message.
    Exits the program with return code 1. *)


module type Config = sig
  type t
  val out: t output
  val prefix: string
  val flags: flag list
end


(** Build a logger module with custom, fixed output, prefix and flags *)
module Make (S:Config) : sig
  val print : ?fp:string -> string -> unit
  (** [print s] logs the message s, returning unit. *)

  val printf: ?fp:string -> ('a, S.t output, unit) Pervasives.format -> 'a
  (** As [Printf.printf], only the message is printed to the logging
      output and prefixed with status information per the current flags and
      the currently set prefix. *)

  val fatal : ?fp:string -> string -> 'a
  (** [fatal s] logs the message [s] and then calls [exit 1].  This
      exits the program with return code 1.  *)

  val fatalf: ?fp:string -> ('a, S.t output, unit) Pervasives.format -> 'a
(** [fatalf] allows a format string (as [Printf.printf])and the
    arguments to that format string to build the logging message.
    Exits the program with return code 1. *)

end

(* Returns an object with methods [fatal], [fatalf], [print], and [printf] that logs to the given output channel, with given prefix and flags. *)
val make_logger :
  'a BatInnerIO.output ->
  string ->
  [< `Custom of unit -> string | `Date | `Filepos | `Time ] list ->
  < fatal : ?fp:string -> string -> 'b;
    fatalf : ?fp:string ->
             ('c, 'a BatInnerIO.output, unit, unit, unit, 'd) format6 -> 'c;
    print : ?fp:string -> string -> unit;
    printf : ?fp:string -> ('e, 'a BatInnerIO.output, unit) BatPrintf.t -> 'e >

module type Level_sig = sig
  type t
  val to_string : t -> string
  val default_level : t
  val compare : t -> t -> int
end

module Make_lev(L:Level_sig)(S:Config) : sig
  val level : L.t ref
  val log : L.t -> string -> unit
  val logf : L.t -> ('a, S.t output, unit) Pervasives.format -> 'a
end

type easy_lev = [ `trace | `debug | `info | `warn | `error | `fatal | `always ]
module Easy : sig
  val level : easy_lev ref
  val log : easy_lev -> string -> unit
  val logf : easy_lev -> ('a, unit output, unit) Pervasives.format -> 'a
end
