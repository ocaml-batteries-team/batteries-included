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

(** This ref holds the output channel for simple logging.  Defaults to
    [stderr]

    @since 2.0; had getter and setter in 1.x
*)
val output : unit output ref

(** This ref holds the text printed before each log message.  Defaults to
    the empty string.

    @since 2.0; had getter and setter in 1.x
*)
val prefix : string ref

type flag = [
  | `Date (** Print the current date as 2011-06-28 *)
  | `Time (** Print the current time as 01:23:45 *)
  | `Filepos (** Print the file and linenum of this log command (UNIMPLEMENTED - needs syntax extension) *)
  | `Custom of unit -> string (** Print the results of running the given closure *)
]

(** This ref holds the output flags.  These flags control how the log
    messages are output.  The default is [`Date; `Time] and log
    messages are printed as:

    2011/0628 01:23:45: prefixmessage

    @since 2.0; had getter and setter in 1.x
*)
val flags : flag list ref

(** [log s] logs the message s, returning unit.

    @since 2.0; was [print] in 1.x
*)
val log : ?fp:string -> string -> unit


(** As [Printf.printf], only the message is printed to the logging
    output and prefixed with status information per the current flags and
    the currently set prefix.

    @since 2.0; was [printf] in 1.x
*)
val logf: ?fp:string -> ('a, unit output, unit) Pervasives.format -> 'a

(** [fatal s] logs the message [s] and then calls [exit 1].  This
    exits the program with return code 1.  *)
val fatal : ?fp:string -> string -> 'a

(** [fatalf] allows a format string (as [Printf.printf])and the
    arguments to that format string to build the logging message.
    Exits the program with return code 1. *)
val fatalf: ?fp:string -> ('a, unit output, unit) Pervasives.format -> 'a

module type Config = sig
  type t
  val out: t output
  val prefix: string
  val flags: flag list
end

(** Build a logger module with custom, fixed output, prefix and flags *)
module Make (S:Config) : sig
  (** [print s] logs the message s, returning unit. *)
  val log : ?fp:string -> string -> unit

  (** As [Printf.printf], only the message is printed to the logging
      output and prefixed with status information per the current flags and
      the currently set prefix. *)
  val logf: ?fp:string -> ('a, S.t output, unit) Pervasives.format -> 'a

  (** [fatal s] logs the message [s] and then calls [exit 1].  This
      exits the program with return code 1.  *)
  val fatal : ?fp:string -> string -> 'a

  (** [fatalf] allows a format string (as [Printf.printf])and the
      arguments to that format string to build the logging message.
      Exits the program with return code 1. *)
  val fatalf: ?fp:string -> ('a, S.t output, unit) Pervasives.format -> 'a

end

(** Returns an object with methods [fatal], [fatalf], [log], and
    [logf] that logs to the given output channel, with given prefix
    and flags.  These methods work like the corresponding functions in
    the BatLog module.

    @since 2.0
*)

val make_logger :
  'a output ->
  string ->
  [< `Custom of unit -> string | `Date | `Filepos | `Time ] list ->
  < fatal : ?fp:string -> string -> 'b;
    fatalf : ?fp:string ->
      ('c, 'a output, unit, unit, unit, 'd) format6 -> 'c;
    log : ?fp:string -> string -> unit;
    logf : ?fp:string -> ('e, 'a output, unit) format -> 'e >

        (** The different verbosity levels supported in the [Easy] logger *)
type easy_lev = [ `trace | `debug | `info | `warn | `error | `fatal | `always ]

(** A simple-to-use logger with verbosity levels that outputs by
    default to stderr (changeable at runtime) with the date and time
    at the beginning of each log message.

    @since 2.0
*)
module Easy : sig
  (** Set this ref to the lowest level of log you want logged.  For
      example, [Easy.level := `always] disables all logging except
      that at the [`always] level.  Setting [Easy.level := `info] will
      enable logging for [`info], [`warn], [`error], [`fatal] and
      [`always] levels. *)
  val level : easy_lev ref
  (** Set this ref to the output you want logging messages to go
      to.  Defaults to [stderr]. *)
  val output : unit output ref

  (** [log lev msg] logs the message [msg] if the current logging
      level is [lev] or lower.  *)
  val log : ?fp:string -> easy_lev -> string -> unit

  (** As [log], but instead of a string message, a printf format is
      allowed with whatever arguments are appropriate. *)
  val logf : ?fp:string -> easy_lev -> ('a, unit output, unit) format -> 'a
end


(** The details of a level scheme for verbosity-level loggers *)
module type Level_sig = sig
  (** A type for level values, usually a polymorphic variant *)
  type t
  (** Convert each level to a string *)
  val to_string : t -> string
  (** The default level for loggers created with this; log messages
      with level less than this won't be printed by default. *)
  val default_level : t
  (** a comparison function between levels, to know whether logging at a
      particular level should be printed *)
  val compare : t -> t -> int
end

(** Make your own level-based logger, like [Easy] *)
module Make_lev(L:Level_sig)(S:Config) : sig
  val level : L.t ref
  val output : S.t output ref
  val log : ?fp:string -> L.t -> string -> unit
  val logf : ?fp:string -> L.t -> ('a, S.t output, unit) format -> 'a
end
