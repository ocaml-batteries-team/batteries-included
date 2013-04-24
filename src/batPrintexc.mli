(*
 * BatPrintexc - Extended Printexc module
 * Copyright (C) 1996 Xavier Leroy
 *               2008 David Teller, LIFO, Universite d'Orleans
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


(** Facilities for printing exceptions.

    @author Xavier Leroy (Base module)
    @author David Teller
*)

val pass : ('a -> 'b) -> 'a -> 'b
(** [Printexc.pass fn x] applies [fn] to [x] and returns the result.
    If the evaluation of [fn x] raises any exception, the name of the
    exception is printed on standard error output, and the exception
    is raised again.  The typical use is to catch and report
    exceptions that escape a function application. This function is a
    renamed version of [Printexc.print] from stdlib.*)

val catch: ('a -> 'b) -> 'a -> 'b
(** [Printexc.catch fn x] is similar to {!Printexc.print}, but aborts
    the program with exit code 2 after printing the uncaught exception.
    This function is deprecated: the runtime system is now able to
    print uncaught exceptions as precisely as [Printexc.catch] does.
    Moreover, calling [Printexc.catch] makes it harder to track the
    location of the exception using the debugger or the stack backtrace
    facility.  So, do not use [Printexc.catch] in new code.  *)

val to_string: exn -> string
(** [Printexc.to_string e] returns a string representation of the
    exception [e]. *)

val print_backtrace: _ BatInnerIO.output -> unit
(** [print_backtrace oc] Prints the an exception backtrace on the
    output channel [oc].  The backtrace lists the program locations
    where the most-recently raised exception was raised and where it
    was propagated through function calls.

    @since 1.4.0
*)

val get_backtrace: unit -> string
(** [Printexc.get_backtrace ()] returns a string containing the same
    exception backtrace that [Printexc.print_backtrace] would print.
*)

val record_backtrace: bool -> unit
(** [Printexc.record_backtrace b] turns recording of exception
    backtraces on (if [b = true]) or off (if [b = false]).  Initially,
    backtraces are not recorded, unless the [b] flag is given to the
    program through the [OCAMLRUNPARAM] variable.
*)

val backtrace_status: unit -> bool
(** [Printexc.backtrace_status()] returns [true] if exception
    backtraces are currently recorded, [false] if not.
*)

val register_printer: (exn -> string option) -> unit
(** [Printexc.register_printer fn] registers [fn] as an exception
    printer.  The printer should return [None] or raise an exception
    if it does not know how to convert the passed exception, and [Some
    s] with [s] the resulting string if it can convert the passed
    exception. Exceptions raised by the printer are ignored.

    When converting an exception into a string, the printers will be invoked
    in the reverse order of their registrations, until a printer returns
    a [Some s] value (if no such printer exists, the runtime will use a
    generic printer).
*)

val print : _ BatInnerIO.output -> exn -> unit
  (** Print an exception.  The stdlib [print] function is now named [!pass].*)
