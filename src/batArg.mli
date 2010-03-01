(* 
 * ExtArg - Additional operations on arguments
 * Copyright (C) 1996 Damien Doligez
 *               2009 David Teller, LIFO, Universite d'Orleans
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

(** Parsing of command line arguments.

   This module provides a general mechanism for extracting options and
   arguments from the command line to the program.

   Syntax of command lines:
    A keyword is a character string starting with a [-].
    An option is a keyword alone or followed by an argument.
    The types of keywords are: [Unit], [Bool], [Set], [Clear],
    [String], [Set_string], [Int], [Set_int], [Float], [Set_float],
    [Tuple], [Symbol], and [Rest].
    [Unit], [Set] and [Clear] keywords take no argument. A [Rest]
    keyword takes the remaining of the command line as arguments.
    Every other keyword takes the following word on the command line
    as argument.
    Arguments not preceded by a keyword are called anonymous arguments.

   Examples ([cmd] is assumed to be the command name):
-   [cmd -flag           ](a unit option)
-   [cmd -int 1          ](an int option with argument [1])
-   [cmd -string foobar  ](a string option with argument ["foobar"])
-   [cmd -float 12.34    ](a float option with argument [12.34])
-   [cmd a b c           ](three anonymous arguments: ["a"], ["b"], and ["c"])
-   [cmd a b -- c d      ](two anonymous arguments and a rest option with
                           two arguments)

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html}Arg}
    module, go there for documentation on the rest of the functions
    and types.

    @author Damien Doligez (Base module)
    @author David Teller
*)

type command (**The type describing both the name, documentation and behavior
	  associated with a keyword.*)

val command: ?doc:string -> string -> Arg.spec -> command
(** Construct a new command, i.e. the specification of a keyword,
    an associated behavior and optionally a usage documentation.

    @param doc A string which will be displayed to the user in
    case of parsing error, and which should explain both the
    behavior and the syntax of this keyword. If left unspecified,
    no documentation is printed.
*)

val handle : ?usage:string -> command list -> string list
(**
   [Arg.handle commands] parses the command-line and applies
   the specifications of [commands] and returns the list
   of anonymous arguments.

   In case of error, the program exits and displays the
   usage message, if specified, and the documentation of
   [command].

   @param usage An optional string which will be displayed to
   the user in case of parsing error. Typically, this string
   should contain the name and version of the program. If
   left unspecified, no usage string is displayed in case of
   error.
*)

