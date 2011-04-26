(* 
 * ExtPrintexc - Extended Printexc module
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

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Printexc.html}Printexc}
    module, go there for documentation on the rest of the functions
    and types.

    @author Xavier Leroy (Base module)
    @author David Teller
*)

val pass : ('a -> 'b) -> 'a -> 'b
(** [Printexc.pass fn x] applies [fn] to [x] and returns the result.
    If the evaluation of [fn x] raises any exception, the
    name of the exception is printed on standard error output,
    and the exception is raised again.
    The typical use is to catch and report exceptions that
    escape a function application. *)

val print : _ BatInnerIO.output -> exn -> unit
(** Print an exception.*)

val print_backtrace: _ BatInnerIO.output -> unit
(** [print_backtrace oc] Prints the an exception backtrace on the output channel [oc]. 
    @since 1.4.0
*)
