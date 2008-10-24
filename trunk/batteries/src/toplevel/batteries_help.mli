(* 
 * Batteries_help - Calling the help system from the toplevel
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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

(** Tools for reading the documentation from the toplevel

    All these tools are invoked automatically by the Batteries
    Toplevel.  They are provided here if you wish to integrate them
    into your own toplevel.

    @author David Teller
*)

val init : unit -> unit
(** Proceed to initialization.

    This function loads the primary help files and registers the
    toplevel directives.

    If you integrate the on-line help system into your toplevel, you
    must call this function before any of the other functions of this
    module. *)

val help : unit -> unit
(** [help ()] opens the tutorial.*)

val man           : string -> unit
(** [man "something"] opens the help about subject ["something"].

    [man] is the most generic kind of man.
*)

val man_value     : string -> unit
(** [man_value "something"] opens the help about a value named ["something"].

    Use this function to find informations on variables, constants, functions,
    constructors ...
*)

val man_type: string -> unit
val man_language: string -> unit
val man_module: string -> unit
val man_exception: string -> unit
val man_exn: string -> unit
(**As {!man_exception}*)

val man_signature: string -> unit
val man_modtype: string -> unit
(**As {!man_signature}*)

val man_class: string -> unit
val man_method: string -> unit
val man_attribute: string -> unit
val man_field: string -> unit
(**As {!man_attribute}*)

val man_objtype: string -> unit


(** A mechanism for extending the help system.*)
module Extend :
sig 

(**The category of help being installed*)
type kind =
  | Language    (**Documentation on the language itself*)
  | Values      (**Documentation on values*)
  | Types       (**Documentation on types*)
  | Modules     (**Documentation on modules*)
  | Exceptions  (**Documentation on exception constructors*)
  | Module_types(**Documentation on module types*)
  | Classes     (**Documentation on classes*)
  | Methods     (**Documentation on methods*)
  | Attributes  (**Documentation on attributes*)
  | Class_types (**Documentation on class types*)

val register : name:string -> kind:kind -> index:string -> prefix:string -> unit
(**Register a new help package

   @param name a human-readable name for the help package
   @param kind the kind of help added by this package
   @param index the complete path to the index file for this package -- for the moment, this must be a local file
   @param prefix the start of the URL at which to find the html files containing help (don't forget ["file:///"]
    if the files are local).
*)

end
