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

    @author David Teller
*)

val tutorial : unit -> unit
(** [tutorial ()] opens the tutorial.*)


(*
val help           : string -> unit
val help_language  : string -> unit
val help_value     : string -> unit
val help_type      : string -> unit
val help_module    : string -> unit
val help_exception : string -> unit
*)

(** A mechanism for extending the help system.*)
module Extend :
sig

(**The category of help being installed*)
type kind =
  | Language (**Help on the language itself*)
  | Values   (**Documentation on values*)
  | Types    (**Documentation on types*)
  | Modules  (**Documentation on modules*)
  | Exceptions
  | Module_types
  | Classes
  | Methods
  | Attributes
  | Class_types


val register : name:string -> kind:kind -> index:string -> directory:string -> unit
(**Register a new help package

   @param name a human-readable name for the help package
   @param kind the kind of help added by this package
   @param index the complete path to the index file for this package -- for the moment, this must be a local file
   @param directory the directory (either local or distant) where the on-line help may be found
*)

end
