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

type kinds = 
  | Values
  | Types
  | Topics
  | Modules
  | Exns
  | Modtypes
  | Classes
  | Methods
  | Attributes
  | Objtypes

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
*)

