(*
 * Batteries_config - Configuration module for OCaml Batteries Included
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

(**
   This system's configuration.
*)

val version            : string
(**
   The version of OCaml Batteries Included, as a human-readable freeform string.

   e.g. ["0.20081006b"]
*)

val documentation_root : string
(**
   A path to the root of the documentation. This directory is expected to contain
   subdirectories [html] and [html/api], as well as a documentation index file
   [documentations.idex].

   e.g. ["/usr/share/doc/batteries"]
*)

val browse : string -> int
(**
   [browse s] opens the platform's default www browser to browse url [s].

   Return the error status.
*)

val set_browser : (string -> int) -> unit
(**
   [set_browser f] replaces the default browser with a function [f]. This
   may be used to add debugging information, use a custom browser, etc.
*)

val max_array_length : int
(**
   The maximal length of a normal array on this system.

   {b Note} Arrays of floats use a different in-memory representation,
   optimised for performance, and do not necessarily have the same
   length as other arrays. The maximum length of a float array is
   [max_array_length/2] on 32-bit machines and [max_array_length] on
   64-bit machines.
*)

val word_size : int
  (** Size of one word on the machine currently executing the Caml
      program, in bits: 32 or 64. *)

val max_string_length : int
  (** Maximum length of a string. *)
