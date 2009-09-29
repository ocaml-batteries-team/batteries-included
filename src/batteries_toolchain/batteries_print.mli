(* 
 * Batteries_print - Pretty-printers for the toplevel
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

(** {6 Printing values}*)

(** {7 Unicode}*)

open BatUChar
open BatUTF8

val print_uchar : Format.formatter -> UChar.t -> unit
(** Pretty-print a [UChar.t]*)

val print_rope : Format.formatter -> Rope.t -> unit
(** Pretty-print a [Rope.t]*)

val print_ustring : Format.formatter -> UTF8.t -> unit
(** Pretty-print a [UTF8.t]*)

(** Pretty-print [String.Cap.t]*)

open BatString

val print_string_cap_rw: Format.formatter -> [> `Read | `Write] String.Cap.t -> unit
(** Pretty-print a read-write [String.Cap.t] (prefixes output with "rw")*)

val print_string_cap_ro: Format.formatter -> [`Read]  String.Cap.t -> unit
(** Pretty-print a read-only [String.Cap.t] (prefixes output with "ro")*)

(** {7 Data structures}*)
(*val print_dllist : Format.formatter -> 'a Dllist.t -> unit*)
