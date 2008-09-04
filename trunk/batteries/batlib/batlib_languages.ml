(*
 * Batlib.Languages - Parsing and printing
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
   Parsing, printing, regular expressions and other transformations from text
   to data, from data to text and from text to text.
*)

(** {1 Parsing} *)

module Genlex          = Batlib_Baselib_Genlex
module Lexing          = Batlib_Baselib_Lexing
module Parsing         = Batlib_Baselib_Parsing
module Scanf           = Batlib_Baselib_Scanf
module Str             = Batlib_Baselib_Str

(** {1 Printing}*)

module Format          = Batlib_Baselib_Format
module Printexc        = Batlib_Baselib_Printexc
module Printf          = Batlib_Baselib_Printf

(**/**)

(**
   {1 Note to developers}

   This module is meant to contain specifically tools which may be used for parsing
   and for printing. Regular expressions are presented in this module insofar as
   they may be used for both purposes.

   Here is a list of other tasks which may be added here
   - serialization-to-human-readable-formats 
   - xml, dom, etc.

   Here is a list of tasks which should probably not be added here
   - unicode utilities which have no special relation to parsing or printing (put them in {!Batlib.Data.Text})
   - bindings to other programming languages (no real place to put them yet, for the moment, {!Batlib.Meta})
*)
