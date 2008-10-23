(* 
 * Top - An interpreted preambule for the toplevel
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
   This file is meant to be invoked by a toplevel and performs initialization
   of OCaml Batteries Included and its libraries.

   Initialization consists in
   - loading Findlib
   - loading dependencies
   - initializing and starting Camlp4
   - loading syntax extensions
   - loading the contents of the on-line help system
   - printing a welcome message
   - actually starting the toplevel

   To take advantage of this file, run
   [ocaml top.ml]
*)

#use "topfind";;
#require "num";;  (*For some reason, if [num] is not loaded before Camlp4, an exception is launched*)
#predicates "preprocessor";;
#camlp4o;;
#require "camlp4";;
#require "batteries";;
#require "batteries.syntax";;
Batteries_help.init ();;
print_endline "Welcome to OCaml Batteries Included.";;
print_endline "To obtain help on any subject, you may use";;
print_endline "    #help \"some_subject\"";;
print_endline "If you don't know where to start, try";;
print_endline "    #help \"starting\"";;
print_newline ();;
print_newline ();;
flush_all ();;
Toploop.loop Format.std_formatter ;; (*At this point, we start the toplevel manually.*)
