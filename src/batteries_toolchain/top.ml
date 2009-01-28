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

   To take advantage of this file, run
   [ocaml -init top.ml] (or, equivalently, use our customized version of [ocaml])
*)
let interactive = !Sys.interactive;;
Sys.interactive := false;; (*Pretend to be in non-interactive mode to avoid toplib messages*)
#use "topfind";;
#require "num";;       (*For some reason, if [num] is not loaded before Camlp4, an exception is launched*)
#require "netstring";; (*For some reason, if [netstring] is not loaded before Camlp4, an exception is launched*)
                       (*Note: a common point between num and netstring is that they both define custom printers
			 for the toplevel. This may be the reason for this bug.*)
#predicates "preprocessor";;
#require "dynlink";;
#camlp4o;;
#require "camlp4";;
#require "batteries";;
#require "batteries.syntax";;
#load "pa_estring_top.cmo";; (*For some reason, pa_estring_top.cmo won't load by itself.*)


if interactive then (*Only initialize help and display welcome if we're in interactive mode.*)
begin
  Batteries_help.init ();
  print_newline ();
  print_endline "----------------------------------------------";
  print_endline "|                                            |";
  print_endline "|     This is OCaml, Batteries Included.     |";
  print_endline "|                                            |";
  print_endline "|                                            |";
  print_endline "|      If you need help, type '#help;;'      |";
  print_endline "|                                            |";
  print_endline "----------------------------------------------";
  print_newline ();
  print_newline ();
  flush_all ()
end

open Batteries;;
open Standard;;
#install_printer Batteries_print.print_uchar;;
#install_printer Batteries_print.print_ustring;;
#install_printer Batteries_print.print_rope;;
#install_printer Batteries_print.print_string_cap_rw;;
#install_printer Batteries_print.print_string_cap_ro;;
Sys.interactive := interactive;; (*Return to regular interactive mode*)
