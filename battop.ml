(* 
 * Top - An interpreted preambule for the toplevel
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

(**
   This file is meant to be invoked by a toplevel and performs initialization
   of OCaml Batteries Included and its libraries.

   Initialization consists of
   - loading Findlib
   - loading dependencies
   - loading the contents of the on-line help system
   - printing a welcome message

   This file is loaded by the magic line in the ocamlinit file.
*)

(* Set the below to false to disable use of syntax extensions in toplevel *)
let ext_syntax = true;;


(* END CONFIGURATION *)

(* MUST BE ALREADY HANDLED BY .ocamlinit
#use "topfind";; 
*)
#thread;;
#require "batteries";;


if !Sys.interactive then (*Only initialize help and display welcome if we're in interactive mode.*)
begin
  Batteries_help.init ();
  print_endline "      _________________________";
  print_endline "    [| +   | |   Batteries   - |";
  print_endline "     |_____|_|_________________|";
  print_endline "      _________________________";
  print_endline "     | -  Type '#help;;' | | + |]";
  print_endline "     |___________________|_|___|";
  print_newline ();
  print_newline ();
  flush_all ()
end;;

open Batteries;;
#install_printer Batteries_print.print_uchar;;
#install_printer Batteries_print.print_ustring;;
#install_printer Batteries_print.print_rope;;
#install_printer Batteries_print.print_string_cap_rw;;
#install_printer Batteries_print.print_string_cap_ro;;
#install_printer Batteries_print.string_dynarray;;
#install_printer Batteries_print.int_dynarray;;
#install_printer Batteries_print.char_dynarray;;
#install_printer Batteries_print.float_dynarray;;
#install_printer Batteries_print.int_set;;
#install_printer Batteries_print.string_set;;
#install_printer Batteries_print.istring_set;;
#install_printer Batteries_print.rope_set;;
#install_printer Batteries_print.char_set;;
#install_printer Batteries_print.int_pset;;
#install_printer Batteries_print.string_pset;;
#install_printer Batteries_print.rope_pset;;
#install_printer Batteries_print.char_pset;;
#install_printer Batteries_print.int_enum;;
#install_printer Batteries_print.string_enum;;
#install_printer Batteries_print.rope_enum;;
#install_printer Batteries_print.char_enum;;


if ext_syntax then begin
  if !Sys.interactive then 
    print_endline "Loading syntax extensions...";
  Topfind.standard_syntax();
  Topfind.load_deeply ["dynlink"; "camlp4"; "batteries.pa_string.syntax";
   		       "batteries.pa_comprehension.syntax"];
end else 
  if !Sys.interactive then 
    print_endline "Batteries Syntax extensions disabled.";
;;
