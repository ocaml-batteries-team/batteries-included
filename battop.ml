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


(* END CONFIGURATION *)

(* MUST BE ALREADY HANDLED BY .ocamlinit
#use "topfind";;
*)
#thread;;
#require "batteries";;


if !Sys.interactive then (*Only initialize help and display welcome if we're in interactive mode.*)
begin
  BatteriesHelp.init ();
  let ver = BatteriesConfig.version in
  let vlen = String.length ver in
  let pad = String.make vlen '_' in
  let pad2 = String.make vlen ' ' in
  print_endline ("      ___________________"^ pad  ^"_______");
  print_endline ("    [| +   | |   Batteries " ^ ver ^ "  - |");
  print_endline ("     |_____|_|___________"^ pad  ^"______|");
  print_endline ("      ___________________"^ pad  ^"_______");
  print_endline ("     | -  Type '#help;;' "^ pad2 ^"| | + |]");
  print_endline ("     |___________________"^ pad  ^"|_|___|");
  print_newline ();
  print_newline ();
  flush_all ()
end;;

open Batteries;;
#install_printer BatteriesPrint.print_uchar;;
#install_printer BatteriesPrint.print_ustring;;
#install_printer BatteriesPrint.print_rope;;
#install_printer BatteriesPrint.print_string_cap_rw;;
#install_printer BatteriesPrint.print_string_cap_ro;;
#install_printer BatteriesPrint.string_dynarray;;
#install_printer BatteriesPrint.int_dynarray;;
#install_printer BatteriesPrint.char_dynarray;;
#install_printer BatteriesPrint.float_dynarray;;
#install_printer BatteriesPrint.int_set;;
#install_printer BatteriesPrint.string_set;;
#install_printer BatteriesPrint.int_pset;;
#install_printer BatteriesPrint.string_pset;;
#install_printer BatteriesPrint.rope_pset;;
#install_printer BatteriesPrint.char_pset;;
#install_printer BatteriesPrint.int_enum;;
#install_printer BatteriesPrint.string_enum;;
#install_printer BatteriesPrint.rope_enum;;
#install_printer BatteriesPrint.char_enum;;
