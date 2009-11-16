(* 
 * Cbtop - Customizing the toplevel to allow loading of a preambule.
 * Copyright (C) 2009 Peng Zang 
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

(** The name of the file containing the pre-initialization.*)
let batteries_init                    = Sys.argv.(1)
let original_parse_use_file           = !Toploop.parse_use_file
let original_parse_toplevel_phrase    = !Toploop.parse_toplevel_phrase

(** Restore the original parsers.
    This function must be called before evaluating the initialization file,
    as that evaluation can also change the parsers (e.g. if it loads Camlp4)
    and we do not want to overwrite such side-effect.*)
let restore_original_parsers () =
  Toploop.parse_use_file        := original_parse_use_file;
  Toploop.parse_toplevel_phrase := original_parse_toplevel_phrase

let rec parse_use_file lb =
  match try Some(!Toploop.parse_toplevel_phrase lb) with End_of_file -> None with
    | Some phrase ->
        phrase :: parse_use_file lb
    | None ->
        []

let replacement_parse_use_file lb =
  restore_original_parsers ();
  ignore (Toploop.use_silently Format.std_formatter batteries_init);
  Toploop.parse_use_file := parse_use_file;
  parse_use_file lb;;

let replacement_parse_toplevel_phrase lb =
  restore_original_parsers ();
  ignore (Toploop.use_silently Format.std_formatter batteries_init);
  Toploop.parse_use_file := parse_use_file;
  !Toploop.parse_toplevel_phrase lb;;

Arg.current                    := 1;;
Toploop.parse_use_file         := replacement_parse_use_file;;
Toploop.parse_toplevel_phrase  := replacement_parse_toplevel_phrase;;
