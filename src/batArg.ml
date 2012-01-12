(*
 * BArg - Additional operations on arguments
 * Copyright (C) 1996 Damien Doligez
 *               2009 David Teller, LIFO, Universite d'Orleans
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

(* GLUE with StdLib *)
type spec = Arg.spec =
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Set of bool ref
  | Clear of bool ref
  | String of (string -> unit)
  | Set_string of string ref
  | Int of (int -> unit)
  | Set_int of int ref
  | Float of (float -> unit)
  | Set_float of float ref
  | Tuple of spec list
  | Symbol of string list * (string -> unit)
  | Rest of (string -> unit)
type key = string
type doc = string
type usage_msg = string
type anon_fun = string -> unit
let parse = Arg.parse
let parse_argv = Arg.parse_argv
exception Help = Arg.Help
exception Bad = Arg.Bad
let usage = Arg.usage
let usage_string = Arg.usage_string
let align = Arg.align
let current = Arg.current
(* END GLUE *)

type command =
    { doc : string(**The documentation associated to the keyword, possibly empty.*);
      kwd : string(**The keyword. Should start with "-"*);
      spec : Arg.spec (**The behavior associated to the keyword*);
    }

let command ?(doc="") kwd spec =
  { doc = doc;
    kwd = kwd;
    spec= spec }

let of_command c = (c.kwd, c.spec, c.doc)

let handle ?(usage="") cmd =
  let speclist = List.map of_command cmd
  and anonymous= BatRefList.empty ()       in
  Arg.parse speclist (fun s -> BatRefList.push anonymous s) usage;
  List.rev (BatRefList.to_list anonymous)
