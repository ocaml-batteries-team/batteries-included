(* 
 * ExtChar - Additional operations on arguments
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

TYPE_CONV_PATH "" (*For Sexplib, Bin-prot...*)

module Arg = struct
  include Arg

  type command =
      { doc : string(**The documentation associated to the keyword, possibly empty.*);
	kwd : string(**The keyword. Should start with "-"*);
	spec : spec (**The behavior associated to the keyword*);
      }

  let command ?(doc="") kwd spec =
    { doc = doc;
      kwd = kwd;
      spec= spec }

  let of_command c = (c.kwd, c.spec, c.doc)

  let handle ?argv ?(align=false) ?(usage="") cmd =
    let speclist = List.map of_command cmd
    and anonymous= RefList.empty () in
    let spec_to_parse = if align then Arg.align speclist else speclist in
    let argv_to_parse = match argv with None -> Sys.argv | Some s -> s in
    let anon_fun = RefList.push anonymous in
      try
        parse_argv argv_to_parse spec_to_parse anon_fun usage;
        List.rev (RefList.to_list anonymous)
      with
      | Help s | Bad s -> prerr_string s; []

end
