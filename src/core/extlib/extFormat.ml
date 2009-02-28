(* 
 * ExtFormat - Extended Format module
 * Copyright (C) 1996 Pierre Weis
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
open IO
module Format = struct
  include Format

  let output_of out = fun s i o -> ignore (really_output out s i o)
  let flush_of  out = InnerIO.get_flush out

  (**{6 New functions}*)


  let formatter_of_output out =
    make_formatter (output_of out) (flush_of out)
      
  let set_formatter_output out =
    set_formatter_output_functions (output_of out) (flush_of out)

  let pp_set_formatter_output f out =
    pp_set_formatter_output_functions f (output_of out) (flush_of out)

  (**{6 Old values, new semantics}*)

  let formatter_of_out_channel     = formatter_of_output
  let set_formatter_out_channel    = set_formatter_output
  let pp_set_formatter_out_channel = pp_set_formatter_output
  let std_formatter                = formatter_of_output IO.stdout

  (**{6 Initialization}*)

  let _ = 
    set_formatter_output IO.stdout;
    Format.pp_set_formatter_output_functions Format.std_formatter (output_of stdout) (flush_of stdout);
    Format.pp_set_formatter_output_functions Format.err_formatter (output_of stderr) (flush_of stderr)
    

    
end
