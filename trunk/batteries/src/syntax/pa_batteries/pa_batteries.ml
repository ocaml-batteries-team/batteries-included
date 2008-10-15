(*
 * Pa_batteries -- Syntax extension for auto-opening Batteries
 * Copyright (C)   2008 David Teller
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

open Camlp4
open PreCast

module Id = struct
  let name = "pa_batteries"
  let version = "0.1"
end


module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax
  open Sig
  open Ast

    DELETE_RULE Gram implem: "#"; a_LIDENT; opt_expr; semi END
    DELETE_RULE Gram implem: str_item; semi; SELF END
    DELETE_RULE Gram implem: `EOI END

    DELETE_RULE Gram interf: "#"; a_LIDENT; opt_expr; semi END
    DELETE_RULE Gram interf: sig_item; semi; SELF END
    DELETE_RULE Gram interf: `EOI END


  let stopped_at _loc =
    Some (Loc.move_line 1 _loc)

  EXTEND Gram
    GLOBAL:implem interf;
    implem_next:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ( [ <:str_item< # $n$ $dp$ >> ] , stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> (si :: sil, stopped)
        | `EOI -> ([], None)
      ] ];
    interf_next:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:sig_item< # $n$ $dp$ >> ], stopped_at _loc)
        | si = sig_item; semi; (sil, stopped) = SELF -> (si :: sil, stopped)
        | `EOI -> ([], None) ] ]
    ;

    implem:
    [ [
	(l,o) = implem_next -> (<:str_item<open Batteries>>::l,o)
      ] ];

    interf:
    [ [
	(l,o) = interf_next -> (<:sig_item<open Batteries>>::l,o)
      ] ];
  END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
