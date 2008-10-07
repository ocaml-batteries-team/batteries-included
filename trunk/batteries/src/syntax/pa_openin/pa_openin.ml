(*
 * Pa_openin -- Syntax extension for local module opening
 * Copyright (C)   2006 Alain Frisch
 *                 2007 Till Varoquaux
 *                 2008 David Teller
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

module Id = struct
  let name = "pa_open"
  let version = "1.0"
end

let fresh () = Printf.sprintf "OPENIN_%i" (Oo.id (object end))


module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax
  open Sig
  open Ast

let local_struct _loc st e =
  let x = fresh () in
  <:expr< let module $x$ = struct $st$ let res = $e$ end in $uid:x$.res >>


EXTEND Gram
GLOBAL: expr;

  expr: LEVEL ";" [
    ["open"; me = module_expr; "in"; e = expr LEVEL "top" ->
       begin match me with
	 | <:module_expr< $id:i$ >> -> 
	   local_struct _loc <:str_item< open $i$ >> e
	 | _ -> 
	     let x = fresh () in
	       local_struct _loc <:str_item< module $x$ = $me$ open $uid:x$ >> e
       end
    | "struct"; st = LIST0 [ s = str_item; OPT ";;" -> s ]; "end"; "in";
     e = expr LEVEL "top" ->
       local_struct _loc (stSem_of_list st) e
    ]
  ];
  
END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
