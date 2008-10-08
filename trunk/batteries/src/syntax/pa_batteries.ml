(*
 * Pa_openin -- Syntax extension for local module opening
 * Copyright (C)   2006 Alain Frisch
 *                 2007 Till Varoquaux
 *                 2008 Gabriel Scherer
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
open PreCast

AstFilters.register_str_item_filter (fun st -> <:str_item< open Foo; $st$ >>);;
(*module Id = struct
  let name = "pa_batteries"
  let version = "0.1"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax
  open Sig
  open Ast



  let f = object (self) 
    inherit Ast.map as super
    method str_item = function
	
      | <:expr< $x$ + 0 >> |
	  <:expr< 0 + $x$ >> -> self#expr x
      | e -> super#expr e
  end

module M = Register.OCamlSyntaxExtension(Id)(Make)
*)
