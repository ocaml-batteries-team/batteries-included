(*
 * Pa_openin_r -- Syntax extension for local module opening, revised syntax version
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

module Id = struct
  let name = "pa_open"
  let version = "1.1"
end

let fresh () = Printf.sprintf "OPENIN_%i" (Oo.id (object end))


module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax
  open Sig
  open Ast


let local_struct _loc st e init =
  let add_initialization _loc e = function
    | None    -> e
    | Some e' -> <:expr< $e'$;$e$ >>
  in
  let x = fresh ()                       in
  let e'= add_initialization _loc e init in
    <:expr< let module $x$ = struct $st$ let res = $e'$ end in $uid:x$.res >>
								 
let global_struct _loc st init = 
  let x = fresh () in
    match init with
      | Some e -> <:str_item<module $x$ = struct $st$ let _ = $e$ end >>
      | None   -> <:str_item<module $x$ = struct $st$ end >>

EXTEND Gram
GLOBAL: expr str_item;


(** Implement syntax extension [open Foo with e]*)
  module_init: [
    ["with"; e = expr -> e]
  ];
  module_with_init: [
    [a = module_expr; e = OPT module_init -> (a,e)]
  ];
(** Implement the opening of several modules at once*)
  one_or_more_modules: [
    [a = LIST1 module_with_init SEP "," -> a]
  ];
(** Implement local opening of modules*)
  expr: LEVEL "simple" [
    ["open"; modules = one_or_more_modules; "in"; e = expr LEVEL "top" ->
       List.fold_left (
	 fun e mi ->
	   begin match mi with
	     | (<:module_expr< $id:i$ >>,init) -> 
	         local_struct _loc <:str_item< open $i$ >> e init
	     | (me, init) -> let x = fresh () in
		 local_struct _loc <:str_item< module $x$ = $me$ open $uid:x$ >> e init
	   end
       ) e modules
    | "struct"; st = LIST0 [ s = str_item; OPT ";;" -> s ]; "end"; "in";
     e = expr LEVEL "top" -> local_struct _loc (stSem_of_list st) e None
    ]];
   str_item: LEVEL "top" [
     ["open"; modules = one_or_more_modules ->
       List.fold_left (
	 fun acc mi ->
	   match mi with
	     | (<:module_expr< $id:i$ >>, Some e') -> 
		 <:str_item<$acc$ open $id:i$ $exp:e'$>>
	     | (<:module_expr< $id:i$ >>, None) -> 
		 <:str_item<$acc$ open $id:i$>>
	     | (me, init) -> 
		 let x  = fresh () in
		 let st = global_struct _loc <:str_item<module $x$ = $me$ open $uid:x$>> init in
		   <:str_item<$acc$;; $st$>>
       ) <:str_item<>> modules]
   ];
(*  expr: LEVEL ";" [
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
  ];*)
  
END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
