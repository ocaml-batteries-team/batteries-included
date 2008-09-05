(* This code is in public domain. Written by Alain Frisch.  2006-05-16.
   Updated by Till Varoquaux. 2007-05-2.
*)

open Camlp4.PreCast
open Syntax
open Ast

let fresh () = Printf.sprintf "OPENIN_%i" (Oo.id (object end))

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
