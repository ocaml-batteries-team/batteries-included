(*
 * pa_strings.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

open Camlp4.PreCast
open Pa_estring

#if ocaml_version < (3, 11)
  (*For the moment, we don't have a version compatible with OCaml < 3.11*)
#else

let _ = Printf.eprintf "Launching pa_estring\n%!"

let _ =

  (* UTF-8 strings *)

  register_expr_specifier "u"
    (fun ctx _loc str ->
       let id = register_shared_expr ctx <:expr< Batteries.UTF8.of_string $str:str$ >> in
       <:expr< $id:id$ >>);
  register_when_specifier "u"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< Batteries.UTF8.of_string $str:str$ >> in
       <:expr< Batteries.UTF8.compare $id:shared_id$ $id:id$ = 0 >>);

  (* Strings with capabilities *)

  register_expr_specifier "rw"
    (fun ctx _loc str ->
       <:expr< Batteries.String.Cap.of_string $str:str$ >>);
  register_when_specifier "rw"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< Batteries.String.Cap.of_string $str:str$ >> in
       <:expr< Batteries.String.Cap.compare $id:shared_id$ $id:id$ = 0 >>);

  register_expr_specifier "ro"
    (fun ctx _loc str ->
       let id = register_shared_expr ctx <:expr< Batteries.String.Cap.read_only (Batteries.String.Cap.of_string $str:str$) >> in
       <:expr< $id:id$ >>);
  register_when_specifier "ro"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< Batteries.String.Cap.of_string $str:str$ >> in
       <:expr< Batteries.String.Cap.compare $id:shared_id$ $id:id$ = 0 >>);

  register_expr_specifier "wo"
    (fun ctx _loc str ->
       <:expr< Batteries.String.Cap.write_only (Batteries.String.Cap.of_string $str:str$) >>);
  register_when_specifier "wo"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< Batteries.String.Cap.of_string $str:str$ >> in
       <:expr< Batteries.String.Cap.compare $id:shared_id$ $id:id$ = 0 >>);

  (* Ropes *)

  register_expr_specifier "r"
    (fun ctx _loc str ->
       let id = register_shared_expr ctx <:expr< Batteries.Rope.of_latin1 $str:str$ >> in
       <:expr< $id:id$ >>);
  register_when_specifier "r"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< Batteries.Rope.of_latin1 $str:str$ >> in
       <:expr< Batteries.UTF8.compare $id:shared_id$ $id:id$ = 0 >>);

  register_expr_specifier "ur"
    (fun ctx _loc str ->
       let id = register_shared_expr ctx <:expr< Batteries.Rope.of_ustring $str:str$ >> in
       <:expr< $id:id$ >>);
  register_when_specifier "ur"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< Batteries.Rope.of_ustring $str:str$ >> in
       <:expr< Batteries.UTF8.compare $id:shared_id$ $id:id$ = 0 >>)

#endif
