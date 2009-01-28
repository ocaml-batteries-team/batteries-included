(*
 * pa_estring_top.ml
 * -----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(* Setup pa_estring for the toplevel *)

open Camlp4.PreCast

#if ocaml_version < (3, 11)
  (*For the moment, we don't have a version compatible with OCaml < 3.11*)
#else

let _ = Printf.eprintf "Launching pa_estring_top\n%!"

(* Hack used to force camlp4 to define a new token stream filtered
   with the newly defined filter. *)
let _ =
  let parse = !Toploop.parse_toplevel_phrase and lexbufs = ref [] in
  Toploop.parse_toplevel_phrase :=
    (fun lexbuf ->
       match try Some(List.assq lexbuf !lexbufs) with _ -> None with
         | Some lexbuf ->
             parse lexbuf
         | None ->
             let new_lexbuf = { lexbuf with Lexing.refill_buff = lexbuf.Lexing.refill_buff } in
             lexbufs := (lexbuf, new_lexbuf) :: !lexbufs;
             parse new_lexbuf)

#endif
