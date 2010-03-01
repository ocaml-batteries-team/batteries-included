(*
 * pa_string_list.ml
 * -----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(* Sample syntax extension for replacing strings by list of
   characters *)

open Camlp4.PreCast
open Pa_estring

let _ =
  register_expr_specifier "l"
    (fun ctx loc str -> llist_expr (fun _loc ch -> <:expr< $chr:Char.escaped ch$ >>) (unescape loc str));
  register_patt_specifier "l"
    (fun ctx loc str -> llist_patt (fun _loc ch -> <:patt< $chr:Char.escaped ch$ >>) (unescape loc str))
