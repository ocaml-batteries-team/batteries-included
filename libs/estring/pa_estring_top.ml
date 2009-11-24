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

(* Reload "Camlp4Top.cmo" to force camlp4 to use the new topphrase filter *)
let _ = Topdirs.dir_load Format.std_formatter "Camlp4Top.cmo"
