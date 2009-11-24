(*
 * sample.ml
 * ---------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open Printf

(* [x] is a list of characters defined in a convenient way: *)
let x = l"Hello, world!"

(* Simple function on list of characters: *)
let replace patt repl l =
  List.map (fun ch -> if ch = patt then repl else ch) l

let y = replace 'o' 'i' x

let output_char_list oc l = List.iter (output_char oc) l

let _ =
  printf "x = %a\ny = %a\n" output_char_list x output_char_list y
