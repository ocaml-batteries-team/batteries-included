(*
 * generate_mli.ml
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 *                 2009, David Rajchenbach-Teller (contributor)
 * Licence   : BSD3
 *)

(* This file generate a .mli from a simplified .ml file. It should be
   invoked like that:

   $ generate_mli input.ml > output.mli

   Allowed constructions in the .ml are:

   - module aliases: "module Foo = Bar"
     ("Bar" must be an external module)
   - structures: "module Foo = struct ... end"
   - comments
*)


open Preprocess_common

(* +------+
   | Main |
   +------+ *)

let _ =
  let destination = ref ""
  and source      = ref ""
  and path        = ref "" in
  Arg.parse
    [("-o", Arg.Set_string destination, "Output to a given file (standard output by default)");
     ("-i", Arg.Set_string source,      "Input from a given file (standard input by default)");
     ("-path", Arg.Set_string path,     "Name of the submodule to extract (default is extract everything)")]
       ignore
       (Printf.sprintf "%s [options]: extract part of the contents of a .mli file\n%!" (Filename.basename Sys.argv.(0)));
  try
    let output = match !destination with
      | "" -> stdout
      | s  -> open_out s
    and input  = match !source      with
      | "" -> stdin
      | s  -> open_in s
    and path   = match !path        with
      | "" -> []
      | s  -> path_of_string (Sys.argv.(2))
    in
      extract !source input output path;
      flush_all ()
  with
    | Exit as e ->
        raise e
    | exn ->
        Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exn

