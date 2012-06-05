(** Implementation of a cat-like tool: read each file whose name is
    given on the command-line and print the contents to stdout.

    Compilation:
     ocamlbuild cat.byte

    Usage:
     ./cat.byte *.ml

*)

open Batteries_uni;;

iter (fun x -> IO.copy (File.open_in x) stdout) (args ());;
