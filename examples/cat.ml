(** Implementation of a cat-like tool: read each file whose name is
    given on the command-line and print the contents to stdout.

    Usage: 
     ./cat.byte *.ml
     
*)

open System, IO

let _ =
  iter (fun x -> copy (File.open_in x) stdout) (args ())

