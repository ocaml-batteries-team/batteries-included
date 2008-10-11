(* Randomly reorder the elements given on the command-line. 

   Usage:
   ./shuffle 1 2 3 4 5 6 7 8 9
*)

open Util.Random with self_init ()
open Data.Mutable, System, IO

let _ = Array.print stdout ~sep:" " ~last:"\n" nwrite (shuffle (args ()))
