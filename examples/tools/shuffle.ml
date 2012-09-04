(* Randomly reorder the elements given on the command-line.

   Usage:
   ./shuffle 1 2 3 4 5 6 7 8 9
*)

open Random with self_init ()

let _ = Array.print ~sep:" " ~first:"" ~last:"\n" output_string stdout (shuffle (args ()));;
