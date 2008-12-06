(*Randomly reorder the elements given on stdin.

  Usage:
  ./shuffle2.byte < some_file.txt
*)

open Random with self_init ()
open IO, Printf

let shift x = x + 1;;

Array.iteri (shift |- printf "%-2d: %s\n") (shuffle (lines_of stdin))

