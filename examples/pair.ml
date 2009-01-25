(*
  Print the contents of two files, optionally using a printf-style format argument.
  I found this useful to write module CharEncodings, and it was a three-liner at
  the time (before I made it parametric).

  Usage:
  ./pair file_1 file_2
  ./pair file_1 file_2 "%s -> %S\n"

  The first usage prints the first line of file_1 followed by the first line of file_2,
  then the second line of file_1 followed by the second line of file_2, etc. until either
  file_1 or file_2 ends.

  The second usage does the same thing but adds characters " -> " between each line of
  file_1 and the corresponding line of file file_2 and puts the contents of each line
  of file_2 between quotes.
*)

open Sys

(*Read the format -- this is the most complicated part of the program*)
let default_format : (_, _, _, _) format4 = "%s %s\n"
if Array.length argv < 2 then failwith "Missing arguments"
let format = if Array.length argv = 3 then  default_format
             else                     Scanf.format_from_string argv.(3) default_format
in
(*Actually do the deed*)
  Enum.iter2
    (fun x y -> Printf.printf format x y)
    (File.lines_of argv.(1))
    (File.lines_of argv.(2))
