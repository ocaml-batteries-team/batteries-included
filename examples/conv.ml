(*
  Convert encodings.
  Everything received from the standard input is converted and written onto the standard output,
  using the encoding specified on the command-line.
*)
open CharEncodings, Sys



let source = `ascii;;
let dest   = `ascii;;

Printf.printf "Reading with encoding %S (%S)\n%!" argv.(1) (name_of_encoding source);;
Printf.printf "Writing with encoding %S (%S)\n%!" argv.(2) (name_of_encoding dest);;
(*IO.copy (stuff_of (transcode_in (of_stuff stdin source) dest)) stdout;;*)
(*IO.copy stdin (stuff_of (transcode_out (of_stuff stdout source) dest));;*)
(*DEBUG:*)IO.copy stdin stdout;;
Printf.printf "Done!\n%!";;
flush_all ()
