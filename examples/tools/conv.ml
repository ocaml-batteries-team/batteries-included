(*
  Convert encodings.
  Everything received from the standard input is converted and written onto the standard output,
  using the encodings specified on the command-line.

  Usage:
  ./conv ASCII UTF-8 < README
*)
open CharEncodings, Sys, IO

try
(*V1: Convert output:
  copy stdin (encoded_as (transcode_out (as_encoded stdout (`named argv.(1))) (`named argv.(2))))*)
(*V2: Convert input*)
  copy  (encoded_as **> transcode_in (as_encoded stdin **> `named argv.(1)) (`named argv.(2))) stdout;
  flush_all ()
with Not_found      -> Print.eprintf p"Sorry, unknown encoding.\n%!"
  |  Malformed_code -> Print.eprintf p"Error: This text is not encoded with encoding %S\n" (argv.(1))
  |  e              -> Print.eprintf p"Error:\n%s\n%!" (Printexc.to_string e)
