(* Compress a file to .gz on the spot

   Usage:
   ./gzip.byte some_file
   (produces some_file.gz, removes some_file)
*)
open File, IO, Filename

iter f (args ())
where let f name =
 with_file_out (name ^ ".gz") (fun out ->
 with_file_in name            (fun inp ->
   copy inp (Gzip.compress out);
   Sys.remove name
 ))
