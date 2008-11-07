(* Compress a file to .gz on the spot

   Usage:
   ./gzip.byte some_file
   (produces some_file.gz, removes some_file)
*)
open System, File, IO, Filename

(*iter f (args ())
where let f name =
  with_file_in  name           (fun inp -> 
  with_file_out (name ^ ".gz") (fun out ->
  GZip.with_out out            (fun out' ->
    copy inp out'(*;       *)
    (*Sys.remove name*)
)));;*)

open System, File, IO, Filename

iter f (args ())
where let f name =
  let inp = open_in   name in
  let out = open_out (name ^ ".gz") in
  let out'= GZip.compress out       in
    begin
      copy inp out';
      close_out out'
    end
