(* Open a .gz file and decompress it on the spot.

   Usage:
   ./gunzip.byte some_file.gz
   (produces some_file, removes some_file.gz)
*)
open File, IO, Filename

iter f (args ())
where let f name =
  if check_suffix name ".gz" then
    with_file_in  name                     (fun inp ->
    with_file_out (chop_suffix name ".gz") (fun out ->
    Gzip.with_in  inp                      (fun inp'->
      copy inp' out;
      Sys.remove name)))
  else prerr_endline ("I don't know what to do with file "^name)
