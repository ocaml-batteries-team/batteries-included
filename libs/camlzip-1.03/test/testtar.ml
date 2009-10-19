open Tar
let pmagic = function
  | V7_FORMAT -> "V7_FORMAT"
  | OLDGNU_FORMAT -> "OLDGNU_FORMAT"
  | GNU_FORMAT -> "GNU_FORMAT"
  | POSIX_FORMAT -> "POSIX_FORMAT";;

let tar = Tar.open_in_chan stdin in
  (* and otar = Tar.open_out ~compress:`Bzip2 "wrote.tar.bz2" *)
  try
    while true do
      let (header, body) = read_entry tar in
	Printf.printf "Type: %s " (pmagic header.t_format);
	Printf.printf "Filename: %s Bytes: %d\n" header.t_name (String.length body);
	(*      Tar.output otar header body *)
    done
  with
      End_of_file ->
	Tar.close_in tar;
	(*      Tar.close_out otar *)
