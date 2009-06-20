open OUnit
open Print
open IO

let string = "hello world"

let test_open_process_readline () =
  try
    let r,w = Unix.open_process "cat" in
      fprintf w p"%s\n" string;
      close_out w;
      match IO.read_line r with
	| s when s = string -> ()
	| s                 -> assert_failure (sprintf p"Expected %S, got %S" string s)
  with e -> assert_failure (sprintf p"Expected %S, got exception %exn" string e)

(*let test_open_process_legacy_readchar () =
  try
    let r,w = Unix.open_process ~cleanup:true "cat" in
      fprintf w p"%s\n" string;
      close_out w;
(*      let fdw = Unix.descr_of_output w              in
	Unix.close fdw;*)(*For some reason, when reading char-by-char
			   instead of line-by-line, [w] must be closed
			   with low-level closing*)
      let fdr = Unix.descr_of_input r               in
      let ic  = Legacy.Unix.in_channel_of_descr fdr in
	while true do
	  ignore (Legacy.Pervasives.input_char ic)
	done
  with End_of_file 
    | No_more_input -> () 
    | e -> assert_failure (sprintf p"Expected %S, got exception %exn" string e)*)

let test_open_process_cleanup () =
  try
    let r,w = Unix.open_process ~cleanup:true "cat" in
      fprintf w p"%s\n" string;                       
      close_out w;
      while true do
	ignore (input_char r) (*This is a way of checking that the process is closed.*)
      done
  with End_of_file 
    | No_more_input -> () 
    | e -> assert_failure (sprintf p"Expected %S, got exception %exn" string e)

(*let test_open_process_close_process () = (*Actually, this test shouldn't work*)
  try
    let r,w = Unix.open_process  "cat" in
      fprintf w p"%s\n" string;                       
      ignore (Unix.close_process (r, w));
      while true do
	ignore (input_char r); (*This is a way of checking that the process is closed.*)
      done
  with End_of_file 
    | No_more_input -> () 
    | e -> assert_failure (sprintf p"Expected %S, got exception %exn" string e)*)

let tests = "Unix" >::: [
  "Open process, then read_line"     >:: test_open_process_readline;
  "Open process, then clean up"      >:: test_open_process_cleanup
]
