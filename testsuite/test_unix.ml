open OUnit
open BatPrintf
open BatIO

let string = "hello world"

let test_open_process_readline () =
  try
    let r,w = BatUnix.open_process "cat" in
      fprintf w "%s\n" string;
      close_out w;
      match BatIO.read_line r with
	| s when s = string -> ()
	| s                 -> assert_failure (BatPrintf.sprintf "Expected %S, got %S" string s)
  with e -> assert_failure (BatPrintf.sprintf "Expected %S, got exception %s" string (Printexc.to_string e))

let test_open_process_cleanup () =
  try
    let r,w = BatUnix.open_process "cat" in
      BatPrintf.fprintf w "%s\n" string;                       
      close_out w;
      while true do
	ignore (BatPervasives.input_char r) (*This is a way of checking that the process is closed.*)
      done
  with End_of_file 
    | No_more_input -> () 
    | e -> assert_failure (BatPrintf.sprintf "Expected %S, got exception %s" string (Printexc.to_string e))


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
