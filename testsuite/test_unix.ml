open OUnit
open Print

let string = "hello world"

let test_open_process () =
  try
    let r,w = Unix.open_process "cat" in
      fprintf w p"%s\n" string;
      close_out w;
      match IO.read_line r with
	| s when s = string -> ()
	| s                 -> assert_failure (sprintf p"Expected %S, got %S" string s)
  with e -> assert_failure (sprintf p"Expected %S, got exception %exn" string e)



let tests = "Unix" >::: [
  "Open process" >:: test_open_process
]
