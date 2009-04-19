open OUnit
open File
open IO

(**Initialize data sample*)
let state  = Random.State.make [|0|]
let buffer = Array.of_enum (Enum.take 60 (Random.State.enum_int state 255))

(**Write sample to temporary file*)
let write buf =
  let (out, name) = open_temporary_out () in
    write_bytes out (Array.enum buffer);
    close_out out;
    name

(**Read from temporary file*)
let read_regular name =
  with_file_in name (fun inp -> Array.of_enum (bytes_of inp))

let read_mmap    name =
  with_file_in ~mode:[`mmap] name (fun inp -> Array.of_enum (bytes_of inp))


(**Actual tests*)

let print_array out =
  Printf.sprintf2 "%a" (Array.print ~sep:"; " Int.print) out

let test_read_back_tmp () =
  let name = write buffer in
  let aeq msg result = assert_equal ~printer:print_array ~msg buffer result in
    aeq "regular" (read_regular name);
    aeq "mmap" (read_mmap name)

let test_open_files_not_autoclosed () =
  let name = write buffer in
  let f    = open_in name in
    try
      let _ = IO.read_all f in
      let c = IO.read f in
        assert_failure (Printf.sprintf "Expecting: IO.No_more_input, got char %C" c)
    with
      | IO.No_more_input -> () (* pass *)
      | IO.Input_closed ->
          assert_failure "Expected: IO.No_more_input, got IO.Input_closed."
      | e ->
          let _ = IO.close_in f in
            assert_failure
              (Printf.sprintf "Expected: IO.No_more_input, got %s"
                 (Printexc.to_string e))

let test_open_close_many () =
  try
    for i = 0 to 10000 do
      Unix.unlink (write buffer)
    done;
    (* pass *)
  with Sys_error e -> assert_failure "Got Sys_error _"


let test_open_close_many_pervasives () =
  try
    for i = 0 to 10000 do
      let temp = Filename.temp_file "batteries" "test" in
      let oc   = open_out temp                         in
        Standard.output_string oc "test";
        close_out oc;
        Unix.unlink temp
    done;
    (* pass *)
  with Sys_error e -> assert_failure "Got Sys_error _"

let tests = "File" >::: [
  "Reading back output to temporary file" >:: test_read_back_tmp;
  "open_in'd files should not autoclose" >:: test_open_files_not_autoclosed;
  "opening and closing many files" >:: test_open_close_many;
  "opening and closing many files (Pervasives)" >:: test_open_close_many_pervasives;
]
