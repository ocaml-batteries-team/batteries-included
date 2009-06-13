open OUnit
open File
open IO
open Print

(**Initialize data sample*)
let state  = Random.State.make [|0|];;
let buffer = Array.of_enum (Enum.take 60 (Random.State.enum_int state 255));;

(**Write sample to temporary file*)
let write buf =
  let (out, name) = open_temporary_out () in
    write_bytes out (Array.enum buf);
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
        assert_failure (sprintf p"Expecting: IO.No_more_input, got char %C" c)
    with
      | IO.No_more_input -> () (* pass *)
      | IO.Input_closed ->
          assert_failure "Expected: IO.No_more_input, got IO.Input_closed."
      | e ->
          let _ = IO.close_in f in
            assert_failure
              (sprintf p"Expected: IO.No_more_input, got %s"
                 (Printexc.to_string e))

let test_open_close_many () =
  try
    for i = 0 to 10000 do
      Unix.unlink (write buffer)
    done;
    (* pass *)
  with Sys_error e -> assert_failure (sprintf p"Got Sys_error %S" e)


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
  with Sys_error e -> assert_failure (sprintf p"Got Sys_error %S" e)

let test_no_append () =
  try
    let temp   = Path.temp_file "ocaml_batteries" "noappend_test" in
    let out    = open_out temp                       in
    let _      = write_bytes out (Array.enum buffer) in
    let _      = close_out out                       in
    let size_1 = size_of temp                        in
    let out    = open_out temp                       in
    let _      = write_bytes out (Array.enum buffer) in
    let _      = close_out out                       in
    let size_2 = size_of temp                        in
      if size_1 <> size_2 then assert_failure
	(sprintf p"Expected two files with size %d, got one with size %d and one with size %d" size_1 size_1 size_2)
  with Sys_error e -> assert_failure (sprintf p"Got Sys_error %S" e)

let test_append () =
  try
    let temp   = Path.temp_file "ocaml_batteries" "noappend_test" in
    let out    = open_out ~mode:[`append] temp       in
    let _      = write_bytes out (Array.enum buffer) in
    let _      = close_out out                       in
    let size_1 = size_of temp                        in
    let out    = open_out ~mode:[`append] temp       in
    let _      = write_bytes out (Array.enum buffer) in
    let _      = close_out out                       in
    let size_2 = size_of temp                        in
      if size_2 <> 2*size_1 then assert_failure
	(sprintf p"Expected a files with size %d, got a first chunk with size %d and a second chunk with size %d" 
	   (2*size_1) size_1 size_2)
  with Sys_error e -> assert_failure (sprintf p"Got Sys_error %S" e)

let tests = "File" >::: [
  "Reading back output to temporary file" >:: test_read_back_tmp;
  "open_in'd files should not autoclose" >:: test_open_files_not_autoclosed;
  "opening and closing many files" >:: test_open_close_many;
  "opening and closing many files (Pervasives)" >:: test_open_close_many_pervasives;
  "default truncation of files" >:: test_no_append;
  "appending to a file" >:: test_append
]
