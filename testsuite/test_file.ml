open OUnit
open BatFile
open BatIO
open BatPrint
open BatPervasives

(**Initialize data sample*)
let state  = BatRandom.State.make [|0|];;
let buffer = BatArray.of_enum (BatEnum.take 60 (BatRandom.State.enum_int state 255));;

(**Write sample to temporary file*)
let write buf =
  let (out, name) = open_temporary_out ~mode:[`delete_on_exit] () in
    write_bytes out (BatArray.enum buf);
    close_out out;
    name

(**Read from temporary file*)
let read_regular name =
  with_file_in name (fun inp -> BatArray.of_enum (bytes_of inp))

let read_mmap    name =
  with_file_in ~mode:[`mmap] name (fun inp -> BatArray.of_enum (bytes_of inp))

let temp_file ?(autoclean = true) pref suff =
  let tf = Filename.temp_file pref suff in
  if autoclean then Pervasives.at_exit (fun () -> try Unix.unlink tf with _ -> ()) ;
  tf

(**Actual tests*)

let print_array out =
  BatPrintf.sprintf2 "%a" (BatArray.print ~sep:"; " BatInt.print) out

let test_read_back_tmp () =
  let name = write buffer in
  let aeq msg result = assert_equal ~printer:print_array ~msg buffer result in
    aeq "regular" (read_regular name);
    aeq "mmap" (read_mmap name)

let test_open_files_not_autoclosed () =
  let name = write buffer in
  let f    = open_in name in
    try
      let _ = BatIO.read_all f in
      let c = BatIO.read f in
        assert_failure (BatPrintf.sprintf "Expecting: BatIO.No_more_input, got char %C" c)
    with
      | BatIO.No_more_input -> () (* pass *)
      | BatIO.Input_closed ->
          assert_failure "Expected: BatIO.No_more_input, got BatIO.Input_closed."
      | e ->
          let _ = BatIO.close_in f in
            assert_failure
              (BatPrintf.sprintf "Expected: BatIO.No_more_input, got %s"
                 (Printexc.to_string e))

let test_open_close_many () =
  try
    for i = 0 to 10000 do
      Unix.unlink (write buffer)
    done;
    (* pass *)
  with Sys_error e -> assert_failure (BatPrintf.sprintf "Got Sys_error %S" e)


let test_open_close_many_pervasives () =
  try
    for i = 0 to 10000 do
      let temp = temp_file "batteries" "test" in
      let oc   = open_out temp                         in
        output_string oc "test";
        close_out oc
    done;
    (* pass *)
  with Sys_error e -> assert_failure (BatPrintf.sprintf "Got Sys_error %S" e)

let test_no_append () =
  try
    let temp   = temp_file "ocaml_batteries" "noappend_test" in
    let out    = open_out temp                       in
    let _      = write_bytes out (BatArray.enum buffer) in
    let _      = close_out out                       in
    let size_1 = size_of temp                        in
    let out    = open_out temp                       in
    let _      = write_bytes out (BatArray.enum buffer) in
    let _      = close_out out                       in
    let size_2 = size_of temp                        in
      if size_1 <> size_2 then assert_failure
	(BatPrintf.sprintf "Expected two files with size %d, got one with size %d and one with size %d" size_1 size_1 size_2)
  with Sys_error e -> assert_failure (BatPrintf.sprintf "Got Sys_error %S" e)

let test_append () =
  try
    let temp   = temp_file "ocaml_batteries" "append_test" in
    let out    = open_out ~mode:[`append] temp       in
    let _      = write_bytes out (BatArray.enum buffer) in
    let _      = close_out out                       in
    let size_1 = size_of temp                        in
    let out    = open_out ~mode:[`append] temp       in
    let _      = write_bytes out (BatArray.enum buffer) in
    let _      = close_out out                       in
    let size_2 = size_of temp                        in
      if size_2 <> 2*size_1 then assert_failure
	(BatPrintf.sprintf "Expected a files with size %d, got a first chunk with size %d and a second chunk with size %d" 
	   (2*size_1) size_1 size_2)
  with Sys_error e -> assert_failure (BatPrintf.sprintf "Got Sys_error %S" e)

let tests = "File" >::: [
  "Reading back output to temporary file" >:: test_read_back_tmp;
  "open_in'd files should not autoclose" >:: test_open_files_not_autoclosed;
  "opening and closing many files" >:: test_open_close_many;
  "opening and closing many files (Pervasives)" >:: test_open_close_many_pervasives;
  "default truncation of files" >:: test_no_append;
  "appending to a file" >:: test_append
]
