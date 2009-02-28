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
  Array.print ~sep:"; " Int.print out

let test_1 = ("File: Reading back output to temporary file",
	     fun () ->

	       let name = write buffer      in
	       let found= read_regular name in
		 if found = buffer then Testing.Pass
		 else Testing.Fail (Printf.sprintf2 "Hoping: %a\n\tGot:    %a" print_array buffer print_array found)
	    )


let test_2 = ("File: MMap Reading back output to temporary file",
	     fun () ->

	       let name = write buffer      in
	       let found= read_mmap name    in
		 if found = buffer then Testing.Pass
		 else Testing.Fail (Printf.sprintf2 "Hoping: %a\n\tGot:    %a" print_array buffer print_array found)
	    )
