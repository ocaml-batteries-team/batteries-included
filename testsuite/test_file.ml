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

let test_3 = ("File: open_in'd files should not autoclose",
	    fun () ->
	      let name = write buffer in
	      let f    = open_in name in
	      try
		let _ = IO.read_all f in
		let c = IO.read f in Testing.Fail
                  (Printf.sprintf "Hoping: IO.No_more_input\n\tGot:    char \'%c\'" c)
	      with
	      | IO.No_more_input ->
		let _ = IO.close_in f in Testing.Pass
	      | IO.Input_closed ->
		Testing.Fail "Hoping: IO.No_more_input\n\tGot:    IO.Input_closed"
	      | _ ->
		let _ = IO.close_in f in
		Testing.Fail "Hoping: IO.No_more_input\n\tGot:    (Different exception)"
	    )


let test_4 = ("File: opening and closing many files",
	      fun () ->
	      try
		for i = 0 to 10000 do
		  ignore (write buffer)
	      done;Testing.Pass
	      with Sys_error e -> Testing.Fail e)


let test_5 = ("File: opening and closing many files (Pervasives)",
	      fun () ->
		try
		for i = 0 to 10000 do
		  let temp = Filename.temp_file "batteries" "test" in
		  let oc   = open_out temp                         in
		    Standard.output_string oc "test";
		    close_out oc
	      done;Testing.Pass
	    with Sys_error e -> Testing.Fail e )
