open OUnit

(*Source code which needs to be executed*)

let make_temporary_file content =
  File.with_temporary_out ~suffix:".ml"
    begin
      fun out name ->
	String.print out content;
	name
    end

let expected = "read-only string";;

open Compilers
open IO

let test_from_source_file () =
  let source    = "Print.printf p\"%sc\" ro\"read-only string\";;" in
  let generated_file = make_temporary_file source in
  let temp_name = Filename.temp_file "ocaml" "test" in
    ignore (Sys.command (string_of_command (ocaml [generated_file]) ^ " > " ^ temp_name));
    let obtained = File.with_file_in temp_name read_all in
      assert_equal ~printer:(Printf.sprintf "%S") expected obtained

let test_from_simulated_cmdline () =
  let temp_name = Filename.temp_file "ocaml" "test" in
  let source    = Print.sprintf
                    p"File.with_file_out %S (fun out -> Print.fprintf out p\"%%sc\" ro\"read-only string\");;\n"
                    temp_name in
  let generated_file = make_temporary_file source in
  let command   = string_of_command (ocaml []) ^ " < " ^ generated_file  ^ " > /dev/null " in
    (*	 Printf.eprintf "Running %S\nWriting to file %S\n%!" command temp_name;*)
    ignore (Sys.command command);
    flush_all ();
    let obtained  = File.with_file_in temp_name read_all in
      assert_equal ~printer:(Printf.sprintf "%S") expected obtained

(*
let test_1 =
  ("OCaml: Testing from source file", fun () ->
     try
       let generated_file =
	 File.with_temporary_out ~suffix:".ml"
	   begin
	     fun out name ->
	       String.print out source;
	       name
	   end
       in
       let temp_name = Filename.temp_file "ocaml" "test" in
	 ignore (Sys.command (string_of_command (ocaml [generated_file]) ^ "> temp_name"));
	 let obtained = File.with_file_in temp_name read_all
	 in
	   if obtained = expected then Testing.Pass
	   else Testing.Fail (Printf.sprintf "Expected: %S\n\tObtained: %S\n" expected obtained)
     with e -> Testing.Err (Printexc.to_string e))

let test_2 =
  ("OCaml: Testing from simulated command-line", fun () ->
     try
       let command   = string_of_command (ocaml []) in
       let (pin, pout)=Unix.open_process ~cleanup:true command    in
	 String.print pout source;
	 close_out pout;
	 let obtained = read_all pin in
	   if obtained = expected then Testing.Pass
	   else Testing.Fail (Printf.sprintf "Expected: %S\n\tObtained: %S\n" expected obtained)
     with e -> Testing.Err (Printexc.to_string e))

*)

let tests = "Toplevel" >::: [
  "From source file" >:: test_from_source_file;
  "From simulated command-line" >:: test_from_simulated_cmdline;
]
