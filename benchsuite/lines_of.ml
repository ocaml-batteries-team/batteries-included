let readfile fn =
  let ic = open_in fn in
  let r = ref [] in
  (try while true do
     let l = input_line ic in
     r := l :: !r
   done with End_of_file -> ()) ;
  close_in ic ;
  List.rev !r

let readfile_batteries fn =
  let open Batteries in
  File.lines_of fn |> List.of_enum

let file_lines_of fn =
  let ic = open_in fn in
  BatEnum.suffix_action
    (fun () -> close_in ic)
    (BatEnum.from (fun () -> try input_line ic with End_of_file -> raise BatEnum.No_more_elements))

let rfb2 fn = 
   BatList.of_enum (file_lines_of fn)

let read_line i =
	let b = Buffer.create 80 in
	let cr = ref false in
	let rec loop() =
		let c = BatIO.read i in
		match c with
		| '\n' ->
			()
		| '\r' ->
			cr := true;
			loop()
		| _ when !cr ->
			cr := false;
			Buffer.add_char b '\r';
			Buffer.add_char b c;
			loop();
		| _ ->
			Buffer.add_char b c;
			loop();
	in
	try
		loop();
		Buffer.contents b
	with
		BatInnerIO.No_more_input ->
			if !cr then Buffer.add_char b '\r';
			if Buffer.length b > 0 then
				Buffer.contents b
			else
				raise BatInnerIO.No_more_input

(** [apply_enum f x] applies [f] to [x] and converts exceptions
    [No_more_input] and [Input_closed] to [BatEnum.No_more_elements]*)
let apply_enum do_close f x =
  try f x
  with 
    | BatInnerIO.No_more_input -> raise BatEnum.No_more_elements
    | BatInnerIO.Input_closed  -> do_close := false; raise BatEnum.No_more_elements

(** [close_at_end input e] returns an enumeration which behaves as [e]
    and has the secondary effect of closing [input] once everything has
    been read.*)
let close_at_end do_close input e =
  BatEnum.suffix_action (fun () -> if !do_close then BatIO.close_in input) e

let make_enum f input =
  let do_close = ref true in
  close_at_end do_close input (BatEnum.from (fun () -> apply_enum do_close f input))

let lines_of input = make_enum read_line input

let rfb3 fn = BatList.of_enum (lines_of (BatFile.open_in fn))

let wrap f () = f "setup.ml"

let () = 
  Bench.config.Bench.samples <- 300;
  Bench.bench ["readfile", wrap readfile;
               "readfile_batteries", wrap readfile_batteries;
               "file_lines_of", wrap rfb2;
(*               "rfb3", wrap rfb3;*)
              ]
