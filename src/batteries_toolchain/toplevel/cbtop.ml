(** This creates a toplevel that uses a special initfile.  The first argument
    to the toplevel *must* be a .ml file which is the special initialization
    file.  Unlike the -init option of the standard toplevel, this special
    initfile is *always* run, even in script mode.
*)


(** defining a new parser *)
let initialized = ref false
let mkparse init_file mode origparse =
  let parse lb =
(*    Printf.eprintf "Parsing in mode %S\n%!" mode;*)
    if !initialized then
      begin
(*	Printf.eprintf "Regular parser\n%!";*)
	origparse lb
      end
    else
      begin
	initialized := true;
(*	Printf.eprintf "Custom parser\n%!";*)
	ignore (Toploop.use_silently Format.std_formatter init_file);
        origparse lb
      end
  in parse


(** starting the toplevel with new reader *)
let _ = 
  let init_file = Sys.argv.(1) in
  Arg.current := 1;
  Toploop.parse_use_file         := mkparse init_file "file"    !Toploop.parse_use_file;
  Toploop.parse_toplevel_phrase  := mkparse init_file "phrase"  !Toploop.parse_toplevel_phrase;
  Topmain.main ()
