(* OASIS shim for building *)

let _ = 
  match Sys.argv.(1) with
    | "-configure" -> print_endline "Configure not necessary"; 0
    | "-build" -> Sys.command "make all"
    | "-test" -> Sys.command "make test"
    | "-doc" -> Sys.command "make doc"
    | "-install" -> Sys.command "make install"
    | "-uninstall" -> Sys.command "make uninstall"
    | "-reinstall" -> Sys.command "make reinstall"
    | "-clean" -> Sys.command "make clean"
    | "-distclean" -> print_endline "Distclean not supported"; 1
    | "-all" -> Sys.command "make all test doc"
    | o -> print_endline ("Unknown option: " ^ o); 1
