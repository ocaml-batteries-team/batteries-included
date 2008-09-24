let _ = Arg.parse [("--capit", Arg.String (fun x -> print_endline (String.capitalize x)), "Capitalize letter")] ignore "Miscellaneous compile-time tools"
