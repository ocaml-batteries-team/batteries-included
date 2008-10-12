let _ = Arg.parse [("--capit", Arg.String (fun x -> print_endline (String.capitalize x)), "Capitalize letter");
		   ("--uncapit", Arg.String (fun x -> print_endline (String.uncapitalize x)), "Uncapitalize letter")] 
  ignore "Miscellaneous compile-time tools"
