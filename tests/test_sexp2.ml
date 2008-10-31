(* Checking that pa_sexp doesn't prevent from writing sub-modules.*)
(*Actually, for the moment, it does, so we're removing pa_sexp from the list of automatically
  used syntax extensions.*)

module A = struct
  print_endline "test_sexp2 succeeded"
end
