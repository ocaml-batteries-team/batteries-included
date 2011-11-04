open Ocamlbuild_plugin
open Command (* no longer needed for OCaml >= 3.10.2 *)

(**
   Overview of tags:
   - [pkg_batteries] to use Batteries as a library, without syntax extensions
   - [use_batteries] and [use_batteries_r] to use both Batteries and all the non-destructive syntax extensions
   - [pkg_sexplib.syntax] with [syntax_camlp4o] or [syntax_camlp4r] for sexplib
*)


(**
   {1 OCamlFind}
*)

let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read

let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

module OCamlFind =
struct
  (* this lists all supported packages *)
  let find_packages () =
    blank_sep_strings &
      Lexing.from_string &
      run_and_read "ocamlfind list | cut -d' ' -f1"

  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]

  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]

  let  before_options () =
    (* by using Before_options one let command line options have an higher priority *)
    (* on the contrary using After_options will guarantee to have the higher priority *)

    (* override default commands by ocamlfind ones *)
    Options.ocamlc     := ocamlfind & A"ocamlc";
    Options.ocamlopt   := ocamlfind & A"ocamlopt";
    Options.ocamldep   := ocamlfind & A"ocamldep";
    Options.ocamldoc   := ocamlfind & A"ocamldoc";
    Options.ocamlmktop := ocamlfind & A"ocamlmktop"

  let get_ocamldoc_directory () =
    let ocamldoc_directory = run_and_read "ocamlfind ocamldoc -customdir" in
    let length             = String.length ocamldoc_directory             in
      assert (length != 0);
      let char = ocamldoc_directory.[length - 1] in
	if (char = '\n') || (char = '\r') then String.sub ocamldoc_directory 0 (length - 1)
	else ocamldoc_directory

  let after_rules () =
       (* When one link an OCaml library/binary/package, one should use -linkpkg *)
       flag ["ocaml"; "byte";   "link"; "program"] & A"-linkpkg";
       flag ["ocaml"; "native"; "link"; "program"] & A"-linkpkg";
       flag ["ocaml"; "native"; "link"; "toplevel"] & A"-linkpkg";


       (* For each ocamlfind package one inject the -package option when
	* compiling, computing dependencies, generating documentation and
	* linking. *)
       List.iter begin fun pkg ->
         flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
       end (find_packages ());

       (* Like -package but for extensions syntax. Morover -syntax is useless
	* when linking. *)
       List.iter begin fun syntax ->
         flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
       end (find_syntaxes ());

       (* The default "thread" tag is not compatible with ocamlfind.
          Indeed, the default rules add the "threads.cma" or "threads.cmxa"
          options when using this tag. When using the "-linkpkg" option with
          ocamlfind, this module will then be added twice on the command line.

          To solve this, one approach is to add the "-thread" option when using
          the "threads" package using the previous plugin.
        *)
       flag ["ocaml"; "pkg_threads"; "compile"]  (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"]     (S[A "-thread"]);
end

(**
   {1 OCaml Batteries Included}
*)

module Batteries =
struct
  let before_options () = ()

  let after_rules () =
    flag ["ocaml"; "link"; "byte";   "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"; A "odoc_info.cma"]);
    flag ["ocaml"; "link"; "native"; "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"(*; A "odoc_info.cmxa"*)]);
    flag ["ocaml"; "docfile";        "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);
    flag ["ocaml"; "docdir";         "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);
    flag ["ocaml"; "doc";            "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);

    (*The command-line for [use_batteries] and [use_batteries_r]*)

    let cl_use_boilerplate = [A "-package"; A "batteries"]
    and cl_use_batteries   = [A "-package"; A "batteries"]
    and cl_use_batteries_o = []
              (*[cl_use_batteries_o]: extensions which only make sense in original syntax*)
    and cl_camlp4o         = [A"-syntax";  A "camlp4o"]
    and cl_camlp4r         = [A"-syntax";  A "camlp4r"] in

    let cl_boilerplate_original = cl_use_boilerplate @ cl_camlp4o
    and cl_boilerplate_revised  = cl_use_boilerplate @ cl_camlp4r
    and cl_batteries_original   = cl_use_batteries   @ cl_use_batteries_o @ cl_camlp4o
    and cl_batteries_revised    = cl_use_batteries   @ cl_camlp4r in

      (** Tag [use_boilerplate] provides boilerplate syntax extensions,
	  in original syntax*)

    flag ["ocaml"; "compile";  "use_boilerplate"] & S cl_boilerplate_original ;
    flag ["ocaml"; "ocamldep"; "use_boilerplate"] & S cl_boilerplate_original ;
    flag ["ocaml"; "doc";      "use_boilerplate"] & S cl_boilerplate_original ;
    flag ["ocaml"; "link";     "use_boilerplate"] & S cl_boilerplate_original ;

      (** Tag [use_boilerplate_r] provides boilerplate syntax extensions,
	  in original syntax*)

    flag ["ocaml"; "compile";  "use_boilerplate_r"] & S cl_boilerplate_revised ;
    flag ["ocaml"; "ocamldep"; "use_boilerplate_r"] & S cl_boilerplate_revised ;
    flag ["ocaml"; "doc";      "use_boilerplate_r"] & S cl_boilerplate_revised ;
    flag ["ocaml"; "link";     "use_boilerplate_r"] & S cl_boilerplate_revised ;

    (** Tag [use_batteries] provides both package [batteries]
	and all syntax extensions, in original syntax. *)

    flag ["ocaml"; "compile";  "use_batteries"] & S cl_batteries_original ;
    flag ["ocaml"; "ocamldep"; "use_batteries"] & S cl_batteries_original ;
    flag ["ocaml"; "doc";      "use_batteries"] & S cl_batteries_original ;
    flag ["ocaml"; "link";     "use_batteries"] & S cl_batteries_original ;

    (** Tag [use_batteries_r] provides both package [batteries]
	and all syntax extensions, in revised syntax. *)

    flag ["ocaml"; "compile";  "use_batteries_r"] & S cl_batteries_revised;
    flag ["ocaml"; "ocamldep"; "use_batteries_r"] & S cl_batteries_revised;
    flag ["ocaml"; "doc";      "use_batteries_r"] & S cl_batteries_revised;
    flag ["ocaml"; "link";     "use_batteries_r"] & S cl_batteries_revised


(*    flag ["ocaml"; "compile";  "use_batteries"] & S[A "-verbose";
						    A"-package"; A "batteries.syntax.full";
						    A"-syntax";  A "batteries.syntax.full"];
    flag ["ocaml"; "ocamldep"; "use_batteries"] & S[A "-verbose";
						    A"-package"; A "batteries.syntax.full";
						    A"-syntax"; A "batteries.syntax.full"];
    flag ["ocaml"; "doc";      "use_batteries"] & S[A "-verbose";
						    A"-package"; A "batteries.syntax.full";
						    A"-syntax"; A "batteries.syntax.full"];
    flag ["ocaml"; "link";     "use_batteries"] & S[A "-verbose";
						    A"-package"; A "batteries.syntax.full";
						    A"-syntax"; A "batteries.syntax.full"];*)


end

let _ = dispatch begin function
   | Before_options ->
       OCamlFind.before_options ();
       Batteries.before_options ()
   | After_rules ->
       OCamlFind.after_rules ();
       Batteries.after_rules ()


   | _ -> ()
end



(**
   which ocamlrun  ->   header

   print_backtrace -> ajouter "-b" après le header
**)
