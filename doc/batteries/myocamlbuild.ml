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


       (* For each ocamlfind package one inject the -package option when
       	* compiling, computing dependencies, generating documentation and
       	* linking. *)
       List.iter begin fun pkg ->
         flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
       end (find_packages ());

       (* Like -package but for extensions syntax. Morover -syntax is useless
       	* when linking. *)
       List.iter begin fun syntax ->
         flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
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
   {1 Utilities}
*)

(**
   Determine if a pathname must be compiled as threaded.

   A pathname is considered as threaded if it has tag ["pkg_threads"].
*)
let is_threaded name =
(*  Tags.print Format.std_formatter (tags_of_pathname name);*)
  let result = Tags.does_match (tags_of_pathname name) (Tags.of_list ["pkg_threads"]) in
    Printf.eprintf "file %s is %s\n" name (if result then "threaded" else "non-threaded");
    result

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
    let cl_use_boilerplate =
      [A"-package"; A "batteries.pa_type_conv.syntax,batteries,sexplib.syntax"]
    and cl_use_batteries   =
      let syntaxes = ["pa_openin";"pa_where";"pa_batteries";"pa_comprehension";"pa_strings"] in
      [A "-package";
       A (String.concat "," (List.map (Printf.sprintf "batteries.%s.syntax") syntaxes));
       A "-package"; A "batteries"]
    and cl_use_batteries_o = []
              (*[cl_use_batteries_o]: extensions which only make sense in original syntax*)
    and cl_camlp4o         = [A"-syntax";  A "camlp4o"]
    and cl_camlp4r         = [A"-syntax";  A "camlp4r"] in (*Temporary fix -- ocamlfind really behaves strangely*)

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
    flag ["ocaml"; "infer_interface"; "use_boilerplate"] & S cl_boilerplate_original;

      (** Tag [use_boilerplate_r] provides boilerplate syntax extensions,
	  in original syntax*)

    flag ["ocaml"; "compile";  "use_boilerplate_r"] & S cl_boilerplate_revised ;
    flag ["ocaml"; "ocamldep"; "use_boilerplate_r"] & S cl_boilerplate_revised ;
    flag ["ocaml"; "doc";      "use_boilerplate_r"] & S cl_boilerplate_revised ;
    flag ["ocaml"; "link";     "use_boilerplate_r"] & S cl_boilerplate_revised ;
    flag ["ocaml"; "infer_interface"; "use_boilerplate_r"] & S cl_boilerplate_revised;

    (** Tag [use_batteries] provides both package [batteries]
	and all syntax extensions, in original syntax. *)

    flag ["ocaml"; "compile";  "use_batteries"] & S cl_batteries_original ;
    flag ["ocaml"; "ocamldep"; "use_batteries"] & S cl_batteries_original ;
    flag ["ocaml"; "doc";      "use_batteries"] & S cl_batteries_original ;
    flag ["ocaml"; "link";     "use_batteries"] & S cl_batteries_original ;
    flag ["ocaml"; "infer_interface"; "use_batteries"] & S cl_batteries_original;

    (** Tag [use_batteries_r] provides both package [batteries]
	and all syntax extensions, in revised syntax. *)

    flag ["ocaml"; "compile";  "use_batteries_r"] & S cl_batteries_revised;
    flag ["ocaml"; "ocamldep"; "use_batteries_r"] & S cl_batteries_revised;
    flag ["ocaml"; "doc";      "use_batteries_r"] & S cl_batteries_revised;
    flag ["ocaml"; "link";     "use_batteries_r"] & S cl_batteries_revised;
    flag ["ocaml"; "infer_interface"; "use_batteries_r"] & S cl_batteries_revised;


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


(**
   {1 Using Batteries as a dynamically-linked library}

   Due to the model of OCaml, to do this, we actually invert the usual dynamic linking
   mechanism: Batteries is used as a main program (run.byte/run.native), while the
   program compiled by the user is actually compiled to a dynamically-loaded .cma/.cmxs.
*)
module Dynamic = struct

  (**
     Produce the name of the runner, depending on whether the code is threaded
  *)
  let decide_runner source runner =
    let dir = if is_threaded source then "@batteries_threads"
              else                       "@batteries_nothreads"
    in
    Printf.sprintf "%s/%s" dir runner

  let generate_stub runner bin =
    let init_loader = 
      Printf.sprintf 
	 "(*Locate Batteries loader*)
          Findlib.init ();;
          let binary = Findlib.resolve_path %S;;" 
	 runner
    and cl_loader =
(*(Pathname.concat Pathname.current_dir_name cma)(*Should do one of the following:
						 - replace this with the complete path
						 - build the path at run-time from [Sys.argv.(0)]
						 - find a way to embed the plug-in inside the .byte *)*)
      Printf.sprintf
	"(*Prepare command-line*)
         let buf = Buffer.create 80;;
         Printf.bprintf buf %S binary (Filename.concat (Filename.dirname (Sys.argv.(0))) %S);;"
	"%S %S -- "
	bin
    and start =
        "for i = 1 to Array.length Sys.argv - 1 do
           Printf.bprintf buf \"%S \" Sys.argv.(i)
         done;;
        let command = Buffer.contents buf in
        Printf.eprintf \"Requesting load of %S\\n...\\n%!\" command;
        Sys.command command
        " 
    in Printf.sprintf "%s\n%s\n%s\n" init_loader cl_loader start


  let before_options () = ()
  let after_rules () =
    begin
      rule ".cma to _dyn.ml (no threads)"
	~prod:"%_dynbyte.ml"
	~dep:"%.cma"
	begin fun env build -> 
	  let dest   = env "%_dynbyte.ml"
	  and bin    = env "%.cma" 
          and runner = decide_runner (env "%.dynbyte") "run.byte" in
	  let contents = generate_stub runner bin
	  in
	    tag_file dest ["pkg_findlib"]; (*Magically depend on findlib, without requiring user-intervention in _tags*)
	    tag_file (env "%_dynbyte.cmo")  ["pkg_findlib"];
	    tag_file (env "%_dynbyte.byte") ["pkg_findlib"];
	    Echo ([contents], dest)
	end;

      rule "_dynbyte.ml to .dynbyte"
	~prod:"%.dynbyte"
	~dep: "%_dynbyte.byte" 
	begin
	  fun env build ->
	  let dest = env "%.dynbyte"
	  and src  = env "%_dynbyte.byte"
	  in
	    tag_file dest ["pkg_findlib"];
	    Seq [Cmd (S[A"cp"; A src;  A dest]);
		 Cmd (S[A"ln"; A"-sf"; P (!Options.build_dir/dest); A Pathname.pwd]
		     )]
	end;

      rule ".cmx to cmxs"
	~prod:"%.cmxs"
	~dep:"%.cmx"
	begin fun env build ->
	  let dest = env "%.cmxs"
	  and src  = env "%.cmx" in
	  Cmd (S[A"ocamlopt"; A src; A "-shared"; A "-o"; A dest])
	end;

      rule ".cmxs to _dynnative.ml (no threads)"
	~prod:"%_dynnative.ml"
	~dep:"%.cmxs"
	begin fun env build ->
	  let dest   = env "%_dynnative.ml"
	  and bin    = env "%.cmxs"
          and runner = decide_runner (env "%.dynnative") "run.native" in
	  let contents = generate_stub runner bin
	  in
	    tag_file dest ["pkg_findlib"]; (*Magically depend on findlib, without requiring user-intervention in _tags*)
	    tag_file (env "%_dynnative.cmx")    ["pkg_findlib"];
	    tag_file (env "%_dynnative.native") ["pkg_findlib"];
	    Echo ([contents], dest)
	end;

      rule "_dynnative.ml to .dynnative"
	~prod:"%.dynnative"
	~dep: "%_dynnative.native" 
	begin
	  fun env build ->
	  let dest = env "%.dynnative"
	  and src  = env "%_dynnative.native"
	  in
	    tag_file dest ["pkg_findlib"];
	    Seq [Cmd (S[A"cp"; A src;  A dest]);
		 Cmd (S[A"ln"; A"-sf"; P (!Options.build_dir/dest); A Pathname.pwd]
		     )]
	end

    end
end

let _ = dispatch begin function
   | Before_options ->
       OCamlFind.before_options ();
       Batteries.before_options ();
       Dynamic.before_options ()
   | After_rules ->
       OCamlFind.after_rules ();
       Batteries.after_rules ();
       Dynamic.after_rules()

       
   | _ -> ()
end



(**
   which ocamlrun  ->   header

   print_backtrace -> ajouter "-b" après le header
**)
