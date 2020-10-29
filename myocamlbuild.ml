(* ocamlbuild plugin for building Batteries.
 * Copyright (C) 2010 Michael Ekstrand
 *
 * Portions (hopefully trivial) from build/myocamlbuild.ml and the
 * Gallium wiki. *)

open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let ocamlfind x = S[A"ocamlfind"; A x]

let packs = "bigarray,num,str"

let doc_intro = "build/intro.text"
let mkconf = "build/mkconf.byte"
let compiler_libs = if Sys.ocaml_version.[0] = '4' then [A"-I"; A"+compiler-libs"] else []

let _ = dispatch begin function
  | Before_options ->
      (* Set up to use ocamlfind *)
      Options.ocamlc     := ocamlfind "ocamlc";
      Options.ocamlopt   := ocamlfind "ocamlopt";
      Options.ocamldep   := ocamlfind "ocamldep";
      Options.ocamldoc   := ocamlfind "ocamldoc";
      Options.ocamlmktop := ocamlfind "ocamlmktop"
  | Before_rules ->
      rule "build shared module"
        ~prod:"%.cmxs"
        ~dep:"%.cmxa"
        begin fun env build ->
          let tags = Tags.union
            (tags_of_pathname (env "%.cmxs"))
            (tags_of_pathname (env "%.cmxa"))
            ++ "ocaml" ++ "link" ++ "module"
          in
            Cmd(S[!Options.ocamlopt; A"-shared"; A"-linkall";
                  T tags; A"-o"; P(env "%.cmxs"); P(env "%.cmxa")])
        end;

      rule "process config file"
        ~prod:"%.ml"
        ~deps:["%.mlp"; mkconf]
        begin fun env build ->
          Cmd(S[A"ocamlrun"; P mkconf; P(env "%.mlp"); P(env "%.ml")])
        end;

      rule "process meta file"
        ~prod:"META"
        ~deps:["META.in"; mkconf]
        begin fun env build ->
          Cmd(S[A"ocamlrun"; P mkconf; P"META.in"; P"META"])
        end

  | After_rules ->

     (* use the home-made prefilter preprocessor, which
        looks for lines starting with ##Vx##, and delete just the tag or the
        whole line depending whether the x matches the ocaml major version
      *)
     let prefilter_rule ext =
       let src = "%." ^ ext ^ "v" in
       let tgt = "%." ^ ext in
       rule (Printf.sprintf "prefilter: %s --> %s" src tgt)
        ~prod:tgt
        ~deps:[src; "build/prefilter.byte"]
        (fun env _build ->
         Cmd (S [P "build/prefilter.byte"; P (env src); Sh ">"; Px (env tgt)]))
     in
     prefilter_rule "ml";
     prefilter_rule "mli";


     let ocaml_version =
       try Scanf.sscanf Sys.ocaml_version "%d.%d" (fun m n -> (m, n))
       with _ -> (* an arbitrary choice is better than failing here *)
         (4, 0)
     in

     begin
       (* BatConcreteQueue is either BatConcreteQueue_40x *)
       let queue_implementation =
         let major, minor = ocaml_version in
         if major < 4 || major = 4 && minor <= 2
         then "src/batConcreteQueue_402.ml"
         else "src/batConcreteQueue_403.ml" in
       copy_rule "queue implementation"
         queue_implementation "src/batConcreteQueue.ml";
     end;

     (* Rules to create libraries from .mllib instead of .cmo.
         We need this because src/batteries.mllib is hidden by src/batteries.ml *)
      rule ".mllib --> .cma"
        ~insert:`top
        ~prod:"%.cma"
        ~dep:"%.mllib"
        (Pack.Ocaml_compiler.byte_library_link_mllib "%.mllib" "%.cma");
      rule ".mllib --> .cmxa"
        ~insert:`top
        ~prod:"%.cmxa"
        ~dep:"%.mllib"
        (Pack.Ocaml_compiler.native_library_link_mllib "%.mllib" "%.cmxa");

    for n = 1 to 30 do
      List.iter (fun symbol ->
        flag ["ocaml"; "compile"; Printf.sprintf "warn_%s%d" symbol n]
          (S[A"-w"; A (Printf.sprintf "%s%d" symbol n)]);
        flag ["ocaml"; "compile"; Printf.sprintf "warn_error_%s%d" symbol n]
          (S[A"-warn-error"; A (Printf.sprintf "%s%d" symbol n)])
      ) ["+"; "-"; "@"]
    done;

      (* When one links an OCaml program, one should use -linkpkg *)
      flag ["ocaml"; "link"; "program"] & A"-linkpkg";

(* Causes build to fail on armel under ocaml 3.10.2
      flag ["ocaml"; "native"; "compile"] & A"-annot"; *)
(* A bad idea for future compatibility if OCaml introduces new warnings;
   we should use an explicit list of warnings here
      flag ["ocaml"; "compile"] & S[A"-warn-error"; A"A"];
*)
      flag ["ocaml"; "compile"] & S[A"-package"; A packs];
      flag ["ocaml"; "ocamldep"] & S[A"-package"; A packs];
      flag ["ocaml"; "doc"] & S[A"-package"; A packs];
      flag ["ocaml"; "link"] & S[A"-package"; A packs];
      flag ["ocaml"; "infer_interface"] & S[A"-package"; A packs];

      List.iter
        (fun pkg ->
          flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
          flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
          flag ["ocaml"; "compile"; "pkg_"^pkg] & S[A"-package"; A pkg];
          flag ["ocaml"; "link"; "pkg_"^pkg] & S[A"-package"; A pkg];
          ())
        ["oUnit"; "benchmark"];

      (* DON'T USE TAG 'thread', USE 'threads'
	 for compatibility with ocamlbuild *)
      flag ["ocaml"; "compile"; "threads"] & A"-thread";
      flag ["ocaml"; "link"; "threads"] & A"-thread";
      flag ["ocaml"; "doc"; "threads"] & S[A"-I"; A "+threads"];

      flag ["ocaml"; "doc"] & S[A"-hide-warnings"; A"-sort"];

      flag ["ocaml"; "compile"; "camlp4rf"] &
        S[A"-package"; A"camlp4.lib"; A"-pp"; A"camlp4rf"];
      flag ["ocaml"; "ocamldep"; "camlp4rf"] &
        S[A"-package"; A"camlp4.lib"; A"-pp"; A"camlp4rf"];

      flag ["ocaml"; "compile"; "camlp4of"] &
        S[A"-package"; A"camlp4.lib"; A"-pp"; A"camlp4of"];
      flag ["ocaml"; "ocamldep"; "camlp4of"] &
        S[A"-package"; A"camlp4.lib"; A"-pp"; A"camlp4of"];

      flag ["ocaml"; "compile"; "syntax_camlp4o"] &
        S[A"-syntax"; A"camlp4o"; A"-package"; A"camlp4"];
      flag ["ocaml"; "ocamldep"; "syntax_camlp4o"] &
        S[A"-syntax"; A"camlp4o"; A"-package"; A"camlp4"];

      ocaml_lib "src/batteries";
      ocaml_lib "src/batteriesThread";

      flag ["ocaml"; "compile"; "compiler-libs"] & S compiler_libs;
      flag ["ocaml"; "link"; "compiler-libs"] & S compiler_libs;
      flag ["ocaml"; "ocamldep"; "compiler-libs"] & S compiler_libs;

      flag ["ocaml"; "link"; "linkall"] & S[A"-linkall"];

      if ocaml_version = (4, 0) then begin
        (* OCaml 4.00 has -bin-annot but no ocamlbuild flag *)
        flag ["ocaml"; "bin_annot"; "compile"] (A "-bin-annot");
        flag ["ocaml"; "bin_annot"; "pack"] (A "-bin-annot");
      end;
(*
      dep ["ocaml"; "link"; "include_tests"; "byte"] &
	[Pathname.mk "qtest/test_mods.cma"];
      dep ["ocaml"; "link"; "include_tests"; "native"] &
	[Pathname.mk "qtest/test_mods.cmxa"]; *)

      (* Some .mli files use INCLUDE "foo.mli" to avoid interface duplication;

         The problem is that the automatic dependency detector of
         ocamlbuild doesn't detect the implicit dependency on the
         included .mli, and doesn't copy it into _build before
         preprocessing the including file.

         Here, we add flags denoting explicit dependencies on the
         included .mli. This solution comes from the following
         explanation of Xavier Clerc:
           http://caml.inria.fr/mantis/print_bug_page.php?bug_id=5162
      *)
      dep ["ocaml"; "doc"; "extension:html"] & [doc_intro];
      flag ["ocaml"; "doc"; "extension:html"] &
        (S[A"-t"; A"Batteries user guide";
           A"-intro"; P doc_intro;
           A"-colorize-code"]);

  | _ -> ()
end
