(* ocamlbuild plugin for building Batteries.
 * Copyright (C) 2010 Michael Ekstrand
 *
 * Portions (hopefully trivial) from build/myocamlbuild.ml and the
 * Gallium wiki. *)

open Ocamlbuild_plugin

let ocamlfind x = S[A"ocamlfind"; A x]

let packs = String.concat "," ["camomile"; "num"; "str"]

let mkconf = "build/mkconf.byte"
let pa_llist = "src/syntax/pa_llist/pa_llist.cmo"
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
        ~deps:["%.mlp"; "VERSION"; mkconf]
        begin fun env build ->
          Cmd(S[A"ocamlrun"; P mkconf; P(env "%.mlp"); P(env "%.ml")])
        end;

      rule "process meta file"
        ~prod:"META"
        ~deps:["META.in"; "VERSION"; mkconf]
        begin fun env build ->
          Cmd(S[A"ocamlrun"; P mkconf; P"META.in"; P"META"])
        end

  | After_rules ->
      (* When one links an OCaml program, one should use -linkpkg *)
      flag ["ocaml"; "link"; "program"] & A"-linkpkg";

(* Causes build to fail on armel under ocaml 3.10.2
      flag ["ocaml"; "native"; "compile"] & A"-annot"; *)
      flag ["ocaml"; "compile"] & S[A"-warn-error"; A"A"];

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

      flag ["ocaml"; "compile";  "with_pa_llist"] &
        S[A"-ppopt"; P pa_llist];
      flag ["ocaml"; "ocamldep";  "with_pa_llist"] &
        S[A"-ppopt"; P pa_llist];
      dep ["ocaml"; "ocamldep"; "with_pa_llist"] [pa_llist];

      ocaml_lib "qtest/test_mods";
      ocaml_lib "src/batteries";

      flag ["ocaml"; "compile"; "compiler-libs"] & S compiler_libs;
      flag ["ocaml"; "link"; "compiler-libs"] & S compiler_libs;
      flag ["ocaml"; "ocamldep"; "compiler-libs"] & S compiler_libs;


      flag ["ocaml"; "link"; "linkall"] & S[A"-linkall"];
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
      dep ["pset_mli"] [Pathname.concat "src" "batPSet.mli"];
      dep ["pmap_mli"] [Pathname.concat "src" "batPMap.mli"];



  | _ -> ()
end
