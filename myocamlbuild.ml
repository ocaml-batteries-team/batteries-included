(* ocamlbuild plugin for building Batteries.  
 * Copyright (C) 2010 Michael Ekstrand
 * 
 * Portions (hopefully trivial) from build/myocamlbuild.ml and the
 * Gallium wiki. *)

open Ocamlbuild_plugin

let ocamlfind x = S[A"ocamlfind"; A x]

let packs = String.concat "," ["camomile"; "num"; "str"]

let _ = dispatch begin function
  | Before_options ->
      (* Set up to use ocamlfind *)
      Options.ocamlc     := ocamlfind "ocamlc";
      Options.ocamlopt   := ocamlfind "ocamlopt";
      Options.ocamldep   := ocamlfind "ocamldep";
      Options.ocamldoc   := ocamlfind "ocamldoc";
      Options.ocamlmktop := ocamlfind "ocamlmktop"
  | Before_rules ->
      rule "preprocess config file"
        ~prod:"%.ml"
        ~deps:["%.mlp"; "Makefile"]
        begin fun env build ->
          Cmd(S[A"make"; P(env "%.ml")])
        end;

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
        end

  | After_rules ->
      (* When one links an OCaml program, one should use -linkpkg *)
      flag ["ocaml"; "link"; "program"] & A"-linkpkg";

      flag ["ocaml"; "compile"] & S[A"-package"; A packs];
      flag ["ocaml"; "ocamldep"] & S[A"-package"; A packs];
      flag ["ocaml"; "doc"] & S[A"-package"; A packs];
      flag ["ocaml"; "link"] & S[A"-package"; A packs];
      flag ["ocaml"; "infer_interface"] & S[A"-package"; A packs];

      flag ["ocaml"; "infer_interface"; "pkg_oUnit"] & S[A"-package"; A"oUnit"];
      flag ["ocaml"; "ocamldep"; "pkg_oUnit"] & S[A"-package"; A"oUnit"];
      flag ["ocaml"; "compile"; "pkg_oUnit"] & S[A"-package"; A"oUnit"];
      flag ["ocaml"; "link"; "pkg_oUnit"] & S[A"-package"; A"oUnit"];

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
  | _ -> ()
end
