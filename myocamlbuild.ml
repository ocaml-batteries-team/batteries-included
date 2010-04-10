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
  | After_rules ->
      flag ["ocaml"; "compile"] & S[A"-package"; A packs];
      flag ["ocaml"; "ocamldep"] & S[A"-package"; A packs];
      flag ["ocaml"; "doc"] & S[A"-package"; A packs];
      flag ["ocaml"; "link"] & S[A"-package"; A packs];
      flag ["ocaml"; "infer_interface"] & S[A"-package"; A packs];

      flag ["ocaml"; "compile"; "threads"] & A"-thread";
      flag ["ocaml"; "link"; "threads"] & A"-thread";
      flag ["ocaml"; "doc"; "threads"] & S[A"-I"; A "+threads"];

      flag ["ocaml"; "doc"] & A"-hide-warnings";
  | _ -> ()
end
