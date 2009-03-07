(**

   Build a .ml and a .mli from a .dist file.


Format of a .dist file:

{[

(*%MODULE InnerFoo = Foo*)

(*Module comment*)
module Foo = A.Module.Path     (*%mli "a/file/path/foo.mli"*)

(*Module comment*)
module Bar = Another.Module.Path.Foo (*%mli "another/file/path/foo.mli" %submodule "Bar"*)

(*Module comment*)
module Sna = struct
   module Toto = Yet.Another.Module.Path (*%mli "a/file/path/foo.mli"*)

       (*...same grammar...*)
end
]}

Producing a .ml is trivial, there's nothing to do.

Producing a .mli is more complex:
- parse the .dist file, keeping comments which don't start with % (gasp)
- build the list of substitutions 
   -- here, every occurrence of [InnerFoo] must become [Foo]
   -- here, every occurrence of [A.Module.Path] must become [Foo]
   -- here, every occurrence of [Yet.Another.Module.Path] must become [Sna.Toto]
- build the list of source .mli
- if necessary, generate each source .mli (so this needs to be done from myocamlbuild.ml)
- from each %mli directive
   -- build a temporary file, obtained by
     ---- extracting only the necessary submodules (remove [module Stuff : sig] and [end])
     ---- performing all the substitutions in the list
   -- invoke ocamldep
   -- parse the result of ocamldep and deduce a list of dependencies for the destination module (here, [Foo], [Bar], [Toto])
- bubble dependencies upwards in the tree of modules (here, [Sna] should inherit all the dependencies of [Toto])
- perform topological sort on dependencies
- assuming topological sort has succeeded, generate a .mli where every module alias is replaced by the contents
  of the corresponding temporary .mli
- write down all of this to a file.

easy, isn't it?
*)

(**Information regarding where to find the signature for a module.*)
type sigsource =
    {
      path       : string (**Path towards the .mli file containing the data for this module, e.g. "src/core/extlib/extList.mli"*);
      inner_path : string option (**Module path towards the interesting module, e.g. "List"*)
    }

type comment = string option

type substitution = string * string

type dist_tree =
  | Leaf of sigsource * comment (**A module alias*)
  | Node of tree list * comment

(** Read and parse the contents of a .dist file and return it without any additional processing*)
let read_dist : string -> dist_tree * substitution list = fun _ -> assert false

(** Return the list of .mli corresponding to %mli directives.*)
let sources_of : dist_tree -> string list = fun _ -> assert false


let mli_of_dist =
  rule ".dist to .mli"
    ~prod:"%.mli"
    ~dep:"%.dist"
    begin
      fun env build ->
	let dest = env "%.mli"
	and src  = env "%.dist" in
	let (tree, substitutions) = read_dist src in
	let sources = sources_of tree             in
	  (*For each module name, first generate the .mli file if it doesn't exist yet.*)
	  List.iter ignore_good (build sources);
	  
    end

(**To obtain the .ml of a .dist, just copy the .dist to .ml*)
let ml_of_dist =
  rule ".dist to .ml"
    ~prod:"%.ml"
    ~dep:"%.dist"
    begin
      fun env build ->
	let dest = env "%.ml"
	and src  = env "%.dist"
	in
	  Cmd (S[A"cp"; A src; A dest])
    end
