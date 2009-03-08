(**

   Build a .ml and a .mli from a .dist file.


Format of a .dist file:

{[

(*Module comment*)
module Foo = A.Module.Path     (*% mli "a/file/path/foo.mli" aka "InnerFoo" %*)

(*Module comment*)
module Bar = Another.Module.Path.Foo (*% mli "another/file/path/foo.mli" submodule "Bar" %*)

(*Module comment*)
module Sna = struct
   module Toto = Yet.Another.Module.Path (*% mli "a/file/path/foo.mli" %*)

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
     ---- extracting only the necessary submodules (remove [module Stuff : sig] and [end (*Stuff*)])
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

type ('a,'b) sigtree = ('a, 'b) sigtree_aux * 'a
and  ('a,'b) sigtree_aux =
  | Leaf of 'b                    * comment (**A module alias*)
  | Node of ('a, 'b) sigtree list * comment
  | Other of string                         (**Some uninterpreted content, such as unattached comments*)


(** Read and parse the contents of a .dist file and return it without any additional processing*)
let read_dist: string -> (unit, sigsource) sigtree * substitution list = fun _ -> assert false

(** Return the list of .mli corresponding to %mli directives.*)
let leaves_of: (_, 'b) sigtree -> 'b list = fun _ -> assert false

(** [patch source subs] creates a temporary file with the contents of [source]
    in which all the substitutions of [subs] have been applied and returns the
    absolute filename of the temporary file *)
let patch: sigsource -> substitution list -> string = fun source -> assert false

(** Go through a tree applying substitutions. 

    For each leaf of the tree
    - read the [source]
    - extract the relevant part
    - apply the substitutions to the relevant part
    - write the substituted version to a temporary file
    - replace the leaf content with the temporary file name*)
let apply_substitutions: ('a, sigsource) sigtree -> substitution list -> ('a, string) sigtree = fun _ _ -> assert false

(** Compute dependencies of each node of the tree.

    For each leaf of the tree
    - apply ocamldep
    - parse the result into a list of dependencies

    For each node, merge the dependencies of subtrees.
*)
let compute_dependencies: (unit, string) sigtree -> (StringSet.t, string) sigtree = fun _ -> assert false

(**Recursively sort by dependencies each level of the tree.
*)
let sort_tree : (StringSet.t, string) sigtree -> (StringSet.t, string) sigtree = fun _ -> assert false

(**Write down tree

   We need to turn it into a list of strings as this seems to be the only way of creating new
   files from within ocamlbuild.
*)
let serialize_tree : (_, string) sigtree -> string list = fun _ -> assert false

let mli_of_dist =
  rule ".dist to .mli"
    ~prod:"%.mli"
    ~dep:"%.dist"
    begin
      fun env build ->
	let dest = env "%.mli"
	and src  = env "%.dist" in
	let (tree, substitutions) = read_dist src in
	  (*For each source .mli, first generate the .mli file if it doesn't exist yet.*)
	  List.iter ignore_good (build (List.map (fun x -> x.path) (leaves_of tree)));
	  (*Now that we have all the source .mli, we can apply substitutions and compute dependencies*)
	  let deps = compute_dependencies (apply_substitutions tree substitutions)     in
	    Echo (write_tree (sort_tree deps)) dest
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
