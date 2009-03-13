(*
  Build a .mli from a .dist file.

  Files with extension .dist are used to assemble modules into a
  consistent hierarchy. Essentially, .dist files are .ml files
  with additional annotations designed to allow generation of
  the corresponding .mli.


  Format of a .dist file:
  
  {[

   (*Module comment*)
   module Foo = A.Module.Path     (*%mli "a/file/path/foo.mli" aka "InnerFoo"*)
  
   (*Module comment*)
    module Bar = Another.Module.Path.Foo (*%mli "another/file/path/foo.mli" submodule "Bar"*)
  
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


open Preprocess_common
open Genlex
open Camlp4.PreCast
open Camlp4.Sig

module StringSet = Set.Make(String)

(** {6 Finding files}*)

(**The list of include directories.
   Specified on the command-line*)
let include_dirs : string list ref = ref []




(** {6 Calling ocamldep} *)

open Ocamlbuild_pack

(** Invoke ocamldep and compute the dependencies of a .mli*)
let stringset_of_ocamldep : string -> StringSet.t = fun file -> 
  List.fold_left (fun acc (_, x) -> StringSet.add x acc) StringSet.empty (Ocaml_utils.path_dependencies_of file)

(** {6 Utilities}*)

(** Imported from {!IO.Printf} to avoid unsolvable dependencies*)
module Printf =
struct
  include Printf
    
  let make_list_printer (p:(out_channel -> 'b -> unit)) 
      (start:   string)
      (finish:  string)
      (separate:string)
      (out:out_channel)
      (l:  'b list  ) = 
    let rec aux out l = match l with
      | []    -> ()
      | [h]   -> p out h
      | h::t  -> fprintf out "%a%s%a" p h separate aux t
    in fprintf out "%s%a%s" start aux l finish
end
open Printf

(** {6 Dependency sorting}*)

module Dependency =
struct
  type t = (string, StringSet.t) Hashtbl.t
      
  let create () = Hashtbl.create 100
  let add tbl k dep =
    try Hashtbl.replace tbl k (StringSet.add dep (Hashtbl.find tbl k))
    with Not_found -> Hashtbl.add tbl k (StringSet.singleton dep)
      
  let remove tbl (k:string) dep =
    try let set = StringSet.remove dep (Hashtbl.find tbl k) in
      if StringSet.is_empty set then Hashtbl.remove tbl k
      else Hashtbl.replace tbl k  set
    with Not_found -> ()
      
  let find tbl (k:string) =
    try  Some (Hashtbl.find tbl k)
    with Not_found -> None
      
  let find_all tbl (k:string) =
    try StringSet.elements (Hashtbl.find tbl k)
    with Not_found -> []
      
  let print out tbl = 
    Printf.fprintf out "{";
    Hashtbl.iter (fun k set -> Printf.fprintf out "%s: {%a}\n"
		    k
		    (Printf.make_list_printer (fun out -> Printf.fprintf out "%s") "{" "}" "; ")
		    (StringSet.elements set)) tbl;
    Printf.fprintf out "}\n"
end

module Depsort = 
struct
  type t = 
      {
	direct : Dependency.t (**Direct dependency*);
	reverse: Dependency.t (**Reverse dependency*);
	set    : StringSet.t ref (**All the nodes*)
      }

  let create () =
    {
      direct  = Dependency.create ();
      reverse = Dependency.create ();
      set     = ref StringSet.empty
    }

  let add_node t node =
    t.set := StringSet.add node !(t.set)

  let add_dependency t depending depended =
    Dependency.add t.direct  depending depended;
    Dependency.add t.reverse depended depending;
    add_node t depending;
    add_node t depended




  let sort t =
(*    Printf.eprintf "Sorting %a\n" Dependency.print t.direct;*)
    let rec aux (sorted:string list) (rest: string list) =
      match rest with 
	| [] -> 
	    sorted
	| _ ->
	    (*Find nodes which haven't been removed and depend on nothing*)
	    match List.fold_left (fun (keep, remove) k -> 
				    match Dependency.find t.direct k with
				      | None   -> 
					  (keep, k::remove)
				      | Some dependencies ->
					  (k::keep, remove)
				 ) ([],[]) rest
	    with
	      | (_, [])       -> 
		  Printf.eprintf "Cyclic dependencies in %a\n" Dependency.print t.direct;
		  failwith "Cyclic dependencies"
	      | (rest, roots) ->
		  List.iter (fun d ->
(*			       Printf.eprintf "Dependency %S resolved\n" d;*)
			       List.iter                (*Dependency [d] has been resolved, remove it.*)
				 (fun x -> Dependency.remove t.direct x d)
				 (Dependency.find_all t.reverse d)) roots;
		  aux (sorted @ roots) rest in
    aux [] (StringSet.elements !(t.set))
end

(** {6 String manipulation} *)

module String =
struct
  include String

  exception Invalid_string
    
  let find str ?(pos=0) ?(end_pos=length str) sub =
    let sublen = length sub in
      if sublen = 0 then
	0
      else
	let found = ref 0 in
	try
	  for i = pos to end_pos - sublen do
	    let j = ref 0 in
	    while unsafe_get str (i + !j) = unsafe_get sub !j do
	      incr j;
	      if !j = sublen then begin found := i; raise Exit; end;
	    done;
	  done;
	  raise Invalid_string
	with
	    Exit -> !found
		
  let split str sep =
    let p = find str sep in
    let len = length sep in
    let slen = length str in
      sub str 0 p, sub str (p + len) (slen - p - len)
	
  let nsplit str sep =
    if str = "" then []
    else (
      let rec nsplit str sep =
	try
	  let s1 , s2 = split str sep in
	    s1 :: nsplit s2 sep
	with
	    Invalid_string -> [str]
      in
	nsplit str sep
    )

  type segment = Changed of string | Slice of int * int 

  let global_replace convs str = (* convs = (seek, replace) list *)
    let repl_one slist (seek,repl) = 
      let rec split_multi acc = function 
	  Slice (start_idx, end_idx) ->
	    begin try
	      let i = find str ~pos:start_idx ~end_pos:end_idx seek in
	      split_multi 
		(* accumulate slice & replacement *)
		(Changed repl :: Slice (start_idx,i-1) :: acc) 
		(* split the rest of the slice *)
		(Slice (i+length seek, end_idx))
	    with
		Invalid_string -> Slice (start_idx,end_idx) :: acc
	    end
	| s -> s :: acc (* don't replace in a replacement *)
      in
      List.fold_left split_multi [] slist in
    let to_str pieces = 
      let len_p = function Changed s -> length s | Slice (a,b) -> b-a + 1 in
      let len = List.fold_left (fun a p -> a + len_p p) 0 pieces in
      let out = String.create len in
      let rec loop pos = function
	  Slice (s, e) :: t -> 
	    String.blit str s out pos (e-s+1);
	    loop (pos+e-s+1) t
	| Changed s :: t ->
	    String.blit s 0 out pos (length s);
	    loop (pos + length s) t
	| [] -> ()
      in
      loop 0 pieces;
      out
    in
    
    to_str (List.fold_left repl_one [Slice (0,length str)] convs)
      
end

(** {6 Representation of the .dist file}*)

type path = string list
(** The type of a module path*)

(**Information regarding where to find the signature for a module.*)
type sigsource = {
  mli        : string (**Path towards the .mli file containing the data for this module, e.g. "src/core/extlib/extList.mli"*);
  inner_path : path   (**Module path towards the interesting module, e.g. "List"*)
}

type comment = string list

type substitution = (string * string) (** [(original, replacement)] *)

type ('a,'b) sigtree = 'a * (('a, 'b) sigtree_aux)
and  ('a,'b) sigtree_aux =
  | Leaf of string * 'b                    * comment (**A module alias*)
  | Node of string * ('a, 'b) sigtree list * comment
  | Other of string                         (**Some uninterpreted content, such as unattached comments*);;



  

(** Return the annotations on a tree*)
let leaves_of (tree: (_, 'b) sigtree) : 'b list = 
  let rec aux acc n = match n with
    | (_, Other _)        -> acc
    | (_, Node (_, l, _)) -> List.fold_left aux acc l
    | (_, Leaf (_, x, _)) -> x :: acc
  in aux [] tree;;

(** [extract_relevant_of_string file submodule] returns a string containing the relevant parts of
    [file]. If [submodule] is [[]], the relevant parts of [file] are the complete contents
    of [source]. If [submodule] is a module path, the relevant parts of [file] are only the
    contents of the corresponding path. *)
let extract_relevant_of_file (filename: string) (path: path) (subs:substitution list) =
  let ic = open_in filename   in
  let buf= Buffer.create 1024 in
  let oc = Format.formatter_of_buffer buf in
    extract filename ic oc path;
    let contents = Buffer.contents buf in
      String.global_replace subs contents;;

  

let parse_annotation stream =
  let parse_annotation_content stream = 
    let rec aux ?mli ~aka ?path = parser
      | [< 'Kwd "aka"; 'String s; stream >]       -> aux ?mli ~aka:(s::aka) ?path stream
      | [< 'Kwd "mli"; 'String mli; stream >]     -> aux ~mli ~aka ?path stream
      | [< 'Kwd "submodule"; 'String s; stream >] -> aux ?mli ~aka ~path:s stream
      | [< >] -> (mli, aka, path)
    in
      aux ~aka:[] (make_lexer ["aka"; "mli"; "submodule"] stream)
  in
  let rec aux stream = match Stream.next stream with
    | ((BLANKS _ | NEWLINE), _) -> aux stream
    | (COMMENT c, _)            -> 
	if String.length c >= 1 && String.get c 0 = '%' then Some (parse_annotation_content (Stream.of_string c))
	else None
    | _                         -> None
  in aux stream


(** Read and parse the contents of a .dist file and return it without any additional processing*)
let read_dist: in_channel -> string -> (unit, sigsource) sigtree * substitution list = fun channel name ->
  let renamings = ref [] in
  let rec aux ~recent_comments (*~old_comments*) ~path stream : (_, _) sigtree list = 
    match Stream.next stream with
      | (COMMENT c, _)          -> aux ~recent_comments:(c::recent_comments) ~path (*~old_comments*) stream
      | ((BLANKS _ | NEWLINE), _) -> aux ~recent_comments:[] ~path (*~old_comments:(recent_comments @ old_comments)*) stream
      | (KEYWORD "module", _) ->
	  begin
            skip_blanks stream;
            let id = parse_uident stream in
              skip_blanks stream;
              parse_equal stream;
              skip_blanks stream;
	      match Stream.peek stream with
		| Some(KEYWORD "struct", _) ->
		    njunk 1 stream;
(*		    List.rev (List.map (fun x -> Other x) old_comments) @*)
		    [((),Node (id, aux ~recent_comments:[] stream ~path:(path^id^"."), List.rev recent_comments))]
		| _ ->
		    begin
		      let source_path = parse_path stream       in
			renamings := 
			  (string_of_path source_path, id) :: 
			  (path, id) :: 
			    !renamings;
			match parse_annotation stream with
			  | Some (Some mli, aka, Some path) ->
			      List.iter (fun x -> renamings := (x, id)::!renamings) aka;
			      let annot = 
				{
				  mli = mli;
				  inner_path = path_of_string path
				}
			      in
			      ((), Leaf (id, annot, List.rev recent_comments)) ::
				aux ~recent_comments:[] ~path stream
			  | None -> failwith "Missing annotation"
			  | _ -> failwith "Incomplete annotation"
		    end
          end
      | (EOI, _)   -> []
      | (tok, loc) -> []
  in (((), Node ("", aux ~recent_comments:[] ~path:"" (tokens_of_channel name channel), [])), !renamings)
  

(** Go through a tree applying substitutions. 

    For each leaf of the tree
    - read the [source]
    - extract the relevant part
    - apply the substitutions to the relevant part
    - write the substituted version to a temporary file
    - replace the leaf content with the temporary file name*)
let apply_substitutions: ('a, sigsource) sigtree -> substitution list -> ('a, string) sigtree = fun tree substitutions ->
  let rec aux = function
    | (tag, Leaf (name, {mli = mli; inner_path = inner_path}, comment)) ->
	let contents = extract_relevant_of_file mli inner_path substitutions in
	let filename = Filename.temp_file "ocamlbuild_distrib" ".mli" in
	let cout     = open_out filename                              in
	  output_string cout contents;
	  (tag, Leaf (name, filename, comment))
    | (tag, Node (name, tree, comment)) -> (tag, Node (name, List.map aux tree, comment))
    | (tag, Other o) -> (tag, Other o)
  in aux tree



(** Compute dependencies of each node of the tree.

    For each leaf of the tree
    - apply ocamldep
    - parse the result into a list of dependencies

    For each node, merge the dependencies of subtrees.
*)
let compute_dependencies: (unit, string) sigtree -> (StringSet.t, string) sigtree = fun tree -> 
  let rec aux = function
    | ((), Other o)                  -> (StringSet.empty, Other o)
    | ((), Node (name, children, comment)) -> 
	let (deps, rewritten) =
	  List.fold_left (fun (deps, rewritten) child  -> let ((child_deps, _) as child') = aux child in
			    (StringSet.union deps child_deps, child'::rewritten))
	    (StringSet.empty, []) children
	in
	  (deps, Node (name, rewritten, comment))
    | ((), Leaf (name, file_name, comment))->
	(stringset_of_ocamldep file_name, Leaf (name, file_name, comment))
  in aux tree

(**
   Sort a list of modules topologically.

   [sort_modules l rename] sorts the modules of list [l]. Each name is transformed using [rename]
   before taking dependencies into account ([rename] serves chiefly to add prefixes).
*)
let sort_modules: ((StringSet.t, _) sigtree list as 'a) -> (string -> string) -> 'a = fun list prefix -> 
  let dependencies = Depsort.create ()
  and modules      = Hashtbl.create 16
  and others       = ref []            in
    List.iter (function ((depends_on, Leaf (name, _, _)) as node)
		 |      ((depends_on, Node (name, _, _)) as node)->
			  let name' = prefix name in (*Collect dependencies*)
			    Hashtbl.add modules name node;
			    Depsort.add_node dependencies name;
			    StringSet.iter (fun dep -> Depsort.add_dependency dependencies name' dep) depends_on
		 | other -> others := other :: !others) list;
    List.rev_append !others (List.map (fun name -> Hashtbl.find modules name) (Depsort.sort dependencies))
			    


(**Recursively sort by dependencies each level of the tree.
*)
let sort_tree : (StringSet.t, string) sigtree -> (StringSet.t, string) sigtree = fun tree ->
  let rec aux prefix = function
    | (_,   Other _) as o -> o
    | (set, Node (name, children, comment)) ->
	(*First sort each child*)
	let prefix'  = prefix ^ name ^ "."           in
	let children = List.map (aux prefix') children in
	let mkprefix = fun s -> prefix' ^ s          in
	(*Then sort between children*)
	(set, Node (name, sort_modules children mkprefix, comment))
    | (_,   Leaf _) as l -> l
  in aux "" tree

(**Write down the tree
*)
let serialize_tree : Format.formatter -> (_, string) sigtree -> unit = fun out ->
  let serialize_comment out l =
    List.iter (Format.fprintf out "%s@\n") l
  in
  let rec aux = function
    | (_, Leaf (name, content, comment)) ->
	Format.fprintf out "%a@\nmodule %s : sig@[<v 5>%s@]@\n" serialize_comment comment name content
    | (_, Node (name, children, comment)) ->
	Format.fprintf out "%a@\nmodule %s = struct@[%a@]@\n" serialize_comment comment name 
	  (fun _ l -> List.iter aux l) children
    | (_, Other s) -> Format.fprintf out "%s@\n" s
  in aux

(** Drive the process*)
let driver name cin cout = 
  let (tree,substitutions) = read_dist cin name                                in
  let deps = compute_dependencies (apply_substitutions tree substitutions)     in
    serialize_tree cout (sort_tree deps)


let _ =
  let out_file = ref ""
  and in_file  = ref "" in
  Arg.parse
    [("-I", Arg.String (fun x -> include_dirs := x :: !include_dirs), "Add include directory");
     ("-o", Arg.Set_string out_file,                                  "Set output .mli (standard out by default)")]
    (fun file -> out_file :=  file)
    "Generate a .mli file from a .dist";
    let cout = match !out_file with
      | "" -> Format.std_formatter
      | s  -> Format.formatter_of_out_channel (open_out s)
    and cin  = match !in_file with
      | "" -> stdin
      | s  -> open_in s
    in
      driver !in_file cin cout;
      flush_all ()


