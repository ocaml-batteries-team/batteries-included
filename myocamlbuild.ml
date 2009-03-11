(*
  This file is Public Domain
*)

open Ocamlbuild_plugin
open Command (* no longer needed for OCaml >= 3.10.2 *)
open Ocamlbuild_pack
open Ocaml_utils
open My_std.Outcome

(** {6 Utilities}*)

  (** Imported from {!List} to avoid unsolvable dependencies*)
  module List =
  struct
    include List
    let filter_map f l =
      let rec loop acc = function
	| []   -> acc
	| h::t -> match f h with
	    | None   -> loop acc t
	    | Some x -> loop (x::acc) t
      in rev (loop [] l)
  end
    
    
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
    

(** Imported from {!ExtString.String} to avoid unsolvable dependencies during compilation*)
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

module StringSet = Set.Make(String)
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
		  assert false
	      | (rest, roots) ->
		  List.iter (fun d ->
(*			       Printf.eprintf "Dependency %S resolved\n" d;*)
			       List.iter                (*Dependency [d] has been resolved, remove it.*)
				 (fun x -> Dependency.remove t.direct x d)
				 (Dependency.find_all t.reverse d)) roots;
		  aux (sorted @ roots) rest in
    aux [] (StringSet.elements !(t.set))
end

(**
   {1 OCamlFind}
*)

module OCamlFind =
struct
  (* these functions are not really officially exported *)
  let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read
    
  let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings
  
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

  let after_rules () =
       (* When one link an OCaml library/binary/package, one should use -linkpkg *)
       flag ["ocaml"; "byte"; "program"] & A"-linkpkg";

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
       flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"]    (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "doc"]     (S[A "-I"; A "+threads"])
end

(**
   {1 Documentation generation}
*)
module Documentation =
struct


  let before_options () =
    (*Options.ocamldoc  := A"ocamldoc"*) ()

  let after_rules () = 
    dep  ["ocaml"; "doc"]   & ["build/odoc_batteries_factored.cmo"; "build/odoc_tags.cmo"; "build/odoc_extract_mli.cmo"];
    flag ["ocaml"; "doc"]   & S[A "-i"; A "_build/build"; 
				A "-i"; A "build";
				A "-g"; A "odoc_batteries_factored.cmo";
				A "-g"; A "odoc_tags.cmo"; 
				(*A "-g"; A "build/odoc_extract_mli.cmo";*)
			        A "-t"; A "OCaml Batteries Included" ;
				A "-intro"; A "../build/intro.text"]
end



(** Create a .mli for each .mlpack which doesn't already have one. *)
module Packs =
struct

  let _PRODUCE_MLI_FROM_PACK = true  (*current workaround for .mlpack + ocamldoc issues*)
  let _PRODUCE_PACKED_ML     = false (*not ready for prime-time*)


    
  let read_dependency dep_name extension =
    let module_name = String.capitalize (Filename.basename (Filename.chop_suffix dep_name extension)) in
    let f = open_in dep_name in
    let (file_name, dependencies) = String.split (input_line f) ":" in
(*      Printf.eprintf "Reading dependency %s => %s\n" file_name dependencies;*)
    let result = (file_name, module_name, List.filter (fun x -> not (String.length x = 0)) (String.nsplit dependencies " ")) in
      close_in f;
      result
	
	

    
  (** Read the dependencies from a directory and sort them
      
      If [l] is a list of files created by [ocamldep], [sort l] reads
      all the files in [l] and produces a sorted list of pairs [(name,
      mli)], where [name] is the name of the module and [mli] is the
      name of the corresponding [mli] file. This list is sorted topologically
      so as to make sure that whenever a module [M] depends on a module [N],
      [N] appears before [M]. 

      As usual, cyclic dependencies are bad.*)
  let sort files extension =
    let depsort = Depsort.create ()
    and src : (string, string) Hashtbl.t = Hashtbl.create 100 in
      (*Read all the dependencies and store them in the tables*)
      List.iter (
	fun f ->
(*	  Printf.eprintf "Adding file %S\n" f;*)
	  if Filename.check_suffix f ".depends" then
	    let (file_name, module_name, dependencies) = read_dependency f extension in
	      Depsort.add_node depsort module_name;
	      List.iter (fun x -> Depsort.add_dependency depsort module_name x) dependencies;
	      Hashtbl.replace src module_name file_name
	  else ()
      ) files ;
      let sorted = Depsort.sort depsort in
	List.filter_map (fun module_name -> 
			   try Some (module_name, Hashtbl.find src module_name)
			   with Not_found ->  None) sorted;;


  (**
     Generate a .mli from a list of modules.
     To do so, read the contents of each module signature
     and produce a .mli along the lines of
     [
     (**First comments of module Foo*)
     module Foo :
     sig
        (*contents of foo.mli minus the first comments*)
     end
     ]

     As an exception, does not extract anything for modules whose name
     starts with Inner.
  *)
  let generate_mli buf l = 
    (*Extract the first comments, up-to the first ocamldoc not followed by a newline.*)
    let parse_header channel = 
      let past    = Buffer.create 1024        in
      let store c = Buffer.add_char past c    in
      let newline = ref false                 in
      let return stream =	
	let rest = Buffer.create 1024         in
	  Stream.iter (fun x -> Buffer.add_char rest x) stream;
	  if !newline then (*There are two newlines after the comment, it's the module comment*)
	    (Buffer.contents past, Buffer.contents rest)
	  else             (*This comment is actually for the first element of the module*)
	    begin
	      Buffer.add_buffer past rest;
	      ("", Buffer.contents past)
	    end
      in
      let rec next_token (strm__ : _ Stream.t) = match Stream.peek strm__ with
	  | Some ('(' as c) -> Stream.junk strm__; store c; maybe_comment strm__
	  | Some (' ' | '\009' | '\026' | '\012' as c) ->
              Stream.junk strm__; store c; next_token strm__
	  | Some ('\n'| '\r' as c) ->
              Stream.junk strm__; store c; next_token strm__
	  | _ -> return strm__
      and maybe_comment (strm__ : _ Stream.t) =
	match Stream.peek strm__ with
	  | Some ('*' as c) -> Stream.junk strm__; store c; 
	      let s = strm__ in maybe_ocamldoc s
	  | _             -> next_token strm__
      and maybe_ocamldoc (strm__ : _ Stream.t) =
	match Stream.peek strm__ with
	  | Some ('*' as c) -> Stream.junk strm__; store c; 
	      let s = strm__ in probably_ocamldoc s
	  | _             -> comment strm__; next_token strm__
      and comment (strm__ : _ Stream.t) =
	match Stream.peek strm__ with
	    Some ('(' as c) -> Stream.junk strm__; store c; maybe_nested_comment strm__
	  | Some ('*' as c) -> Stream.junk strm__; store c; maybe_end_comment strm__
	  | Some c -> Stream.junk strm__; store c; comment strm__
	  | _ -> raise Stream.Failure
      and maybe_nested_comment (strm__ : _ Stream.t) =
	match Stream.peek strm__ with
	    Some ('*' as c) -> Stream.junk strm__; store c; let s = strm__ in ignore (comment s); comment s
	  | Some c -> Stream.junk strm__; store c; comment strm__
	  | _ -> raise Stream.Failure
      and maybe_end_comment (strm__ : _ Stream.t) =
	match Stream.peek strm__ with
	    Some (')' as c) -> Stream.junk strm__; store c; ()
	  | Some ('*' as c) -> Stream.junk strm__; store c; maybe_end_comment strm__
	  | Some c   -> Stream.junk strm__; store c; comment strm__
	  | _ -> raise Stream.Failure
      and probably_ocamldoc (strm__ : _ Stream.t) = 
	match Stream.peek strm__ with
	  | Some '*' -> comment strm__; next_token strm__ (*Actually three ***, so it's a regular comment*)
	  | _        -> ocamldoc strm__
      and ocamldoc (strm__ : _ Stream.t) = 
	comment strm__;
	after_ocamldoc 0 strm__
      and after_ocamldoc n (strm__ : _ Stream.t) = 
	match Stream.peek strm__ with
	  | Some ('\r' | '\n') when n >= 1 ->
	      newline := true;
	      return strm__
	  | Some ('\r' | '\n') ->  
	      Stream.junk strm__;
	      after_ocamldoc (n + 1) strm__
	  | Some ('\009' | '\026' | '\012') ->
              Stream.junk strm__;
	      after_ocamldoc n strm__
	  | _ -> return strm__
      in
	next_token (Stream.of_channel channel)
    in
  let print_modules buf () =
    List.iter (
      fun (name, src) ->
(*	Printf.eprintf "Extracting module %S name from %S\n" name src;
	if try String.find name "Inner" <> 0 (*Don't extract from files whose name starts with Inner*)
	   with String.Invalid_string -> true
	then*)
	  let name = try
	    let index = String.find name ".inferred" in
	      String.sub name 0 index
	  with String.Invalid_string -> name in
	  let (prefix, contents) = with_input_file src parse_header in
	    Printf.bprintf buf "%s\nmodule %s:(*from %S*)\nsig\n%s\nend\n" prefix name src contents
    ) l
  in print_modules buf ()



  let read_pack pack = string_list_of_file pack


  (**{6 OCamlbuild options}*)
	
  let before_options () = ()


  let after_rules    () =
(*    rule ".mlpacklit to .mlpack conversion rule"
      ~prod:"%.mlpack"
      ~dep: "%.mlpacklit"
      begin fun env build ->
	let packlit = env "%.mlpacklit"
	and dest    = env "%.mlpack" in
	  (*Read the list of modules from [packlit], drop comments*)
	let modules = 
	  List.filter_map (function Code c -> Some c
			|      _      -> None )
	  (with_input_file packlit (fun input -> next_token (Stream.of_channel input)))
	in Echo(modules, dest)

      end;*)

    if _PRODUCE_PACKED_ML then
      begin

	(*Convert a .mlpack to .packedml
	  If foo.mlpack consists in list [A], [B], [C], [D]... then
	  foo.packedml consists in
	  [module Foo = struct
	     module A = A
	     module B = B
	     module C = C
             module D = D
             ...
	  end]

	  Note that we need to define [module Foo] inside [foo.packedml] as
	  OCamlDoc doesn't automatically assume that [foo.packedml] contains
	  a module [Foo].
	*)
    rule ".mlpack to .packedml"
      ~prod:"%.packedml"
      ~deps:["%.mlpack"; "%.cmo"]
      begin fun env build ->
	let pack = env "%.mlpack"
	and dest = env "%.packedml"
(*	and name = String.capitalize (Filename.basename (env "%"))*) in
	let modules = read_pack pack in
(*	  Echo((Printf.sprintf "module %s = struct\n(**/**)\n" name)                        ::
		 List.map (fun m -> Printf.sprintf "module %s = struct include %s end\n" m m) modules @
		 ["(**/**)\nend"],
	       dest)*)
	  Echo( List.map (fun m -> Printf.sprintf "module %s = struct include %s end\n" m m) modules,
		  dest)
      end;
    
    rule ".packed.ml to .odoc"
      ~dep:"%.packedml"
      ~prod:"%.odoc"
      begin fun env build ->
	let pack         = env "%.mlpack"
	and mlpacked     = env "%.packedml" 
	and odoc         = env "%.odoc"      in
	let modules      = read_pack pack    in
	let include_dirs = Pathname.include_dirs_of (Pathname.dirname pack) in
	  (*Ocaml_compiler.prepare_compile build mlpacked;*)
	  let deps       = List.map Outcome.good (build (List.map(fun m -> 
								    expand_module include_dirs m ["odoc"]) modules)) 
	  and tags       = (tags_of_pathname mlpacked++"implem") 
	  and arg        = mlpacked in
	    Cmd (S [!Options.ocamldoc;
		    S(List.map (fun a -> S[A"-load"; P a]) deps);
		    ocaml_ppflags (tags++"pp:doc"); 
		    Tools.flags_of_pathname arg;
		    ocaml_include_flags arg; 
		    A"-dump"; 
		    Px odoc; 
		    T(tags++"doc");
		    A "-impl";
		    P mlpacked])
      end
      end;

    if _PRODUCE_MLI_FROM_PACK then
    rule ".mlpack to .mli conversion rule"
      ~prod:"%.mli"
      ~dep:"%.mlpack"
      begin fun env build ->
        (*c The action is a function that receive two arguments:
          [env] is a conversion function that substitutes `\%' occurrences
          according to the targets to which the rule applies.
          [_build] can be called to build new things (dynamic dependencies). *)
	
	let pack         = env "%.mlpack" 
	and dest         = env "%.mli" in
	let include_dirs = Pathname.include_dirs_of (Pathname.dirname pack) in
	  
	(*Read the list of module dependencies from [pack]*)
	let modules      = 
	  with_input_file pack (
	    fun input ->
	      let modules = ref [] in
		try
		  while true do
		    let m = input_line input in
		      (*Printf.eprintf "Reading %S\n" m;*)
		      modules := m::!modules
		  done; assert false
		with End_of_file -> !modules			      
	  ) in

	(*For each module name, first generate the .mli file if it doesn't exist yet.*)
	  List.iter ignore_good (build (List.map(fun m -> expand_module include_dirs m ["mli";
										       "inferred.mli"]) modules));

	(*Deduce file names from these module names and wait for these dependencies to be solved.
	  
	  [build] has a mysterious behaviour which looks like cooperative threading without
	  threads and without call/cc.*)
	let results = build (List.map(fun m -> expand_module include_dirs m ["mli.depends"; 
									     "inferred.mli.depends"]) 
			       modules) in
	  
	(*Now that we have generated the mli files and, more importantly, the corresponding
	  mli.depends, we can read the contents of these dependencies,
	  sort them and generate the mli (see the other functions of this module).*)
	let mli_depends =  
	  List.map ( 
	    function Good s  -> s
	      |      Bad exn -> raise exn (*Stop here in case of problem and propagate the exception*)
	  ) results in
	let buf = Buffer.create 2048 in
	  generate_mli buf (sort mli_depends ".mli.depends");
	  Echo([Buffer.contents buf], dest)
      end

(* Simple utility to sort a list of modules by dependencies. *)

    let generate_sorted buf l = 
      Printf.eprintf "generating\n";
      List.iter (
	fun (name, src) -> 
(*	  Printf.eprintf "Adding module %S\n" name;*)
	  Printf.bprintf buf "%s\n" name ) l;;

      rule ".mlpack to .sorted conversion rule"
	    ~prod:"%.sorted"
	    ~dep:"%.mlpack"
      begin fun env build ->
        (*c The action is a function that receive two arguments:
          [env] is a conversion function that substitutes `\%' occurrences
          according to the targets to which the rule applies.
          [_build] can be called to build new things (dynamic dependencies). *)
	
	let pack         = env "%.mlpack" 
	and dest         = env "%.sorted" in
	let include_dirs = Pathname.include_dirs_of (Pathname.dirname pack) in
	  
	(*Read the list of module dependencies from [pack]*)
	let modules      = 
	  with_input_file pack (
	    fun input ->
	      let modules = ref [] in
		try
		  while true do
		    let m = input_line input in
		      (*Printf.eprintf "Reading %S\n" m;*)
		      modules := m::!modules
		  done; assert false
		with End_of_file -> !modules			      
	  ) in

	(*For each module name, first generate the .ml file if it doesn't exist yet.*)
	  List.iter ignore_good (build (List.map(fun m -> 
						   Printf.eprintf "Expanding ml module\n";
expand_module include_dirs m ["ml"]) modules));

	(*Deduce file names from these module names and wait for these dependencies to be solved.
	  
	  [build] has a mysterious behaviour which looks like cooperative threading without
	  threads and without call/cc.*)
	let results = build (List.map(fun m -> 
					Printf.eprintf "Expanding depends module\n";
					expand_module include_dirs m ["ml.depends"]) 
			       modules) in
	  
	(*Now that we have generated the ml files and, more importantly, the corresponding
	  ml.depends, sort the dependencies. *)
	let ml_depends =  
	  List.map ( 
	    function Good s  -> s
	      |      Bad exn -> raise exn (*Stop here in case of problem and propagate the exception*)
	  ) results in
	let buf = Buffer.create 2048 in
	  generate_sorted buf (sort ml_depends ".ml.depends");
	  Echo([Buffer.contents buf], dest)
      end

    (**{6 Generation of batteries.mli}

       A .mlhierarchy is a file containing the following constructions:
       - [module Foo = Bar]
       - [module Foo = struct (*recursively*) end]
       - comments

       From a .mlhierarchy, we can build a .ml, which is identical.
       From a .mlhierarchy, we can also build a .mli, by 
    *)
(*
    let not_used_yet () =
      rule ".mlhierarchy to .ml" 
	~prod:"%.ml"
	~dep:"%.mlhierarchy" 
	begin fun env build ->
	  (*Just copy .mlhierarchy -> .ml*)
	  assert false
	end;

      rule ".mlhierarchy to .mli"
	~prod:"%.mli"
	~dep:"%.mlhierarchy"
	begin fun env build ->
	  (*Parse .mlhierarchy*)
	end;
*)	

end

module Distrib = struct



    (**

   Build a .ml and a .mli from a .dist file.


Format of a .dist file:

{[

(*Module comment*)
module Foo = A.Module.Path     (*%Foo mli "a/file/path/foo.mli" aka "InnerFoo" %*)

(*Module comment*)
module Bar = Another.Module.Path.Foo (*%Bar mli "another/file/path/foo.mli" submodule "Bar" %*)

(*Module comment*)
module Sna = struct
   module Toto = Yet.Another.Module.Path (*%Sna.Toto mli "a/file/path/foo.mli" %*)

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
      inner_path : string list (**Module path towards the interesting module, e.g. "List"*)
    }

type comment = string option

type substitution = string * string

type ('a,'b) sigtree = 'a * (('a, 'b) sigtree_aux)
and  ('a,'b) sigtree_aux =
  | Leaf of string * 'b                    * comment (**A module alias*)
  | Node of string * ('a, 'b) sigtree list * comment
  | Other of string                         (**Some uninterpreted content, such as unattached comments*)


(** Read and parse the contents of a .dist file and return it without any additional processing*)
let read_dist: string -> (unit, sigsource) sigtree * substitution list = fun _ -> assert false

(** Return the annotations on a tree*)
let leaves_of: (_, 'b) sigtree -> 'b list = fun tree ->
  let rec aux acc = function
    | (_, Other _)        -> acc
    | (_, Node (_, l, _)) -> List.fold_left aux acc l
    | (_, Leaf (_, x, _)) -> x :: acc
  in aux [] tree

(** Apply a number of substitutions on a string*)
let replace_in_string: string -> substitution list -> string = fun _ _ -> assert false

(** [patch source subs] creates a temporary file with the contents of [source]
    in which all the substitutions of [subs] have been applied and returns the
    absolute filename of the temporary file *)
let patch: sigsource -> substitution list -> string = fun source -> assert false

(** [extract_relevant_of_string source submodule] returns a string containing the relevant parts of
    [source]. If [submodule] is [None], the relevant parts of [file] are the complete contents
    of [source]. If [submodule] is a module path, the relevant parts of [file] are only the
    contents of the corresponding path. *)
let extract_relevant_of_string: string -> string list -> string = fun source path -> assert false

(** [extract_relevant_of_file file submodule] behaves as [extract_relevant_of_string] but operates
   on a file name
*)
let extract_relevant_of_file: string -> string list -> string = fun file path -> assert false


  

(** Go through a tree applying substitutions. 

    For each leaf of the tree
    - read the [source]
    - extract the relevant part
    - apply the substitutions to the relevant part
    - write the substituted version to a temporary file
    - replace the leaf content with the temporary file name*)
let apply_substitutions: ('a, sigsource) sigtree -> substitution list -> ('a, string) sigtree = fun tree substitutions ->
  let rec aux = function
    | (tag, Leaf (name, {path = path; inner_path = inner_path}, comment)) ->
	let contents = extract_relevant_of_file path inner_path in
	let contents'= replace_in_string contents substitutions  in
	let filename = Filename.temp_file "ocamlbuild_distrib" ".mli" in
	let cout     = open_out filename                              in
	  output_string cout contents';
	  (tag, Leaf (name, filename, comment))
    | (tag, Node (name, tree, comment)) -> (tag, Node (name, List.map aux tree, comment))
    | (tag, Other o) -> (tag, Other o)
  in aux tree

(** Invoke ocamldep and compute the dependencies of a .mli*)
let stringset_of_ocamldep : string -> StringSet.t = fun file ->
  List.fold_left (fun acc (_, x) -> StringSet.add x acc) StringSet.empty (Ocaml_utils.path_dependencies_of file)

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

(**Write down tree

   We need to turn it into a list of strings as this seems to be the only way of creating new
   files from within ocamlbuild.
*)
let serialize_tree : (_, string) sigtree -> string list = fun _ -> assert false

  let after_rules () =
  rule ".dist to .mli"
    ~prod:"%.mli"
    ~dep:"%.dist"
    begin
      fun env build ->
	let dest = env "%.mli"
	and src  = env "%.dist" in
	let (tree, substitutions) = read_dist src in
	  (*For each source .mli, first generate the .mli file if it doesn't exist yet.*)
	  List.iter ignore_good (build [List.map (fun x -> x.path) (leaves_of tree)]);
	  (*Now that we have all the source .mli, we can apply substitutions and compute dependencies*)
	  let deps = compute_dependencies (apply_substitutions tree substitutions)     in
	    Echo ((serialize_tree (sort_tree deps)), dest)
    end;
    (*To obtain the .ml of a .dist, just copy the .dist to .ml*)
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

end

(**
   {1 Others}
*)
module Misc =
struct
  let after_rules () =
    (*Documentation*)
    flag ["ocaml"; "link"; "byte";   "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"; A "odoc_info.cma"]);
    flag ["ocaml"; "link"; "native"; "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"(*; A "odoc_info.cmxa"*)]);
    flag ["ocaml"; "docfile";        "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);
    flag ["ocaml"; "docdir";         "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);
    flag ["ocaml"; "doc";            "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);

    flag ["ocaml"; "link";    "use_sexplib"]  & S[A"-package"; A "sexplib"];
    flag ["ocaml"; "compile"; "use_sexplib"]  & S[A"-package"; A "sexplib"];
    flag ["ocaml"; "dep";     "use_sexplib"]  & S[A"-package"; A "sexplib"];
(*
    flag ["ocaml"; "doc";     "use_sexplib"]  & S[A"-I"; A"/home/yoric/usr/local/godi/lib/ocaml/pkg-lib/sexplib"]*)
(*    flag ["ocaml"; "doc";     "use_sexplib"]  & S[A"-predicates"; A "!preprocessor"; A "-package"; A "sexplib"]*)

    flag ["ocaml"; "link"; "native"; "use_batteries_threads"] (A "src/main/threads/batteries.cma");
(*    flag ["ocaml"; "pp";       "use_pa_optcomp"] (A"build/optcomp/pa_optcomp.cmo");*)
    dep  ["ocaml"; "ocamldep"; "use_pa_optcomp"] ["src/main/threads/batteries.cma"]


end

(** Provide a library which is as complete as possible.

    Not working yet.*)
(*module Complete_mllib =
struct
  let after_rules () =
    rule "%.ml to %.cma in presence of tag autolib"
      ~deps:["%.ml"; "%.ml.depends"]
      ~tags:["autolib"]
      ~prod:"%.cmz"
      begin fun env build ->
	let src          = env "%.ml" 
	and dep          = env "%.ml.depends"
	and dest         = env "%.mli" 
	and cmo          = env "%.cmo" in
	  (*(*Resource.print Format.std_formatter (Resource.Cache.dependencies src);*)
	  (*Printf.eprintf "%S" (Solver.solve_target cmo (Pathname.include_dirs_of (Pathname.dirname cmo)));*)
	  Printf.eprintf "%S" 
	  Nop*)
	  (*First, compile the .ml into a .cmo, to check it's possible.*)
	(*let cmo          =
	  match [build expand_module (Pathname.include_dirs_of src) src ["cmo"]] with
	    | [Good g] -> g
	    | [Bad  e] -> raise e
	    | _        -> assert false
	in
	  (** [loop deps acc] 

	      1. Read each .depends from [deps]
	      2. Build the corresponding .cmo and add it to [acc]
	  *)
	let rec loop deps acc = 
	  let modules = List.concat (List.map (
	    fun dep ->
	      with_input_file dep (
		fun input ->
		  (Lexer.blank_sep_strings (Lexing.from_channel input), (Pathname.dirname dep))
	      )) deps) in
	  let potential_cmos = build (List.map fun (m,dir) -> expand_module (Pathname.include_dirs_of dir) m 
				      ["cmo"]) modules in
	  let actual_cmos    = List.filter_map 
	    (function Good path -> Some path
	       |      Bad  _    -> None) potential_cmos in
	    (*From this, deduce a new list of .depends*)
	    List.map (fun path -> Pathname.add_extension Pathname.remove_extension path ) actual_cmos
	in

	in
	  loop [(dep, Pathname.dirname src)] [cmo]
	(*Read the list of module dependencies from [pack]*)
	let modules      = 
	  with_input_file pack (
	    fun input ->
	      let modules = ref [] in
		try
		  while true do
		    let m = input_line input in
		      (*Printf.eprintf "Reading %S\n" m;*)
		      modules := m::!modules
		  done; assert false
		with End_of_file -> !modules			      
	  ) in

	(*For each module name, first generate the .mli file if it doesn't exist yet.*)
	  List.iter ignore_good (build (List.map(fun m -> expand_module include_dirs m ["mli";
										       "inferred.mli"]) modules));

	(*Deduce file names from these module names and wait for these dependencies to be solved.
	  
	  [build] has a mysterious behaviour which looks like cooperative threading without
	  threads and without call/cc.*)
	let results = build (List.map(fun m -> expand_module include_dirs m ["mli.depends"; 
									     "inferred.mli.depends"]) 
			       modules) in*)
      end
end*)

(*module Generatemli =
struct
  let after_rules () =
    rule "%.mllib to %.mli"
      ~dep:"%.mllib"
end
*)
(*
(** Link a .cma to another name. *)
module Linkas =
struct
  let after_rules () =
    rule "%.cma and %.linkas to [linkas].cma, where [linkas] is the content of %.rename"
      ~deps:["%.cma";"%.linkaas"]
      begin fun env build ->
	
      end
end
*)

module Preprocessing =
struct
  let before_options () = ()

  let after_rules () = 
(*    flag ["ocaml"; "pp";       "camlp4ofix"]  (S[A"camlp4o";
						 A"-printer"; A"o";
						 A"-sep"; A "";
						 A "|";
						 A "sed"; A "-e"; A"'s/\\*)/*)\\n/"]);*)
(*    dep  ["ocaml"; "ocamldep"; "camlp4ofix"]  ["build/fix_camlp4_print.cmo"];
    flag ["ocaml"; "pp";       "camlp4ofix"]  (S[A"camlp4"; 
						 A"-parser";  A"Camlp4OCamlRevisedParser";
						 A"-parser";  A"Camlp4OCamlParser";
						 A"-printer"; A"Camlp4OCamlPrinter";
						 A"-printer"; A"build/fix_camlp4_print.cmo"]);*)
(*    flag ["ocaml"; "pp";       "camlp4ofix"]  (S[A"camlp4"; 
						 A"-parser"; A"Camlp4OCamlRevisedParser";
						 A"-parser"; A"Camlp4OCamlParser";
						 A"-printer"; A"Camlp4OCamlPrinter";
						 A"-sep"; A" "
(*						 A"-sep"; A "\"(*hack*)\""*)]);*)

    (* optcomp as a syntax extension *)
    flag ["ocaml"; "pp";       "use_pa_optcomp"] (A"build/optcomp/pa_optcomp.cmo");
    dep  ["ocaml"; "ocamldep"; "use_pa_optcomp"] ["build/optcomp/pa_optcomp.cmo"];

    (* optcomp as a standalone preprocessor, original syntax *)
    flag ["ocaml"; "pp";       "use_optcomp_o"] (A"build/optcomp/optcomp_o.byte");
    dep  ["ocaml"; "ocamldep"; "use_optcomp_o"] ["build/optcomp/optcomp_o.byte"];

    (* optcomp as a standalone preprocessor, revised syntax *)
    flag ["ocaml"; "pp";       "use_optcomp_r"] (A"build/optcomp/optcomp_r.byte");
    dep  ["ocaml"; "ocamldep"; "use_optcomp_r"] ["build/optcomp/optcomp_r.byte"]

end

let _ = dispatch begin function
   | Before_options ->
       OCamlFind.before_options     ();
       Documentation.before_options ();
       Packs.before_options         ();
       Preprocessing.before_options ()
   | After_rules ->
       OCamlFind.after_rules     ();
       Documentation.after_rules ();
       Packs.after_rules         ();
(*       Complete_mllib.after_rules ();*)
       Misc.after_rules          ();
       Preprocessing.after_rules ()
       
   | _ -> ()
end
