(*
  This file is Public Domain
*)

open Ocamlbuild_plugin
open Command (* no longer needed for OCaml >= 3.10.2 *)
open Ocamlbuild_pack
open Ocaml_utils
open My_std.Outcome
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
       flag ["ocaml"; "byte"; "link"] & A"-linkpkg";

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
    dep  ["ocaml"; "doc"]   & ["build/odoc_generator_batlib.cmo"];
    flag ["ocaml"; "doc"]   & S[A "-i"; A "_build/build"; 
				A "-i"; A "build";
				A "-g"; A "odoc_generator_batlib.cmo"; 
			        A "-t"; A "OCaml Batteries Included" ;
				A "-intro"; A "../build/intro.text"]
end



(** Create a .mli for each .mlpack which doesn't already have one. *)
module Packs =
struct

  let _PRODUCE_MLI_FROM_PACK = true  (*current workaround for .mlpack + ocamldoc issues*)
  let _PRODUCE_PACKED_ML     = false (*not ready for prime-time*)

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
      
    let find str sub =
      let sublen = length sub in
	if sublen = 0 then
	  0
	else
	  let found = ref 0 in
	  let len = length str in
	    try
	      for i = 0 to len - sublen do
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
  end
    
  let read_dependency dep_name extension =
    let module_name = String.capitalize (Filename.basename (Filename.chop_suffix dep_name extension)) in
    let f = open_in dep_name in
    let (file_name, dependencies) = String.split (input_line f) ":" in
    let result = (file_name, module_name, List.filter (fun x -> not (String.length x = 0)) (String.nsplit dependencies " ")) in
      close_in f;
      result
	
	
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
    
  (** Read the dependencies from a directory and sort them
      
      If [l] is a list of files created by [ocamldep], [sort l] reads
      all the files in [l] and produces a sorted list of pairs [(name,
      mli)], where [name] is the name of the module and [mli] is the
      name of the corresponding [mli] file. This list is sorted topologically
      so as to make sure that whenever a module [M] depends on a module [N],
      [N] appears before [M]. 

      As usual, cyclic dependencies are bad.*)
  let sort files extension = 
    let dep     = Dependency.create () (*Direct  dependencies*)
    and rev     = Dependency.create () (*Reverse dependencies*)
    and modules = ref StringSet.empty      (*All modules involved, including external ones*)
    and src : (string, string) Hashtbl.t = Hashtbl.create 100 in
      (*Read all the dependencies and store them in the tables*)
      List.iter (
	fun f -> 
	  if Filename.check_suffix f ".depends" then
	    let (file_name, module_name, dependencies) = read_dependency f extension in
	      List.iter (fun x ->
			   modules := StringSet.add x !modules;
			   Dependency.add dep module_name x; 
			   Dependency.add rev x module_name
			)
		dependencies;
	      modules := StringSet.add module_name !modules;
	      Hashtbl.replace src module_name file_name
	  else ()
      ) files ;
      (*Now, start sorting*)
      let rec aux (sorted:string list) (rest: string list) =
	match rest with 
	  | [] -> 
	      sorted
	  | _ ->
	      (*Find nodes which haven't been removed and depend on nothing*)
	      match List.fold_left (fun (keep, remove) k -> 
				      match Dependency.find dep k with
					| None   -> 
					    (keep, k::remove)
					| Some dependencies ->
					    (k::keep, remove)
				   ) ([],[]) rest
	      with
		| (_, [])       -> 
		    Printf.eprintf "Cyclic dependencies in %a\n" Dependency.print dep;
		    assert false
		| (rest, roots) ->
		    List.iter (fun d ->
				 List.iter                (*Dependency [d] has been resolved, remove it.*)
				   (fun x -> Dependency.remove dep x d)
				   (Dependency.find_all rev d)) roots;
		    aux (sorted @ roots) rest in
      let sorted = aux [] (StringSet.elements !modules) in
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
     ]*)
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

    (*let scan chan = *)

    (*TODO: Scan the start of the file until the first [(**]
            Initialize counter at 1
            Then scan what comes next until either
                  1. [(*]   -> increment counter
                  2. [*)]   -> decrement counter
            If counter reaches 0, stop and return [(hd, tl)]
            where [hd] is everything up to this point
            and   [tl] is the rest of the file, either as a [channel_in]
            or as a [string] or [string_list]
    *)*)
    (*let scan file*)
(*    in
    let feed file buf () =
      with_input_file file (
	fun cin ->
	  try while true do
	    Buffer.add_string buf  (input_line cin);
	    Buffer.add_char   buf  '\n'
	  done
	  with End_of_file -> ()
      )in
    let print_modules buf () =
      List.iter (
	fun (name, src) -> 
	  let name = try 
	    let index = String.find name ".inferred" in
	      String.sub name 0 index
	  with String.Invalid_string -> name in
	  Printf.bprintf buf "module %s:(*from %S*)\nsig\n%a\nend\n" name src (feed src) ()
	  (*try ignore (String.find name ".inferred")
	  with String.Invalid_string -> Printf.bprintf buf "module %s:(*from %S*)\nsig\n%a\nend\n" name src (feed src) ()*)
      ) l in
      (*  Printf.fprintf out "module %s:\nsig\n%a\nend\n" pack print_modules ()*)
      print_modules buf ()*)
    in
  let print_modules buf () =
    List.iter (
      fun (name, src) ->
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

end

(**
   {1 Others}
*)
module Misc =
struct
  let after_rules () =
    flag ["ocaml"; "link"; "byte";   "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"; A "odoc_info.cma"]);
    flag ["ocaml"; "link"; "native"; "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"(*; A "odoc_info.cmxa"*)]);
    flag ["ocaml"; "docfile";        "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);
    flag ["ocaml"; "docdir";         "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);
    flag ["ocaml"; "doc";            "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"])    
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

let _ = dispatch begin function
   | Before_options ->
       OCamlFind.before_options     ();
       Documentation.before_options ();
       Packs.before_options         ()
   | After_rules ->
       OCamlFind.after_rules     ();
       Documentation.after_rules ();
       Packs.after_rules         ();
(*       Complete_mllib.after_rules ();*)
       Misc.after_rules          ()
       
   | _ -> ()
end
