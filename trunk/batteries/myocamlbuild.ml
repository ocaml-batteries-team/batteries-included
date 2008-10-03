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
    flag ["ocaml"; "doc"]   & S[(*A "-v";*)
				A "-i"; A "_build/build"; 
				A "-i"; A "build";
				A "-g"; A "odoc_generator_batlib.cmo"; 
			        A "-t"; A "OCaml Batteries Included" ;
				A "-intro"; A "../build/intro.text"]
end



(** Create a .mli for each .mlpack which doesn't already have one. *)
module Packs =
struct

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
    
  let read_dependency dep_name =
    let module_name = String.capitalize (Filename.basename (Filename.chop_suffix dep_name ".mli.depends")) in
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
  let sort files = 
    let dep     = Dependency.create () (*Direct  dependencies*)
    and rev     = Dependency.create () (*Reverse dependencies*)
    and modules = ref StringSet.empty      (*All modules involved, including external ones*)
    and src : (string, string) Hashtbl.t = Hashtbl.create 100 in
      (*Read all the dependencies and store them in the tables*)
      List.iter (
	fun f -> 
	  if Filename.check_suffix f ".mli.depends" then
	    let (file_name, module_name, dependencies) = read_dependency f in
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

  let generate_mli buf l =
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
	  (*Printf.eprintf "Writing the contents of %s\n" name;*)
	  let name = try 
	    let index = String.find name ".inferred" in
	      String.sub name 0 index
	  with String.Invalid_string -> name in
	  Printf.bprintf buf "module %s:(*from %S*)\nsig\n%a\nend\n" name src (feed src) ()
	  (*try ignore (String.find name ".inferred")
	  with String.Invalid_string -> Printf.bprintf buf "module %s:(*from %S*)\nsig\n%a\nend\n" name src (feed src) ()*)
      ) l in
      (*  Printf.fprintf out "module %s:\nsig\n%a\nend\n" pack print_modules ()*)
      print_modules buf ()
	

  (** Tiny parser *)
(*  type token =   Code    of string
	       | Comment of string*)

  (*let token_buf = Buffer.create 16

  let as_code ()  = Code (Buffer.contents token_buf)
  let as_comment()= Comment (Buffer.contents token_buf)

  let return v    = (*Stream.ising v*)[v]
  let cons   v f  = (*Stream.icons v (Stream.slazy f)*) v::(f ())
  let add_char c  =
    Stream.junk;
    Buffer.add_char token_buf c

  let rec next_token stream =
    try match Stream.next stream with
      |	Some '(' -> maybe_comment stream
      | Some c   -> Buffer.add_char token_buf c; 
	  next_token stream
      | None     -> return (as_code ())
  and maybe_comment stream =
    match Stream.next stream with
      | Some '*' -> cons   (as_code ()) (fun () -> comment stream)
      | Some c   ->
	  Buffer.add_char token_buf '('; 
	  Buffer.add_char token_buf c; 
	  next_token stream
      | None     -> return (as_code ())
  and comment stream =
    match Stream.next stream with
      | Some '(' -> 
	  Buffer.add_char token_buf '(';
	  maybe_nested_comment 1 stream
      | Some '*' -> maybe_end_of_comment stream
      | Some c   ->
	  Buffer.add_char token_buf c;
	  comment stream
      | None     -> return (as_comment ())
  and maybe_nested_comment n stream =
    match Stream.next stream with
      | Some '*' -> 
	  Buffer.add_char token_buf '*';
	  nested_comment n stream
      | Some c   ->
	  Buffer.add_char token_buf c  ;
	  comment stream
      | None     -> return (as_comment ())
  and maybe_end_of_comment stream =
    match Stream.next stream with
      | Some ')' -> 
	  cons (as_comment ()) (fun () -> next_token stream)
      | Some c   ->
	  Buffer.add_char token_buf '*';
	  Buffer.add_char token_buf c;
	  comment stream
      | None     ->
	  Buffer.add_char token_buf '*';
	  return (as_comment ())
  and nested_comment n stream =
    match Stream.next stream with
      | Some '(' -> 
	  Buffer.add_char token_buf '(';
	  maybe_nested_comment (n + 1) stream
      | Some '*' -> 
	  maybe_end_of_nested_comment n stream
      | Some c   ->
	  Buffer.add_char token_buf c;
	  nested_comment n stream
      | None     -> return (as_comment ())
  and maybe_end_of_nested_comment n stream =
    match Stream.next stream with
      | Some ')' -> if n >= 2 then begin
	  Buffer.add_char token_buf ')';
	  nested_comment (n - 1) stream
	end else begin
	  Buffer.add_char token_buf ')';
	  comment stream
	end
      | Some c   ->
	  Buffer.add_char token_buf '*';
	  Buffer.add_char token_buf c;
	  nested_comment n stream
      | None     ->
	  Buffer.add_char token_buf '*';
	  return (as_comment ())*)




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
    rule "ocaml: mllib & cmx* & o* -> cmxa & a -- with pack"
      ~tags:["ocaml"; "native"; "library"]
      ~prods:["%.cmxa"; Pathname.add_extension "%"  !Options.ext_lib]
      ~dep:"%.mllib"
      begin fun env build ->
	assert false
      end;

    rule ".mlpacklit to .mli conversion rule"
      ~prod:"%.mli"
      ~dep:"%.mlpacklit"
      begin fun env build ->
        (*c The action is a function that receive two arguments:
          [env] is a conversion function that substitutes `\%' occurrences
          according to the targets to which the rule applies.
          [_build] can be called to build new things (dynamic dependencies). *)
	
	let pack         = env "%.mlpacklit" 
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
	(*let dest'  = (*Pathname.concat (Sys.getcwd ()) dest*) Resource.in_build_dir dest in*)
(*	let dest' = "/tmp/"^dest in*)
(*	  Shell.mkdir_p (Pathname.parent_directory dest');*)
(*	let dest = Pathname.concat (Sys.getcwd ()) dest in*)
(*	let dest = Resource.in_build_dir dest in*)
(*	let dest = Pathname.pwd / !Options.build_dir / dest in
	  Shell.mkdir_p (Filename.dirname dest);*)
(*	  Printf.eprintf "Writing file %S\n" dest;*)
	let buf = Buffer.create 2048 in
	  generate_mli buf (sort mli_depends);
(*	  flush output;*)
(*	  Printf.eprintf "Written %d bytes\n" (pos_out output);*)
(*	  if not (My_std.sys_file_exists dest) then assert false;*)
	  (*if not (My_std.sys_file_exists dest) then assert false;*)
	  (*Resource.Cache.import_in_build_dir dest';*)
	  Echo([Buffer.contents buf], dest)
        end;

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
    flag ["ocaml"; "doc";            "use_ocamldoc_info"] (S[A "-I"; A "+ocamldoc"]);
    
    flag ["ocaml"; "use_core"; "compile"] (P "src/core/core.cma");
    ocaml_lib ~dir:"src/core" "core.cma"


end

let _ = dispatch begin function
   | Before_options ->
       OCamlFind.before_options     ();
       Documentation.before_options ();
       Packs.before_options         ()
   | After_rules ->
       OCamlFind.after_rules     ();
       Documentation.after_rules ();
       Packs.after_rules         ();
       Misc.after_rules          ()
       
   | _ -> ()
end
