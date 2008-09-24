open Batteries
open System
open Data.Mutable
open Data.Persistent
open Data.Text
open Languages

let read_dependency dep_name =
  let module_name = String.capitalize (Filename.basename (Filename.chop_suffix dep_name ".mli.depends")) in
(*  Printf.eprintf "Reading %S => %s\n" dep_name module_name;*)
  File.with_file_in dep_name (
    fun f ->
      let (file_name, dependencies) = String.split (IO.read_line f) ":" in
	(file_name, module_name, List.filter (fun x -> not (String.is_empty x)) (String.nsplit dependencies " "))
  )

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
    Hashtbl.iter (fun k set -> Printf.fprintf out "%s: %a\n"
		    k
		    (Printf.make_list_printer IO.nwrite "{" "}" "; ")
		    (StringSet.elements set)) tbl;
    Printf.fprintf out "}\n"
end

(** Read the dependencies from a directory and sort them*)
let sort directory =
  let dep     = Dependency.create () (*Direct  dependencies*)
  and rev     = Dependency.create () (*Reverse dependencies*)
  and modules = ref StringSet.empty      (*All modules involved, including external ones*)
  and src : (string, string) Hashtbl.t = Hashtbl.create 100
  and files = Sys.readdir directory in
    (*Read all the dependencies and store them in the tables*)
    Array.iter (
      fun f -> 
	if Filename.check_suffix f ".mli.depends" then
	  let (file_name, module_name, dependencies) = read_dependency (Filename.concat directory f) in
	    List.iter (fun x ->
(*			 Printf.eprintf "Adding a dependency %S => %S\n" module_name x;*)
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
(*      Printf.eprintf "Preparing a generation\n";
      Printf.eprintf "I still need to sort %a\n" (Printf.make_list_printer IO.nwrite "[" "]" "; ") rest;*)
      match rest with 
	| [] -> 
	    sorted
	| _ ->
	(*Find nodes which haven't been removed and depend on nothing*)
	match List.fold_left (fun (keep, remove) k -> 
				match Dependency.find dep k with
				  | None   -> 
(*				      Printf.eprintf "Module %s has no dependencies left\n"
					k;*)
				      (keep, k::remove)
				  | Some dependencies ->
(*				      Printf.eprintf "Module %s can't be removed, as it depends on %a (%d)\n"
					k
					(Printf.make_list_printer IO.nwrite "[" "]" "; ")
					(StringSet.elements dependencies) 
					(List.length (StringSet.elements dependencies) );*)
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
	      aux (sorted @ roots) rest
    in 
    let sorted = aux [] (StringSet.elements !modules) in
      List.filter_map (fun module_name -> 
			 try Some (module_name, Hashtbl.find src module_name)
			 with Not_found -> 
(*			   Printf.eprintf "I'm not going to add module %s, it's external\n%!" module_name;*)
			   None) sorted
let generate_mli out pack l =
  let feed file out () =
    File.with_file_in file (
      fun input -> IO.write_line_enum out (IO.lines_of input) 
    ) in
  let print_modules out () =
    List.iter (
      fun (name, src) -> 
	Printf.fprintf out "module %s:\nsig\n%a\nend\n" name (feed src) ()
    ) l in
(*  Printf.fprintf out "module %s:\nsig\n%a\nend\n" pack print_modules ()*)
    print_modules out ()


let dir = ref "."
let out = ref ""
let pack= ref ""


let _ = 
  let _ = Arg.parse [("-i",    Arg.Set_string dir, "Choose the directory containing dependencies" );
		     ("--in",  Arg.Set_string dir, "Choose the directory containing dependencies" );
		     ("-o",    Arg.Set_string out, "Choose a destination file (stdout by default)");
		     ("--out", Arg.Set_string out, "Choose a destination file (stdout by default)");
		     ("-pack", Arg.Set_string pack,"Set the name of the enclosing module")]
    ignore "" 
  in
  let dir = !dir
  and out = match !out with "" -> IO.stdout | name -> File.open_out name 
  and pack= match !pack with"" -> failwith "Missing argument -pack" | name -> name in
    generate_mli out pack (sort dir)

