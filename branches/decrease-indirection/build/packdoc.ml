(*open Batteries
open System
open Data.Mutable
open Data.Persistent
open Data.Text
open Languages*)

module String =
struct
  include String

  exception Invalid_string

  let is_empty s = length s = 0 
      
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
    Hashtbl.iter (fun k set -> Printf.fprintf out "%s: {%a}\n"
		    k
		    (fun out s -> List.iter (fun x -> Printf.fprintf out "%s; " x) s)
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
      Printf.eprintf "Preparing a generation\n";
      List.iter (fun x -> Printf.eprintf "Need to sort   %s\n" x) rest;
      List.iter (fun x -> Printf.eprintf "Already sorted %s\n" x) sorted;
      (*Printf.eprintf "I still need to sort %a\n" (Printf.make_list_printer IO.nwrite "[" "]" "; ") rest;*)
      match rest with 
	| [] -> 
	    sorted
	| _ ->
	(*Find nodes which haven't been removed and depend on nothing*)
	match List.fold_left (fun (keep, remove) k -> 
				match Dependency.find dep k with
				  | None   -> 
				      Printf.eprintf "Module %s has no dependencies left\n" k;
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
      List.fold_left (fun acc module_name ->
			Printf.eprintf "Should I add module %s?\n" module_name;
			try  (module_name, Hashtbl.find src module_name)::acc
			with Not_found -> acc) [] sorted

let generate_mli cout pack l =
  let feed file cout () =
    let cin = open_in file         in
    let buf = String.make 1024 ' ' in
    let rec aux () =
      let read = input cin buf 0 1024 in
	if read = 0 then 
	  Printf.eprintf "Done writing\n"
	else 
	  begin
	  Printf.eprintf "Writing %d bytes\n" read;
	  output cout buf 0 read;
	  aux ()
	end
    in aux ()
  in let print_modules cout () =
    List.iter (
      fun (name, src) -> 
	Printf.fprintf cout "module %s:\nsig\n%a\nend\n" name (feed src) ()
    ) l in
(*  Printf.fprintf out "module %s:\nsig\n%a\nend\n" pack print_modules ()*)
    Printf.eprintf "Starting mli generation\n%!";
    List.iter (fun (name,_) -> Printf.eprintf "Module %s " name) l;
    print_modules cout ();
    Printf.eprintf "\nModule generation complete\n%!"


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
  and out = match !out with "" -> stdout | name -> open_out name 
  and pack= match !pack with"" -> failwith "Missing argument -pack" | name -> name in
    Printf.eprintf "Sorting directory %s/%s\n" (Unix.getcwd()) dir;
    generate_mli out pack (sort dir)
