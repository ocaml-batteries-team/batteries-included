(*
 * generate_mli.ml
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

(* This file generate a .mli from a simplified .ml file. It should be
   invoked like that:

   $ generate_mli input.ml > output.mli

   Allowed constructions in the .ml are:

   - module aliases: "module Foo = Bar"
     ("Bar" must be an external module)
   - structures: "module Foo = struct ... end"
   - comments
*)

open Printf
open Camlp4.PreCast
open Camlp4.Sig

(* +----------------------------------------+
   | Batteries specifics harcorded settings |
   +----------------------------------------+ *)
(*
let search_paths = ref [
  "src/core";
  "src/core/extlib";
  "src/core/extlib_threads";
  "src/core/baselib";
  "src/core/baselib_threads";
  "src/libs";
  "src/libs/camlzip";
  "src/libs/compilers";
  "src/libs/common";
  "src/libs/findlib";
  "src/libs/ocamlnet";
  "src/libs/sexplib";
]
*)
(* +------------------+
   | Stream utilities |
   +------------------+ *)

let map_stream f stream =
  Stream.from (fun _ -> Some(f (Stream.next stream)))

let keywords = [ "open"; "module"; "struct"; "object"; "sig"; "end"; "."; "=" ]

let filter_keywords =
  map_stream (function
                | ((LIDENT id, loc) | (SYMBOL id, loc)) when List.mem id keywords -> (KEYWORD id, loc)
                | x -> x)

let njunk n stream =
  for i = 1 to n do
    Stream.junk stream
  done

(* +------------------------------+
   | Module signatures resolution |
   +------------------------------+ *)

type path = string list
    (* A module path. It is the list of components *)

let rec find_map f = function
  | [] ->
      None
  | x :: l ->
      match f x with
        | None ->
            find_map f l
        | y ->
            y
(*
(* Return [Some(filename, module_path_in_filename)] or [None] *)
let rec resolv_path = function
  | [] ->
      None
  | name :: rest ->
      let names = [name; String.uncapitalize name] in
      match
        find_map (fun search_path ->
                    find_map (fun name ->
                                find_map (fun ext ->
                                            let fname = sprintf "%s/%s.%s" search_path name ext in
                                            if Sys.file_exists fname then
                                              Some fname
                                            else
                                              None)
                                  [ "mli"; "mlpack" ])
                      names) !search_paths
      with
        | None ->
            None
        | Some filename ->
            if Filename.check_suffix filename ".mlpack" then
              resolv_path rest
            else
              Some(filename, rest)
*)
(* +--------------------------------+
   | Filtering (parsing + printing) |
   +--------------------------------+ *)

external filter : 'a Gram.not_filtered -> 'a = "%identity"

let tokens_of_channel filename ic =
  filter_keywords
    (filter
       (Gram.lex (Loc.mk filename)
          (Stream.of_channel ic)))

let print_tokens stream =
  try
    Stream.iter (fun (tok, loc) -> Optcomp.print_token tok) stream
  with
      Exit -> ()

let rec skip_blanks stream = match Stream.peek stream with
  | Some((BLANKS _ | NEWLINE | COMMENT _), _) ->
      Stream.junk stream;
      skip_blanks stream
  | _ ->
      ()

let parse_uident stream = match Stream.next stream with
  | (UIDENT id, loc) -> id
  | (_, loc) -> Loc.raise loc (Failure "upper identifier expected")

let parse_equal stream = match Stream.next stream with
    | (KEYWORD "=", _) ->
        ()
    | (_, loc) ->
        Loc.raise loc (Failure "'=' expected")

(* Search and print a module in a file: *)
let rec search_print_module stream = function
  | [] ->
      (* Search "sig": *)
      while fst (Stream.next stream) <> KEYWORD "sig" do () done;
      (* Now print until we reach the corresponding "end": *)
      let rec loop level = match Stream.next stream with
        | (KEYWORD (("sig" | "object") as kwd), _) ->
            print_string kwd;
            loop (level + 1)
        | (KEYWORD "end", _) ->
            if level = 0 then
              print_newline ()
            else begin
              print_endline "end";
              loop (level - 1)
            end
        | (EOI, _) ->
            ()
        | (tok, loc) ->
            Optcomp.print_token tok;
            loop level
      in
      loop 0

  | name :: rest ->
      let rec skip_module level = match Stream.next stream with
        | (KEYWORD ("sig" | "object"), _) ->
            skip_module (level + 1)
        | (KEYWORD "end", _) ->
            if level = 1 then
              ()
            else
              skip_module (level - 1)
        | (EOI, _) ->
            ()
        | _ ->
            skip_module level
      in
      (* Find the next module *)
      let rec next_module last_comment = match Stream.next stream with
        | COMMENT str, _ ->
            next_module str
        | KEYWORD "module", _ ->
            skip_blanks stream;
            (last_comment, parse_uident stream)
        | EOI, loc ->
            Loc.raise loc (Failure(sprintf "module %S not found" name))
        | _ ->
            next_module last_comment
      in
      let comment, id = next_module "" in
      if id = name then begin
        print_string comment;
        search_print_module stream rest
      end else begin
        skip_module 0;
        search_print_module stream (name :: rest)
      end

let rec print_docs stream = match Stream.peek stream with
  | Some(COMMENT str, _) ->
      njunk 1 stream;
      if String.length str > 4 && str.[2] = '*' && str.[3] <> '*'  then
        print_string str;
      print_docs stream
  | Some((BLANKS _ | NEWLINE) as tok, _) ->
      njunk 1 stream;
      Optcomp.print_token tok;
      print_docs stream
  | _ ->
      ()

(* Extract a module signature from a file. [path] is the path in the
   filename, the empty list means the whole file. *)
let print_from filename path =
  let ic = open_in filename in
  let stream = tokens_of_channel filename ic in
  let rec next i =
    match Stream.npeek 3 stream with
      | (UIDENT id, _) :: (KEYWORD ".", _) :: _ when String.length id > 3 && String.sub id 0 3 = "Ext" ->
          njunk 2 stream;
          next i
      | (KEYWORD "open", _) :: (BLANKS _, _) :: (UIDENT id, _) :: _ when String.length id > 3 && String.sub id 0 3 = "Ext" ->
          njunk 3 stream;
          next i
      | _ ->
          Some(Stream.next stream)
  in
  let stream = Stream.from next in
  begin match path with
    | [] ->
        print_docs stream;
        print_tokens stream;
    | [_] ->
        print_docs stream;
        search_print_module stream path
    | _ ->
        search_print_module stream path
  end;
  close_in ic

(* Parse a module path *)
let rec parse_path stream =
  match Stream.npeek 2 stream with
    | [(UIDENT id, _); (KEYWORD ".", _)] ->
        njunk 2 stream;
        id :: parse_path stream
    | (UIDENT id, _) :: _ ->
        njunk 1 stream;
        [id]
    | (_, loc) :: l ->
        Loc.raise loc (Failure "invalid module path")
    | _ ->
        raise End_of_file
(*
(* Filter input *)
let rec loop stream =
  match Stream.next stream with
    | (KEYWORD "module", _) ->
        skip_blanks stream;
        let id = parse_uident stream in
        skip_blanks stream;
        parse_equal stream;
        skip_blanks stream;
        printf "module %s : sig\n" id;
        begin match Stream.peek stream with
          | Some(KEYWORD "struct", _) ->
              njunk 1 stream;
              loop stream
          | _ ->
              let path = parse_path stream in
              match resolv_path path with
                | Some(filename, path) ->
                    print_from filename path;
                    print_string "end\n"
                | None ->
                    eprintf "generate_mli: module %S not found!\n%!" (String.concat "." path)
        end;
        loop stream
    | (EOI, _) ->
        ()
    | (tok, loc) ->
        Optcomp.print_token tok;
        loop stream
*)
(* +-------+
   | Tools |
   +-------+ *)

let path_of_string string =
  Str.split (Str.regexp "\\.") string 

(* +------+
   | Main |
   +------+ *)

let _ =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "usage: %s <file> [<path>]\n%!" (Filename.basename Sys.argv.(0));
    exit 2
  end;
  try
    let fname = Sys.argv.(1) in
    let path  = 
      if Array.length Sys.argv < 3 then []
      else 
	begin
	  let result = path_of_string (Sys.argv.(2)) in
	    Printf.eprintf "Extracting module %a from file %s\n%!" 
	      (fun out l -> List.iter (fun x -> Printf.fprintf out "%s/" x) l) result fname;
	    result
	end
    in

(*      search_paths := Filename.dirname fname :: !search_paths;*)
      print_from fname path
  with
    | Exit as e ->
        raise e
    | exn ->
        Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exn

