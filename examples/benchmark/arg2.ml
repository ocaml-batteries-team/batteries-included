(* O'Caml enhanced command line argument handling module            *)
(* by Travis Bemann and Eric Norige                                 *)
(*                                                                  *)
(* This program is free software; you can redistribute it and/or    *)
(* modify it under the terms of the GNU Lesser General Public       *)
(* License as published by the Free Software Foundation; either     *)
(* version 2 of the License, or (at your option) any later version. *)
(*                                                                  *)
(* This program is distributed in the hope that it will be useful,  *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of   *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *)
(* GNU Lesser General Public License for more details.              *)

type spec =
    Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | String_var of string ref   (* Set the reference to the string argument *)
  | Int_var of int ref         (* Set the reference to the int argument *)
  | Float_var of float ref     (* Set the reference to the float argument *)
  | Rest of (string -> unit);; (* Stop interpreting keywords and call the
                                  function with each remaining argument *)

type switch =
    Char of char               (* Character switch *)
  | Char_arg of char * string  (* Character switch with argument name *)
  | Char_extra of char * string
      (* Character switch with extra non-simple argument information,
       * usually final argument list information *)
  | Name of string             (* Name switch *)
  | Name_arg of string * string
      (* Name switch with argument name *)
  | Name_extra of string * string
      (* Name switch with extra non-simple argument information,
       * usually final argument list information *)
  | Both of char * string      (* Both character and name switch *)
  | Both_arg of char * string * string
      (* Both character and name switch with argument name *)
  | Both_extra of char * string * string;;
      (* Both character and name switch with extra non-simple argument
       * information, usually final argument list information *)

exception Bad of string;;
exception Usage;;
exception Parse_halt;;

let descr_col = 29;; (* Column (with first column being zero) where
			description begins; if the switch information
			extends to or beyond this column, then the
			description is pushed to the next line *)

let max_col = 79;;

let switch_indent = 2;; (* Number of columns to indent switch
			   information by *)

let usage_head = "Usage: ";;

let switch_format switch =
  match switch with
    Char ch ->
      Printf.sprintf "-%c" ch
  | Char_arg (ch, arg) ->
      Printf.sprintf "-%c %s" ch arg
  | Char_extra (ch, extra) ->
      Printf.sprintf "-%c %s" ch extra
  | Name name ->
      Printf.sprintf "    --%s" name
  | Name_arg (name, arg) ->
      Printf.sprintf "    --%s=%s" name arg
  | Name_extra (name, extra) ->
      Printf.sprintf "    --%s %s" name extra
  | Both (ch, name) ->
      Printf.sprintf "-%c, --%s" ch name
  | Both_arg (ch, name, arg) ->
      Printf.sprintf "-%c %s, --%s=%s" ch arg name arg
  | Both_extra (ch, name, extra) ->
      Printf.sprintf "-%c, --%s %s" ch name extra;;

let whitespace chars =
  String.make chars ' ';;

let usage_indent_str = whitespace (String.length usage_head);;

let switch_indent_str = whitespace switch_indent;;

let descr_indent_str = whitespace descr_col;;

let word_break str off_start len_start =
  let rec step off len space_off =
    if (len > 0) && (off < (String.length str)) then
      let ch = String.get str off in
      match ch with
	' ' | '\t' | '\n' | '\r' ->
	  step (off + 1) (len - 1) off
      |	_ ->
	  step (off + 1) (len - 1) space_off
    else
      if (off_start + len_start) >= (String.length str) then
	String.sub str off_start ((String.length str) - off_start),
	String.length str
      else
	if off_start <> space_off then
	  String.sub str off_start (space_off - off_start), (space_off + 1)
	else
	  String.sub str off_start len_start, (off_start + len_start)
  in
  step off_start len_start off_start;;

let descr_format descr =
  let buf = Buffer.create (String.length descr)
  and len = String.length descr
  and line_len = max_col - descr_col in
  let rec step off =
    if (len - off) <= line_len then
      begin
	Buffer.add_string buf (String.sub descr off ((String.length descr) - off));
	Buffer.contents buf
      end
    else
      let sub, off_next = word_break descr off line_len in
      Buffer.add_string buf sub;
      if off_next = (String.length descr) then
	Buffer.contents buf
      else
	begin
	  Printf.bprintf buf "\n%s" descr_indent_str;
	  step off_next
	end
  in
  step 0;;

let usage_format usage =
  let buf = Buffer.create (String.length usage)
  and len = String.length usage
  and line_len = max_col - (String.length usage_head) in
  let rec step off =
    if (len - off) <= line_len then
      begin
	Buffer.add_string buf (String.sub usage off ((String.length usage) - off));
	Buffer.contents buf
      end
    else
      let sub, off_next = word_break usage off line_len in
      Buffer.add_string buf sub;
      if off_next = (String.length usage) then
	Buffer.contents buf
      else
	begin
	  Printf.bprintf buf "\n%s" usage_indent_str;
	  step off_next
	end
  in
  step 0;;

let keyword_char keywords char =
  let matches keyword =
    let switch, _, _, _ = keyword in
    match switch with
      Char sw_char | Char_arg (sw_char, _) |
      Char_extra (sw_char, _) | Both (sw_char, _) |
      Both_arg (sw_char, _, _) |
      Both_extra (sw_char, _, _) ->
	char = sw_char
    | _ ->
	false in
  match List.filter matches keywords with
    keyword :: _ ->
      keyword
  | [] ->
      raise Not_found;;

let keyword_name keywords name =
  let matches keyword =
    let switch, _, _, _ = keyword in
    match switch with
      Name sw_name | Name_arg (sw_name, _) |
      Name_extra (sw_name, _) | Both (_, sw_name) |
      Both_arg (_, sw_name, _) |
      Both_extra (_, sw_name,_) ->
	name = sw_name
    | _ ->
	false in
  match List.filter matches keywords with
    keyword :: _ ->
      keyword
  | [] ->
      raise Not_found;;

let usage_raise () =
  raise Usage;;

let help_add keywords =
  try
    let _ = keyword_name keywords "help" in
    keywords
  with Not_found ->
    keywords @
    [Name "help", [Unit usage_raise], [], "Display this help and exit"];;

let usage ~keywords ~usage ~descr ~notes =
  let keywords = help_add keywords in
  Printf.printf "%s%s\n%s\n\n" usage_head (usage_format usage) descr;
  let print_switch keyword =
	let switch, _, _, descr = keyword in
	let switch_sh = switch_format switch in
	if
	  (switch_indent + (String.length switch_sh)) < (descr_col - 1)
	then
	  Printf.printf "%s%s%s%s\n"
	    switch_indent_str
	    switch_sh
	    (whitespace
	       (descr_col - ((String.length switch_sh) + switch_indent)))
	    (descr_format descr)
	else
	  Printf.printf "%s%s\n%s%s\n"
	    switch_indent_str
	    switch_sh
	    descr_indent_str
	    (descr_format descr) in
  List.iter print_switch keywords;
  print_newline ();
  if notes <> "" then
    if (String.get notes ((String.length notes) - 1)) <> '\n' then
      print_endline notes
    else
      print_string notes
  else
    ();;

(* here starts the code written by eric *)

type handler =
    Required of spec
  | Optional of spec * (unit -> unit);;

type token =
    Argument of string
  | Long_switch of string
  | Short_switch_list of char list
  | Long_switch_with_arg of string * string

let tokenize raw =
  try
    match (raw.[0], raw.[1]) with
      '-', '-' ->  (* A long command *)
	begin try
	  let split = String.index raw '=' in
	  let name = String.sub raw 2 (split - 2)
	  and data =
	    String.sub raw (split + 1) ((String.length raw) - split-1) in
	  Long_switch_with_arg (name, data)
	with
	  Not_found ->
	    Long_switch (String.sub raw 2 ((String.length raw) - 2))
	end
    | '-', _ ->  (* a sequence of short commands *)
	let char_list = ref [] in
	for i = 1 to (String.length raw) - 1 do
	  char_list := raw.[i] :: !char_list
	done;
	Short_switch_list !char_list
    | _ -> Argument raw  (* an argument to a command *)
  with Invalid_argument _ -> Argument raw


let qflush hq =
  let noarg = function
    | Required (Unit uh) -> uh ()
    | Required _ -> raise (Bad "no data left for required arguments")
    | Optional (_,unitfun) -> unitfun () in
  Queue.iter noarg hq;
  Queue.clear hq

let parse ~keywords ~others ~usage:args ~descr ~notes =
  let keywords = help_add keywords in
  let handler_queue = Queue.create ()
  and extra_arg_handler = ref others in
  let enqueue_require = function
    | Set ref -> ref := true
    | Clear ref -> ref := false
    | Rest sh -> extra_arg_handler := sh
    | h -> Queue.add (Required h) handler_queue
  and enqueue_option = function
    | (Set _,_) -> raise (Bad "Set arguments can't be optional")
    | (Clear _,_) -> raise (Bad "Clear options can't be optional")
    | (Rest sh,_) -> extra_arg_handler := sh
    | (s,u) -> Queue.add (Optional (s,u)) handler_queue
  in
  let push_char_handlers c =
    qflush handler_queue;
    try
      let (_, reqh, opth, _) = keyword_char keywords c in
      List.iter enqueue_require reqh;
      List.iter enqueue_option opth
    with
      Not_found -> raise (Bad (Printf.sprintf "char argument not found: %c" c))
  and push_long_handlers name =
    qflush handler_queue;
    try
      let (_, reqh, opth, _) = keyword_name keywords name in
      List.iter enqueue_require reqh;
      List.iter enqueue_option opth
    with
      Not_found -> raise (Bad ("argument not found:" ^ name))
  in
  let rec handle arg =
    try
      let handler =
	match Queue.take handler_queue with
	    Required h -> h
	  | Optional (h,_) -> h in
      match handler with
	  String sh -> sh arg
	| Int ih -> ih (int_of_string arg)
	| Float fh -> fh (float_of_string arg)
	| String_var svh -> svh := arg
	| Int_var ivh -> ivh := (int_of_string arg)
	| Float_var fvh -> fvh := (float_of_string arg)
	| Unit uh -> uh (); handle arg
	| _ -> raise (Bad "Ran into bad argument handler")
    with
	Queue.Empty -> !extra_arg_handler arg
      | Failure x -> raise (Bad ("Failed converting: "^x))
  in
  let act_on_arg arg =
    match tokenize arg with
      Long_switch name ->
	push_long_handlers name
    | Short_switch_list short_list ->
	List.iter push_char_handlers short_list
    | Argument arg ->
	handle arg
    | Long_switch_with_arg (name, arg) ->
	push_long_handlers name;
	handle arg
  in
  try
    let argv = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) in
    Array.iter act_on_arg argv;
    qflush handler_queue
  with
    Bad msg ->
      usage keywords args descr notes;
      print_endline msg;
      raise Parse_halt
  | Usage ->
      usage keywords args descr notes;
      exit 0;;
