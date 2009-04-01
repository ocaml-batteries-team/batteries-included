open Format
open Camlp4.PreCast
open Camlp4.Sig

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

let path_of_string string =
  Str.split (Str.regexp "\\.") string 

let string_of_path path =
  match path with
    | []   -> ""
    | h::t ->
	let buf = Buffer.create 32 in
	  Buffer.add_string buf h;
	  List.iter (Buffer.add_string buf) t;
	  Buffer.contents buf


(* +--------------------------------+
   | Filtering (parsing + printing) |
   +--------------------------------+ *)

external filter : 'a Gram.not_filtered -> 'a = "%identity"

let tokens_of_channel filename ic =
  filter_keywords
    (filter
       (Gram.lex (Loc.mk filename)
          (Stream.of_channel ic)))


let print_token ?(oc=Format.std_formatter) = function
  | KEYWORD kwd ->
      fprintf oc "%s" kwd
  | SYMBOL sym ->
      fprintf oc "%s" sym
  | LIDENT lid ->
      fprintf oc "%s" lid
  | UIDENT uid ->
      fprintf oc "%s" uid
  | ESCAPED_IDENT id ->
      fprintf oc "( %s )" id
  | INT(_, s) ->
      fprintf oc "%s" s
  | INT32(_, s) ->
      fprintf oc "%s%c" s 'l'
  | INT64(_, s) ->
      fprintf oc "%s%c" s 'L'
  | NATIVEINT(_, s) ->
      fprintf oc "%s%c" s 'n'
  | FLOAT(_, s) ->
      fprintf oc "%s" s
  | CHAR(_, s) ->
      fprintf oc "%s" s
  | STRING(_, s) ->
      fprintf oc "%S" s
  | LABEL lbl ->
      fprintf oc "~%s:" lbl
  | OPTLABEL lbl ->
      fprintf oc "?%s:" lbl
  | QUOTATION quot ->
      if quot.q_name = "" then
        fprintf oc "<<"
      else begin
        fprintf oc "<:%s" quot.q_name;
        if quot.q_loc <> "" then
          fprintf oc "%c%s" '@' quot.q_loc;
        fprintf oc "<"
      end;
      fprintf oc "%s>>" quot.q_contents
  | ANTIQUOT(n, s) ->
      fprintf oc "$";
      if n <> "" then
        fprintf oc "%s:" n;
      fprintf oc "%s$" s
  | COMMENT comment ->
      fprintf oc "%s" comment
  | BLANKS s ->
      fprintf oc "%s" s
  | NEWLINE ->
      fprintf oc "\n"
  | LINE_DIRECTIVE(n, fname_opt) ->
      fprintf oc "# %d" n;
      begin match fname_opt with
        | Some fname ->
            fprintf oc " %S\n" fname
        | None ->
            fprintf oc "\n"
      end
  | EOI ->
      raise Exit

let print_tokens oc stream =
  try
    Stream.iter (fun (tok, loc) -> print_token ~oc tok) stream
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
let rec search_print_module oc stream = function
  | [] ->
      (* Search "sig": *)
      while fst (Stream.next stream) <> KEYWORD "sig" do () done;
      (* Now print until we reach the corresponding "end": *)
      let rec loop level = match Stream.next stream with
        | (KEYWORD (("sig" | "object") as kwd), _) ->
            fprintf oc "%s" kwd;
            loop (level + 1)
        | (KEYWORD "end", _) ->
            if level = 0 then
              fprintf oc "\n"
            else begin
              fprintf oc "end\n";
              loop (level - 1)
            end
        | (EOI, _) ->
            ()
        | (tok, loc) ->
            print_token ~oc tok;
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
	fprintf oc "end\n%s" comment;
        search_print_module oc stream rest
      end else begin
        skip_module 0;
        search_print_module oc stream (name :: rest)
      end

let rec print_docs oc stream = match Stream.peek stream with
  | Some(COMMENT str, _) ->
      njunk 1 stream;
      if String.length str > 4 && str.[2] = '*' && str.[3] <> '*'  then
	fprintf oc "%s" str;
      print_docs oc stream
  | Some((BLANKS _ | NEWLINE) as tok, _) ->
      njunk 1 stream;
      print_token ~oc tok;
      print_docs oc stream
  | _ ->
      ()

(* Extract a module signature from a file. [path] is the module path
   inside the file, the empty list means the whole file. *)
let extract filename ic oc path =
  let stream = tokens_of_channel filename ic in
  let rec next i(*ignored*) =
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
          print_docs oc stream;
          print_tokens oc stream;
      | [_] ->
          print_docs oc stream;
          search_print_module oc stream path
      | _ ->
          search_print_module oc stream path
    end

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


