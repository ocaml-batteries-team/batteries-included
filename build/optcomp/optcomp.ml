(*
 * optcomp.ml
 * ----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

(* Standalone version *)

open Camlp4.PreCast
open Camlp4.Sig

let print_token = function
  | KEYWORD kwd ->
      print_string kwd
  | SYMBOL sym ->
      print_string sym
  | LIDENT lid ->
      print_string lid
  | UIDENT uid ->
      print_string uid
  | ESCAPED_IDENT id ->
      print_string "( ";
      print_string id;
      print_string " )"
  | INT(_, s) ->
      print_string s
  | INT32(_, s) ->
      print_string s;
      print_char 'l'
  | INT64(_, s) ->
      print_string s;
      print_char 'L'
  | NATIVEINT(_, s) ->
      print_string s;
      print_char 'n'
  | FLOAT(_, s) ->
      print_string s
  | CHAR(_, s) ->
      print_string s
  | STRING(_, s) ->
      print_char '"';
      print_string s;
      print_char '"'
  | LABEL lbl ->
      print_char '~';
      print_string lbl;
      print_char ':'
  | OPTLABEL lbl ->
      print_char '?';
      print_string lbl;
      print_char ':'
  | QUOTATION quot ->
      if quot.q_name = "" then
        print_string "<<"
      else begin
        print_string "<:";
        print_string quot.q_name;
        if quot.q_loc <> "" then begin
          print_char '@';
          print_string quot.q_loc
        end;
        print_char '<'
      end;
      print_string quot.q_contents;
      print_string ">>"
  | ANTIQUOT(n, s) ->
      print_char '$';
      if n <> "" then begin
        print_string n;
        print_char ':'
      end;
      print_string s;
      print_char '$'
  | COMMENT comment ->
      print_string comment
  | BLANKS s ->
      print_string s
  | NEWLINE ->
      print_newline ()
  | LINE_DIRECTIVE(n, fname_opt) ->
      print_string "# ";
      print_int n;
      begin match fname_opt with
        | Some fname ->
            print_string " \"";
            print_string fname;
            print_string "\"\n"
        | None ->
            print_newline ()
      end
  | EOI ->
      raise Exit

let filter_sharp stream =
  Stream.from (fun _ -> match Stream.next stream with
                 | (SYMBOL "#", loc) -> Some(KEYWORD "#", loc)
                 | x -> Some x)

external filter : 'a Gram.not_filtered -> 'a = "%identity"

let main () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "usage: %s <file>\n%!" (Sys.argv.(0));
    exit 2
  end;
  let ic = open_in Sys.argv.(1) in
  try
    Stream.iter
      (fun (tok, loc) -> print_token tok)
      (Pa_optcomp.stream_filter (fun x -> x)
         (filter_sharp
            (filter
               (Gram.lex Loc.ghost
                  (Stream.of_channel ic)))));
    close_in ic
  with
      Exit -> ()
