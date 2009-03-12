(*
 * optcomp.ml
 * ----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of optcomp.
 *)

(* Standalone version *)

open Camlp4.PreCast
open Camlp4.Sig

let print_token ?(oc=stdout) = function
  | KEYWORD kwd ->
      Printf.fprintf oc "%s" kwd
  | SYMBOL sym ->
      Printf.fprintf oc "%s" sym
  | LIDENT lid ->
      Printf.fprintf oc "%s" lid
  | UIDENT uid ->
      Printf.fprintf oc "%s" uid
  | ESCAPED_IDENT id ->
      Printf.fprintf oc "%s" "( ";
      Printf.fprintf oc "%s" id;
      Printf.fprintf oc "%s" " )"
  | INT(_, s) ->
      Printf.fprintf oc "%s" s
  | INT32(_, s) ->
      Printf.fprintf oc "%s" s;
      Printf.fprintf oc "%c" 'l'
  | INT64(_, s) ->
      Printf.fprintf oc "%s" s;
      Printf.fprintf oc "%c" 'L'
  | NATIVEINT(_, s) ->
      Printf.fprintf oc "%s" s;
      Printf.fprintf oc "%c" 'n'
  | FLOAT(_, s) ->
      Printf.fprintf oc "%s" s
  | CHAR(_, s) ->
      Printf.fprintf oc "%s" s
  | STRING(_, s) ->
      Printf.fprintf oc "%c" '"';
      Printf.fprintf oc "%s" s;
      Printf.fprintf oc "%c" '"'
  | LABEL lbl ->
      Printf.fprintf oc "%c" '~';
      Printf.fprintf oc "%s" lbl;
      Printf.fprintf oc "%c" ':'
  | OPTLABEL lbl ->
      Printf.fprintf oc "%c" '?';
      Printf.fprintf oc "%s" lbl;
      Printf.fprintf oc "%c" ':'
  | QUOTATION quot ->
      if quot.q_name = "" then
        Printf.fprintf oc "%s" "<<"
      else begin
        Printf.fprintf oc "%s" "<:";
        Printf.fprintf oc "%s" quot.q_name;
        if quot.q_loc <> "" then begin
          Printf.fprintf oc "%c" '@';
          Printf.fprintf oc "%s" quot.q_loc
        end;
        Printf.fprintf oc "%c" '<'
      end;
      Printf.fprintf oc "%s" quot.q_contents;
      Printf.fprintf oc "%s" ">>"
  | ANTIQUOT(n, s) ->
      Printf.fprintf oc "%c" '$';
      if n <> "" then begin
        Printf.fprintf oc "%s" n;
        Printf.fprintf oc "%c" ':'
      end;
      Printf.fprintf oc "%s" s;
      Printf.fprintf oc "%c" '$'
  | COMMENT comment ->
      Printf.fprintf oc "%s" comment
  | BLANKS s ->
      Printf.fprintf oc "%s" s
  | NEWLINE ->
      Printf.fprintf oc "\n"
  | LINE_DIRECTIVE(n, fname_opt) ->
      Printf.fprintf oc "# %d" n;
      begin match fname_opt with
        | Some fname ->
            Printf.fprintf oc " \"%s\"\n" fname
        | None ->
            Printf.fprintf oc "\n"
      end
  | EOI ->
      raise Exit

let filter_keywords stream =
  Stream.from (fun _ -> match Stream.next stream with
                 | (SYMBOL ("#"|"="|"("|")"|"{"|"}"|"["|"]" as sym), loc) -> Some(KEYWORD sym, loc)
                 | x -> Some x)

external filter : 'a Gram.not_filtered -> 'a = "%identity"

let main () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "usage: %s <file>\n%!" (Filename.basename Sys.argv.(0));
    exit 2
  end;
  try
    let fname = Sys.argv.(1) in
    let ic = open_in fname in
    Stream.iter
      (fun (tok, loc) -> print_token tok)
      (Pa_optcomp.stream_filter (fun x -> x)
         (filter_keywords
            (filter
               (Gram.lex (Loc.mk fname)
                  (Stream.of_channel ic)))));
    close_in ic
  with
    | Exit -> ()
    | exn ->
        Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exn
