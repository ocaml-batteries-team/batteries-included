{
(*
 * qtest: quick unit tests: extract oUnit tests from OCaml components
 *
 *
 * Copyright 2012 Vincent Hugot and the "OCaml Batteries Included" team
 *
 *  vhugot.com ; batteries.forge.ocamlcore.org
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *)

open Core;;
open Qparse;;

module B = Buffer;;
(** the do-it-all buffer; always clear *after* use *)
let buffy = B.create 80

(** register a raw metatest from the lexing buffer *)
let register_mtest lexbuf lexhead lexbod line kind =
  let header = metaheader_ lexhead lexbuf in
  let statements = lexbod lexbuf in
  Lexing.(
    register @@ Meta_test { kind; line ; header ;
    source = lexbuf.lex_curr_p.pos_fname; statements ;
  })

let lnumof lexbuf = Lexing.(lexbuf.lex_curr_p.pos_lnum)
let fileof lexbuf = Lexing.(lexbuf.lex_curr_p.pos_fname)
let info lb = fileof lb, lnumof lb
} (****************************************************************************)

let blank = [' ' '\t']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let lident = lowercase identchar* | '(' blank* symbolchar+ blank* ')'
let uident = uppercase identchar*

(** extract tests from ml file *)
rule lexml t = parse
  (* test pragmas *)
  (****************)
| "(*$Q"  { (* quickcheck (random) test *)
  let lnum = lnumof lexbuf in
  register_mtest lexbuf lexheader (lexbody (succ lnum) buffy []) lnum Random  }
| "(*$T"  { (* simple test *)
  let lnum = lnumof lexbuf in
  register_mtest lexbuf lexheader (lexbody (succ lnum) buffy []) lnum Simple }
| "(*$="  { (* equality test *)
  let lnum = lnumof lexbuf in
  register_mtest lexbuf lexheader (lexbody (succ lnum) buffy []) lnum Equal }
| "(*$R"  { (* raw test *)
  let lnum = lnumof lexbuf in
  register_mtest lexbuf lexheader (lexbody_raw (succ lnum) buffy) lnum Raw }
  (* manipulation pragmas *)
  (************************)
| "(*$<"  {
  let global, modules = modules_ lexmodules lexbuf
  and loc_register m = register Env_begin; register @@ Open m
  and glo_register m = register @@ Open m
  in let reg = if global then glo_register else loc_register
  in List.iter reg modules }
| "(*$>*)" { register Env_close }
| "(*${*)" { lexinjectcp buffy lexbuf }
| "(*${"   { lexinjectmv buffy lexbuf }
  (* error cases *)
  (***************)
| "(*$" { raise @@ Invalid_pragma (snip lexbuf) }
| "(*" (blank | '*')+ "$" [^'\n']* as y {
  let f,n = info lexbuf in
  epf "\nWarning: likely qtest syntax error: `%s' at %s:%d. " y f n }
| '\n' { eol lexbuf }
  (* others *)
| _ { () } | eof {t()}

(** body of a test: simply extract lines *)
and lexbody ln b acc = parse
| "\\\n"  { eol lexbuf ; B.add_char b '\n'; lexbody ln b acc lexbuf  }
| [^'\n'] as c { B.add_char b c; lexbody ln b acc lexbuf }
| blank* '\n' {
  eol lexbuf; let code = B.contents b in B.clear b;
  lexbody Lexing.(lexbuf.lex_curr_p.pos_lnum) b ({ln ; code} :: acc) lexbuf }
| blank* "*)" { acc }
| ([^'\n']#blank)* blank* '*'+ ")" as x
  { failwith ("runaway test body terminator: " ^ x) }
| eof { raise @@ Unterminated_test acc }

(** body of a raw test... everything until end comment *)
and lexbody_raw ln b = parse
| _ as c {
  if c = '\n' then eol lexbuf;
  B.add_char b c; lexbody_raw ln b lexbuf }
| '\n' blank* "*)" {
  eol lexbuf;
  let s = B.contents b in B.clear b; [{ln; code=s}]}

(** body of an injection pragma: copy *)
and lexinjectcp b = parse
| _ as c {
  if c = '\n' then eol lexbuf;
  B.add_char b c; lexinjectcp b lexbuf }
| "(*$}*)" {
   let code = B.contents b in B.clear b;
   register @@ Inject (info lexbuf,code) }

(** body of an injection pragma: move *)
and lexinjectmv b = parse
| _ as c {
  if c = '\n' then eol lexbuf;
  B.add_char b c; lexinjectmv b lexbuf }
| "}*)" { (* note: the 2 spaces are for column numbers reporting *)
   let code = "  " ^ B.contents b in B.clear b;
   register @@ Inject (info lexbuf,code) }


(** prepare to parse test header *)
and lexheader = parse
| blank { lexheader lexbuf }
| ";" { SEMI }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "as" { AS }
| "in" { IN }
| "forall" { FORALL }
| lident as x { ID x }
| "&"  ([^'\n']* as x) { PARAM (trim x) }
| '\n' { eol lexbuf; EOF }
| eof  { failwith "unterminated header at end of file" }
| _ as c { raise @@ Bad_header_char((soc c), snip lexbuf) }

(** parse list of modules *)
and lexmodules = parse
| blank { lexmodules lexbuf }
| "," { COMMA }
| "*)"  { EOF  }  (* local open, closed later *)
| ">*)" { EOF2 }  (* global open *)
| uident as x { UID x }
| _ as c { raise @@ Bad_modules_open_char (soc c) }

(**TODO: deal with strings and nested comments *)

{ (****************************************************************************)

(** register all the tests in source file, and register them in the suite *)
let extract_from pathin = Lexing.(
  epf "`%s' %!" pathin;
  let chanin = open_in pathin in
  let lexbuf = from_channel chanin in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with
    pos_fname = pathin; pos_lnum = 1;
  };
  (* getting the module *)
  let mod_name = Filename.(
    let fn_base = basename pathin in
    if not (check_suffix fn_base ".ml" || check_suffix fn_base ".mli") then
      (Printf.eprintf "File %S is not a ML module!\n%!" pathin ; exit 2);
    String.capitalize (chop_extension fn_base)
  ) in
  (* adding the file's pragmas to the suite *)
  register Env_begin; register (Open mod_name);
  exhaust lexml lexbuf; register Env_close;
  close_in chanin
)

(** Show welcome message *)
let welcome() = epf "
** qtest (%s)
USAGE: qtest [options] extract <file.mli?>...

OPTIONS:
--output <file.ml>    (-o) def: standard output
  Open or create a file for output; the resulting file will be an OCaml
  source file containing all the tests.
  
--preamble <string>   (-p) def: empty
  Add code to the tests' preamble; typically this will be an instruction
  of the form 'open Module;;'


--help          Displays this help page and stops
" (Sys.argv.(0))

(** Generate the test suite from files list on currently selected output *)
let generate paths =
  eps "Extraction : "; List.iter extract_from paths;
  out "let ___tests = ref []\nlet ___add test = ___tests := test::!___tests\n";
  out hard_coded_preamble;
  out !global_preamble;
  listiteri process (preprocess @@ List.rev !suite);
  out "let () = ignore (Runner.run (\"\" >::: !___tests))\n";
  eps "Done.\n"

(** Parse command line *)
let rec pcl = function
  | ("-p"|"--preamble") :: code :: l -> global_preamble := code ^ "\n"; pcl l
  | ("-o"|"--output") :: path :: l ->
    epf "Target file: `%s'. " path; outc := open_out path; pcl l
  | "extract" :: paths -> generate paths
  | "--help" :: _ -> welcome ()
  | x :: _ -> failwith @@ "bad arg: " ^ x
  | [] -> ()

let () =
  let commands = List.tl (Array.to_list Sys.argv) in
  if commands = [] then  pl "qtest: use --help for usage notes.";
  pcl commands

}
