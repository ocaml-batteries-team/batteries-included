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
let register_mtest lexbuf lexhead lexbod head line kind =
  eol lexbuf;Lexing.(
  register @@ Meta_test { kind; line = succ line;
    header = metaheader_ (lexhead head) (from_string head);
    source = lexbuf.lex_curr_p.pos_fname;
    statements = lexbod lexbuf;
  })
  
} (****************************************************************************)

let blank = [' ' '\t']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let lident = lowercase identchar*
let restline = ([^'\n']* as x) '\n'
let test_header_pat = blank+ restline | (blank* as x) '\n'

(** extract tests from ml file *)
rule lexml t = parse
| "(*$Q" test_header_pat { (* quickcheck (random) test *)
  let line = Lexing.(lexbuf.lex_curr_p.pos_lnum) in
  register_mtest lexbuf lexheader (lexbody  buffy[]) x line Random  }
| "(*$T" test_header_pat { (* simple test *)
  let line = Lexing.(lexbuf.lex_curr_p.pos_lnum) in
  register_mtest lexbuf lexheader (lexbody buffy []) x line Simple }
| "(*$=" test_header_pat { (* equality test *)
  let line = Lexing.(lexbuf.lex_curr_p.pos_lnum) in
  register_mtest lexbuf lexheader (lexbody buffy []) x line Equal }
| "(*$R" test_header_pat { (* raw test *)
  let line = succ Lexing.(lexbuf.lex_curr_p.pos_lnum) in
  register_mtest lexbuf lexheader (lexbody_raw buffy (succ line)) x line Raw }
| "(*$" restline { failwith @@ va "Unrecognised qtest pragma: `%s'" x }
| "(*" (blank | '*')+ "$" [^'\n']* as y
  { epf "\nWarning: likely qtest syntax error: `%s'. " y }
| '\n' { eol lexbuf }
| _ { () }
| eof {t()}

(** body of a test: simply extract lines *)
and lexbody b acc = parse
| blank* "\\\n" blank* { eol lexbuf ; B.add_char b ' '; lexbody b acc lexbuf  }
| [^'\n'] as c { B.add_char b c; lexbody b acc lexbuf }
| '\n' { eol lexbuf; let line = B.contents b in B.clear b;
         lexbody b ({ln = Lexing.(lexbuf.lex_curr_p.pos_lnum) ; code = line} :: acc) lexbuf }
| blank* "*)" { acc }
| ([^'\n']#blank)* blank* '*'+ ")" as x
  { failwith ("runaway test body terminator: " ^ x) }
| eof { raise @@ Unterminated_test acc }

(** body of a raw test... everything until end comment *)
and lexbody_raw b ln = parse
| _ as c {
  if c = '\n' then eol lexbuf;
  B.add_char b c; lexbody_raw b ln lexbuf }
| '\n' blank* "*)" {
  eol lexbuf;
  let s = B.contents b in B.clear b; [{ln; code=s}]}

(** prepare to parse test header *)
and lexheader hd = parse
| blank { lexheader hd lexbuf }
| "," { COMMA }
| ";" { SEMI }
| "as" { AS }
| lident as x { ID x }
| "&"  (_* as x) { PARAM (trim x) }
| eof { EOF }
| _ as x { raise @@ Bad_header_char((soc x), hd) }

(**TODO: deal with strings and nested comments *)

{ (****************************************************************************)

(** register all the tests in source file, and register them in the suite *)
let extract_from pathin = Lexing.(
  epf "`%s' %!" pathin;
  let chanin = open_in pathin in
  let lexbuf = from_channel chanin in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with
    pos_fname = pathin; pos_lnum = 0; (* one behind; pre-incrementation *)
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
  out "let _ = ignore (Runner.run (\"\" >::: !___tests))\n";
  eps "Done.\n"
    
(** Parse command line *)
let rec pcl = function
  | ("-p"|"--preamble") :: code :: l -> global_preamble := code ^ "\n"; pcl l
  | ("-o"|"--output") :: path :: l ->
    epf "Target file: `%s'. " path; outc := open_out path; pcl l
  | "extract" :: paths -> generate paths
  | "--help" :: _ -> welcome ()
  | x::l -> failwith @@ "bad arg: " ^ x
  | [] -> ()

let _ = 
  let commands = List.tl (Array.to_list Sys.argv) in
  if commands = [] then  pl "qtest: use --help for usage notes.";
  pcl commands

}