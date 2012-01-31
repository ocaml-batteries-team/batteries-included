(*
 * qTest: quick unit tests: extract oUnit tests from OCaml components
 * under GNU GPL v3: see qtest.mll
 *)

(**** TOOLKIT ****)
let fpf = Printf.fprintf let va  = Printf.sprintf
let epf = Printf.eprintf let eps  = prerr_string
let pl  = print_endline
(** Toggle an option on and off *)
let toggle br = br := not !br
(** Apply a lexing rule so long as it can be applied.
  Avoids having to do a bloody recursive call at the end
  of each action...
  The rule receives a function terminator() as its last
  argument; using it lets the rule signify it is done. *)
let exhaust_lexer rule lex =
  let continue = ref true in let term () = toggle continue in
  while !continue do rule term lex done
(** Push a value as head of a list reference *)
let push x l = l := x :: !l
(** Function application operator (read "of"). This is used to avoid LISP-like
parentheses creep; the point of using an [@...] operator is that it has just the
right precedence and is right-associative *)
let (@@) f x = f x
let soi = string_of_int
(** Convert a [char] to a [string] *)
let soc  = String.make 1
(** Trim white spaces  *)
let is_blank_char = function ' '|'\t'|'\n'|'\r' -> true | _ -> false 
let rec trim s =
  let l = String.length s in if l=0 then s
  else if is_blank_char s.[0]   then trim (String.sub s 1 (l-1))
  else if is_blank_char s.[l-1] then trim (String.sub s 0 (l-1))
  else s
let listiteri f l = let c = ref (-1) in let call x = incr c; f !c x in
  List.iter call l
(*****************)
 
(** output channel *)
let outc = ref stdout
(** main output function *)
let out s = output_string !outc s
(** formatted output function *)
let outf x = fpf !outc x

(** output for targets list *)
let outc_target = ref (open_out "qtest.targets.log")
(** formatted output for targets list *)
let outf_target s = fpf !outc_target s

(** indispensable preamble *)
let hard_coded_preamble = "open OUnit;;
module Q = Quickcheck;;let ( ==> ) = Q.( ==> );;
Random.self_init()\n\n"

(** global preamble, user-definable *)
let global_preamble = ref ""

(** update the buffer to reflect current line number *)
let eol lexbuf = Lexing.(
  let curr = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { curr with
    pos_lnum = curr.pos_lnum + 1 ;
    pos_bol = curr.pos_cnum
  })

let exhaust = exhaust_lexer

(** a simple statement, such as "foo 2 = 4", with line number *)
type statement = { ln : int ; code : string }

(** foo as bar *)
type binding = string * string
(** foo, bar as baz *)
type metabinding = string list * string
(** additional testing parameters: caml code for oUnit *)
type param = string
(** instanciable header: just a list of bindings *)
type header = {
  hb : binding list;
  hpar : param;
}
(** metabindings need to be instanciated before the test can *)
type metaheader = {
  mhb: metabinding list;
  mhpar : param;
}

(** what kind of test are we talking about ? *)
type kind =
| Simple    (* statement is asserted to be true *)
| Random    (* statement is tested on random inputs, using Quickcheck *)
| Raw       (* raw oUnit statement *)
| Equal     (* Equality statement *)

(** a test : several statements *)
type 'a test = {
  header : 'a ; (* bindings or metabindings *)
  kind : kind ; 
  line : int ;  (* header line number *)
  source : string ; (* original source file *)
  statements : statement list ; (* test code *)
}

(** what kind of instruction are we talking about ? *)
type pragma =
| Meta_test of metaheader test  (* describes one or several tests *)
| Test of header test           (* do some testing... *)
| Env_begin       (* open a test environment, eg. a module or file *)
| Env_close       (* ... and close it *)
| Open of string  (* open a module, within the scope of current environment *)

(** storage facility for all tests in input files *)
let suite : pragma list ref = ref []
(** add a pragma to the current suite *)
let register prag = push prag suite



(** if a test header contains invalid characters *)
exception Bad_header_char of string * string

(** if a test's body is never closed *)
exception Unterminated_test of statement list

(** a test contains no statement *)
exception Empty_test of string

(** human-readable form *)
let str_of_metabinding (bind:metabinding) =
  let targets,alias = bind in String.concat ", " targets ^
  if List.length targets > 1 || List.hd targets <> alias then " as " ^ alias else ""
    
let str_of_binding ((f,a):binding) = str_of_metabinding ([f],a)

(** lexical closure generation, single binding *)
let code_of_binding ((f,a):binding) = va "let %s = %s in" a f
(** same, for a list of bindings, ie. a test's header *)
let code_of_bindings bl = String.concat " " (List.map code_of_binding bl)

(** get the functions targeted by a header *)
let targets_of_header (hd:header) = match hd.hb with
  | [] -> ["&empty&"]
  |  l -> let x,_ = List.split l in x

(** get an informal "foo, bar as x; a,b as y" string which
  summarises the metatest *)
let get_metatest_name (test : metaheader test) =
  String.concat "; " (List.map str_of_metabinding test.header.mhb)

(** get an informal "foo, bar as x; a,b as y" string which summarises the test *)
let get_test_name (test: header test) = String.concat "; " (List.map str_of_binding test.header.hb)

(** explode a metaheader into the correponding headers *)
let headers_of_metaheader (mh:metaheader) =
  let rec z = function [] -> assert false
  | [(foos,x)] -> List.map (fun foo -> [foo,x]) foos
  | (foos,x) :: mbs -> let rest = z mbs in
    let combine foo = List.map (fun others ->(foo,x) :: others) rest in
    List.concat @@ List.map combine foos
  in match mh.mhb with
  | [] ->  [{hb = []; hpar = mh.mhpar}]
  | l -> ((List.map (fun b-> {hb = b; hpar = mh.mhpar}) (z l)) : header list)
  
(** break down metatests (tests w/ multiple targets) and enforce that each
  test is non-empty, ie. has at least one statement.
  Also, put the statements back in the order they appear in *)
let preprocess pragmas = let rec z = function [] -> []
| Meta_test test :: l ->
  if test.statements = [] then raise @@ Empty_test (get_metatest_name test);
  let test = {test with
    statements = List.filter (fun s->s.code <> "") test.statements}
  in List.map (fun hd -> Test {test with header = hd})
    (headers_of_metaheader test.header) @ z l
| x :: l -> x :: z l  (* leave non-metatest pragmas untouched *)
in z pragmas

(** get the name of the test function, given its uid *)
let test_handle_of_uid uid = "_test_" ^ soi uid

(** execute a pragma; in particular, output the executable version of a test *)
let process uid = function
  | Test test ->
    let test_handle = test_handle_of_uid uid in
    let targets = targets_of_header test.header in
    List.iter (fun t->
      outf_target "%30s %4d    %s\n" test.source test.line t
    ) targets;
    outf "let %s = %S >::: [\n" test_handle (get_test_name test);
    (* handle individual statements *)
    let do_statement st = 
      let location = va "%s:%d" test.source st.ln in
      let extended_name = va "\"%s\"" (* pretty, detailed name for the test *)
        (String.escaped location^":  "^String.escaped st.code)
      and bind = code_of_bindings test.header.hb
      in match test.kind with
      | Simple -> outf "#%d \"%s\"\n \"%s\" >::
        (%s fun () -> OUnit.assert_bool %s (%s));\n"
        st.ln test.source location bind extended_name st.code;
      | Equal -> outf "#%d \"%s\"\n \"%s\" >::
        (%s fun () -> OUnit.assert_equal ~msg:%s %s %s);\n"
        st.ln test.source location bind extended_name test.header.hpar st.code;
      | Random -> outf "#%d \"%s\"\n \"%s\" >::
        (%s fun () -> Q.laws_exn %s %s);\n"
        st.ln test.source location bind extended_name st.code;
      | Raw -> outf "#%d \"%s\"\n \"%s\" >::
        (%s fun () -> (%s));\n"
        st.ln test.source location bind st.code;
    in List.iter do_statement test.statements;
    outf "];; let _ = ___add %s;;\n" test_handle;
  | Env_begin -> outf  "\n\nmodule Test__environment_%d = struct\n" uid
  | Env_close -> out  "end\n\n"
  | Open modu -> outf "open %s;;\n" modu
  | Meta_test _ -> assert false (* metas should have been pre-processed out *)


  