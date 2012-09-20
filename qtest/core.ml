(*
 * qTest: quick unit tests: extract oUnit tests from OCaml components
 * under GNU GPL v3: see qtest.mll
 *)

(**** TOOLKIT ****)
module M = Misclex;;
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
let is_blank_char = function ' '|'\t'|'\n'|'\r' -> true | _ -> false 
let listiteri f l = let c = ref (-1) in let call x = incr c; f !c x in
  List.iter call l
let lex_str lexer s = (* use an ocamllex lexer *)
  let buff = Lexing.from_string s in lexer buff
let trim = lex_str M.trim
and normalise = lex_str M.normalise
let snippet s n =
  if String.length s <= n then s else Str.first_chars s n ^ "..."
let snip lex = (** Snippet of current lexer buffer context *)
  let curr = max 0 (lex.Lexing.lex_start_pos - 5) in
  let vicinity = Str.string_after lex.Lexing.lex_buffer curr
  in snippet vicinity 70
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
let hard_coded_preamble = "open OUnit;;\n\
module Q = Quickcheck;;let ( ==> ) = Q.( ==> );;\n\
Random.self_init()\n\n"

(** global preamble, user-definable *)
let global_preamble = Buffer.create 100

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
(** instantiable header: just a list of bindings *)
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
| Env_begin             (* open a test environment, eg. a module or file *)
| Env_close             (* ... and close it *)
| Open of string        (* open a module, within the scope of current environment *)
| Inject of (string * int) * string (*= (foo.ml ln) code *)
                        (* inject code into test environment *)

(** storage facility for all tests in input files *)
let suite : pragma list ref = ref []
(** add a pragma to the current suite *)
let register prag = push prag suite



(** if a test header contains invalid characters *)
exception Bad_header_char of string * string
(** if a test body is never closed *)
exception Unterminated_test of statement list
(** a test contains no statement *)
exception Empty_test of string
(** an "open modules" pragma is invalid at lexing level *)
exception Bad_modules_open_char of string
(** ... or at parsing level *)
exception Modules_syntax_error
(** this looks like a qtest pragma, but isn't *)
exception Invalid_pragma of string

(** human-readable form *)
let str_of_metabinding (bind:metabinding) =
  let targets,alias = bind in String.concat ", " targets ^
  if List.length targets > 1 || List.hd targets <> alias then " as " ^ alias else ""
    
let str_of_binding ((f,a):binding) = str_of_metabinding ([f],a)

(** lexical closure generation, single binding *)
let code_of_binding ((f,a):binding) = va "let %s = %s in" a f
(** same, for a list of bindings, ie. a test header *)
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

(** get a pretty, user-friendly version of the code wrt. whitespace *)
let prettify s =
  if String.contains s '\n'
  then (* multi-line : as-is     *)   "\n\n" ^ s ^ "\n\n"
  else (* single-line: normalise *)   trim (normalise s)

(** filter tests so that only those which involve a specific
  function name are kept. Option --run-only *)
let _run_only = ref None
let retain_test test = match !_run_only with
  | None -> true | Some pattern ->
    let targets,_ = List.split test.header.hb in
    List.mem pattern targets

(** execute a pragma; in particular, output the executable version of a test *)
let process uid = function
  | Test test when retain_test test  ->
    let test_handle = test_handle_of_uid uid 
    and targets = targets_of_header test.header in
    List.iter (fun t->
      outf_target "%30s %4d    %s\n" test.source test.line t
    ) targets;
    outf "let %s = %S >::: [\n" test_handle (get_test_name test);
    (* handle individual statements *)
    let do_statement st = 
      let location = va "%s:%d" test.source st.ln in
      let extended_name = va "\"%s\"" (* pretty, detailed name for the test *)
        (String.escaped location^":  "^String.escaped (prettify st.code)) 
      and lnumdir = va "\n#%d \"%s\"\n" st.ln test.source in
      let bind = lnumdir ^ code_of_bindings test.header.hb 
      in match test.kind with
      | Simple -> outf
        "\"%s\" >:: (%s fun () -> OUnit.assert_bool %s (%s%s%s));\n"
        location bind extended_name test.header.hpar lnumdir st.code;
      | Equal -> outf
        "\"%s\" >:: (%s fun () -> OUnit.assert_equal ~msg:%s %s %s%s);\n"
        location bind extended_name test.header.hpar lnumdir st.code;
      | Random -> outf
        "\"%s\" >:: (%s fun () -> Q.laws_exn %s %s %s%s);\n"
        location bind extended_name test.header.hpar lnumdir st.code;
      | Raw -> outf
        "\"%s\" >:: (%s fun () -> (%s%s));\n"
        location bind lnumdir st.code;
    in List.iter do_statement test.statements;
    outf "];; let _ = ___add %s;;\n" test_handle
  | Test test -> epf "Skipping `%s'\n" (get_test_name test)
  | Env_begin -> outf  "\n\nmodule Test__environment_%d = struct\n" uid
  | Env_close -> out  "end\n\n"
  | Open modu -> outf "open %s;;\n" modu
  | Inject ((modu,ln),cd) ->
    let lnumdir = va "\n#%d \"%s\"\n" ln modu in
    out @@ lnumdir ^ "    " ^ cd ^ " " (* 4 spaces for column numbers reporting *)
  | Meta_test _ -> assert false (* metas should have been pre-processed out *)


(** Shuffling tests as per --shuffle *)

module Shuffle = struct

type imbrication =
| Env of imbrication list
| Prg of pragma (* but without Env_begin and close *)

(* turn a raw pragma list into an imbrication *)
let input pl =
  let rec z acc = function
  | [] -> [], List.rev acc
  | Env_close :: l -> l, List.rev acc
  | Env_begin :: l ->
    let rest, result = z [] l in
    z (Env result :: acc) rest
  | p :: l -> z (Prg p :: acc) l
  in let rest, res = z [] pl
  in if (rest <> []) then
  epf "Warning: shuffle has rests: check that every opened module is closed\n";
  Env res

(* turn an imbrication list back into a raw pragma list *)
let output imbl =
  let rec z  = function
  | Prg p -> [p]
  | Env il -> Env_begin :: (List.concat @@ List.map z il) @ [Env_close]
  in z imbl

(* Durstenfeld shuffling algorithm *)
let durstenfeld l = Array.(
  let a = of_list l in
  let ex i j =
    let oldi = a.(i) in
    a.(i) <- a.(j); a.(j) <- oldi
  in
  for k = length a - 1 downto 1 do
    ex k (Random.int (succ k))
  done;
  to_list a
)

let rec shuffle = function
  | Prg p -> Prg p
  | Env (opn::il) -> Env (opn ::
    if List.exists (function Prg(Inject(_,_)) -> true | _ -> false) il
    then List.map shuffle il else List.map shuffle (durstenfeld il))
  | Env _ -> assert false

       
let exec suite =
(*   assert (!suite = output (input !suite)); *)
  suite := output @@ shuffle (input !suite)

end (* Shuffle *)
