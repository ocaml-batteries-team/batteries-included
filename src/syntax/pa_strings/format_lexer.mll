(*
 * format_lexer.mll
 * ----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

{
  open Format_ast
}

let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']

let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let name =
  (identstart identbody* | "_")

let directive =
  identbody+

rule main = parse
  | [^'%'] as x { Cst(String.make 1 x) :: main lexbuf }
  | "%%" { Cst "%" :: main lexbuf }
  | "%!" { Cst "%!" :: main lexbuf }
  | "%(" { let names, dir = long_directive lexbuf in Dir(names, dir) :: main lexbuf }
  | "%" (['a'-'z' 'A'-'Z']+ as dir) { Dir([], dir) :: main lexbuf }
  | eof { [] }

and long_directive = parse
  | (directive as dir) ")" { ([], dir) }
  | (name as name) "," { let names, dir = long_directive_with_names lexbuf in
                         (name :: names, dir) }
  | (name as name) ":" (directive as dir) ")" { ([name], dir) }

and long_directive_with_names = parse
  | (name as name) "," { let names, dir = long_directive_with_names lexbuf in
                         (name :: names, dir) }
  | (name as name) ":" (directive as dir) ")" { ([name], dir) }
