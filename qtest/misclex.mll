{
(*
 * qTest: quick unit tests: extract oUnit tests from OCaml components
 * under GNU GPL v3: see qtest.mll
 *)
module B = Buffer;;
let b = B.create 80
} (***************************************************************)

let blank = [' ' '\t']

(* remove surrounding whitespace *)
rule trim = parse blank* (_* as x) blank* eof { x }

(* collapse multiple spaces into one *)
and normalise = parse
| blank+ { B.add_char b ' '; normalise lexbuf }
| _ as c { B.add_char b  c ; normalise lexbuf }
| eof { let s = B.contents b in B.clear b ; s }

