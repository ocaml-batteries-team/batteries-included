(*
 * make_suite -- extract oUnit tests from OCaml components and print them to stdout
 * Copyright (C) 2011  Batteries Included Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

{
  open Lexing

  let eol lexbuf =
    let curr = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { curr with
                             pos_lnum = curr.pos_lnum + 1 ;
                             pos_bol = curr.pos_cnum }

  let withbuf lexfn lb =
    let buf = Buffer.create 19 in
    lexfn buf lb

  let wspre = Str.regexp "[ \t]*\\(.*\\)[ \t]*"

  let strip s =
    ignore (Str.string_match wspre s 0) ;
    Str.matched_group 1 s

  let sanitize s =
    (* I want to be able to use "String.foo" as a test name; currently
       it doesn't work because it gets included in the test
       identifier, `let __test_String.foo = ...`, which raises
       a Syntax error *)
    let s = String.copy s in
    for i = 0 to String.length s - 1 do
      s.[i] <- begin match s.[i] with
        | 'a'..'z' | '_' | '0'..'9' -> s.[i]
        | 'A'..'Z' -> Char.lowercase s.[i]
        | _ -> '_'
      end;
    done;
    s


let all_tests : string list ref = ref []

let handle_test mn (k, str, pos) =
  let lines = Str.split (Str.regexp "\n") str in
  match lines with
    | name :: tests -> begin
      let name = match strip name with
        | "" -> Printf.sprintf "%s_%d" mn pos.pos_lnum
        | nm -> nm
      in
      let test_name = "__test_" ^ sanitize name in
      all_tests := test_name :: !all_tests ;
      let rec number tests k = function
        | [] -> List.rev tests
        | t :: ts ->
          let tests = match strip t with
            | "" -> tests
            | t -> (k, t) :: tests
          in
          number tests (k + 1) ts
      in
      Printf.printf "let %s = %S >::: [\n" test_name name ; 
      Printf.printf "\"label\" >:: (fun () -> print_string %S) ;\n" name;
      begin
        match k with
          | `Q ->
            let tests = number [] (pos.pos_lnum + 1) tests in
            List.iter begin fun (k, t) ->
              Printf.printf
		"#%d \"%s\"\n\"%s:%d\" >:: (fun () -> Q.laws_exn %S %s) ;\n" k pos.pos_fname pos.pos_fname k t t
            end tests
          | `T ->
            let tests = number [] (pos.pos_lnum + 1) tests in
            List.iter begin fun (k, t) ->
              Printf.printf
		"#%d \"%s\"\n\"%s:%d\" >:: (fun () -> OUnit.assert_bool \"%s:%d\" (%s)) ;\n" k pos.pos_fname pos.pos_fname k pos.pos_fname k t
            end tests
          | `S ->
            let tests = String.concat "\n" tests in
	    Printf.printf "#%d \"%s\"\n\"%s:%d\" >:: (fun () -> %s) ;\n" pos.pos_lnum pos.pos_fname pos.pos_fname pos.pos_lnum tests
      end ; 
      Printf.printf "] ;;\n%!"
    end
    | _ ->
      failwith "handle_test"
}

let newline  = ('\r' | '\n' | "\r\n")

rule scan = parse
  | newline { eol lexbuf ; scan lexbuf }
  | "(**Q " { withbuf (fun buf ->
                         Buffer.add_char buf ' ' ;
                         pragma `Q lexbuf.lex_start_p buf) lexbuf }
  | "(**T " { withbuf (fun buf ->
                         Buffer.add_char buf ' ' ;
                         pragma `T lexbuf.lex_start_p buf) lexbuf }
  | "(*** " { withbuf (fun buf ->
                         Buffer.add_char buf ' ' ;
                         pragma `S lexbuf.lex_start_p buf) lexbuf }
  | "(*" { withbuf (comment 1) lexbuf ; scan lexbuf }
  | _ { scan lexbuf }

and comment dep buf = parse
  | newline { eol lexbuf ;
              Buffer.add_char buf '\n' ;
              comment dep buf lexbuf }
  | "(*" {
      Buffer.add_string buf "(*" ;
      comment (dep + 1) buf lexbuf
    }
  | "*)" {
      Buffer.add_string buf "*)" ;
      if dep = 1 then ()
      else comment (dep - 1) buf lexbuf
    }
  | _ as c { Buffer.add_char buf c ; comment dep buf lexbuf }

and pragma k start buf = parse
  | newline { eol lexbuf ;
              Buffer.add_char buf '\n' ;
              pragma k start buf lexbuf }
  | "(*" { withbuf (comment 1) lexbuf ;
           pragma k start buf lexbuf }
  | "**)" { (k, Buffer.contents buf, start) }
  | _ as c { Buffer.add_char buf c ; pragma k start buf lexbuf }

{

  let process fn =
    let fn_base = Filename.basename fn in
    if not (Filename.check_suffix fn_base ".ml") then begin
      Printf.eprintf "Filename %S does not end in .ml\n%!" fn ;
      Pervasives.exit 2
    end ;
    let mn = String.capitalize (Filename.chop_suffix fn_base ".ml") in
    let ich = Pervasives.open_in fn in
    let lb = Lexing.from_channel ich in
    lb.lex_curr_p <- { pos_fname = fn
                     ; pos_cnum  = 0
                     ; pos_lnum  = 1
                     ; pos_bol   = 0 } ;
    let rec spin () = match (try Some (scan lb) with _ -> None) with
      | None -> ()
      | Some prag -> handle_test mn prag ; spin ()
    in
    List.iter (Printf.printf "%s\n") [
      "open OUnit;;" ;
      "module Q = Quickcheck;;" ;
      "let ( ==> ) = Q.( ==> );;" ;
      "open Batteries;;" ;
      "open " ^ mn ^ ";;" ;
    ] ;
    spin () ;
    List.iter (Printf.printf "%s\n") [
      "let suite = \"" ^ mn ^ "\" >::: [" ;
      String.concat ";" !all_tests ;
      "];;" ;
      "let () = Tests.register suite;;"
    ] ;
    Pervasives.flush stdout

  let () =
    if Array.length Sys.argv != 2 then failwith "make_suite: Missing filename to process" ;
    process Sys.argv.(1)

}
