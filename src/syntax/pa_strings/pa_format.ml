(*
 * pa_format.ml
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

open Camlp4.PreCast
open Pa_estring

(* Syntax extension for format string *)

#if ocaml_version < (3, 11)
  (*For the moment, we don't have a version compatible with OCaml < 3.11*)
#else

open Format_ast

(* Create the pattern string from the format ast.

   For example:

   make_pattern [Cst "x="; Dir([], "d"); Cst " y="; Dir([], "d")] = "x=%(0) y=%(1)"
*)
let make_pattern ast =
  let rec aux n = function
    | Cst s :: l -> s :: aux n l
    | Dir _ :: l -> "%(" :: string_of_int n :: ")" :: aux (n + 1) l
    | _ -> []
  in
  String.concat "" (aux 0 ast)

(* Returns the expression of a directives, handling labelled
   arguments.

   For example:

   expr_of_directive _loc [] "d" = <:expr< pdir_d >>
   expr_of_directive _loc ["a"; "b"] "s" = <:expr< fun k ~a ~b -> pdir_s k a b >>
*)
let expr_of_directive _loc names dir =
  let expr = <:expr< $lid:"pdir_" ^ dir$ >> in
  match names with
    | [] -> expr
    | _ ->
        let rec make_lidents n = function
          | "_" :: l -> (false, "__" ^ string_of_int n) :: make_lidents (n + 1) l
          | name :: l -> (true, name) :: make_lidents n l
          | [] -> []
        in
        let lids = (false, "__k") :: make_lidents 0 names in
        List.fold_right
          (fun (labeled, id) acc ->
             if labeled then
               <:expr< fun ~ $id$ -> $acc$ >>
             else
               <:expr< fun $lid:id$ -> $acc$ >>)
          lids
          (List.fold_left (fun acc (labeled, id) ->
                             <:expr< $acc$ $lid:id$ >>) expr lids)

(* Builds the expression of a printer from a format ast.

   For example:

   make_printer _loc [Cst "x="; Dir([], "d"); Cst " y="; Dir([], "s")] =
     <:expr<
       pdir_d (fun __printer ->
                 __printers.(0) <-- __printer;
                 pdir_s (fun __printer ->
                           __printers.(1) <-- __printer;
                           __k (fun oc -> Batteries.Printf2.format oc __pattern __printers)))
     >>
*)
let make_printer _loc ast =
  let rec aux n = function
    | Cst _ :: l -> aux n l
    | Dir(names, dir) :: l ->
        let dir = expr_of_directive _loc names dir in
        <:expr< $dir$ (fun __printer ->
                         __printers.($int:string_of_int n$) <- __printer;
                         $aux (n + 1) l$) >>
    | [] ->
        <:expr< __k (fun oc -> Batteries.Printf2.format oc __pattern __printers) >>
  in
  aux 0 ast

(* Lex of format string into a format ast *)
let lex_format loc str =
  try
    Format_lexer.main (Lexing.from_string str)
  with
      exn -> Loc.raise loc exn

let _ =
  register_expr_specifier "p"
    (fun ctx _loc str ->
       let ast = lex_format _loc
         (* Unescape the string before parsing it *)
         (Camlp4.Struct.Token.Eval.string ~strict:() str) in

       (* Count the number of directives *)
       let directive_count = List.length (List.filter (function Cst _ -> false | Dir _ -> true) ast) in

       (* Creates the format expression *)
       <:expr< { Batteries.Printf2.pattern = $str:String.escaped(make_pattern ast)$;
                 Batteries.Printf2.printer = (fun __pattern __k ->
                                                let __printers =
                                                  Array.create
                                                    $int:string_of_int directive_count$
                                                    Pervasives.ignore in
                                                $make_printer _loc ast$) } >>)

#endif
