(* File: pa_where.ml

   Copyright (C) 2007-
     mfp <mfp@acm.org>
     bluestorm <bluestorm.dylc@gmail.com>

    This program is free software: you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation, either
    version 2 of the License, or (at your option) any later version,
    with the special exception on linking described in the file
    LICENSE.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Library General Public License (file LICENSE) for more details.

   Use :
     Introduce a "where" keyword for backward declarations.

     Generic form :
       <something> where <declaration>  ==>  <declaration> (sep) <something>
     Supported forms : where let, where val
     Examples :
       expr where let a = b     ==>  let a = b in expr
       expr where let rec a = b ==>  let rec a = b in expr
       str_item where val a = b ==>  let a = b ;; str_item 
   
     Default case : as "where let" is the more common form,
     the "let" is optional and you can use "where" alone :
       expr where a = b  ==> let a = b in expr

   Associativity : a where b where c ==> (a where b) where c
   Precedence : let a = b where c and d ==> let a = (b where c and d)

   Example Input :
     let a =
       b c
       where b = d
       where d = e

     where val c = f

   Output :
     let c = f

     let a =
       let d = e in
       let b = d in
     b c

   Compilation :
     ocamlfind ocamlc -syntax camlp4o -package camlp4.extend,camlp4.quotations -c pa_where.ml
   Ocamlfind installation :
     ocamlfind install pa_where META pa_where.cmo pa_where.ml test.ml
   Ocamlfind use :
     ocamlfind ocamlc -syntax camlp4o -package pa_where.syntax ....
*)

open Camlp4
open Sig

module Id = struct
  let name = "pa_where"
  let version = "0.4"
  let description = "'where' backward declarations"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax

  let test_where_let = Gram.Entry.of_parser "test_where_let"
    (fun strm ->
       match Stream.npeek 2 strm with
         [ (KEYWORD "where", _); (KEYWORD ("let" | "rec"), _) ] -> ()
       | [ (KEYWORD "where", _); (KEYWORD _, _) ] -> 
	   raise Stream.Failure
       | [ (KEYWORD "where", _); _ ] -> ()
       | _ -> 
	     raise Stream.Failure)
       
  EXTEND Gram
    GLOBAL: expr str_item;

    str_item: BEFORE "top"
      [ NONA
          [ e = str_item; "where"; "val"; rf = opt_rec; lb = where_binding ->
              <:str_item< value $rec:rf$ $lb$ ; $e$ >>
          ] ];

(*    expr: BEFORE "top"
      [ NONA
          [ e = expr; test_where_let; "where"; OPT "let";
            rf = opt_rec; lb = where_binding ->
              <:expr< let $rec:rf$ $lb$ in $e$ >>
          ] ];*)

    expr: BEFORE "top"
      [ NONA
          [ e = expr; "where"; OPT "let";
            rf = opt_rec; lb = where_binding ->
              <:expr< let $rec:rf$ $lb$ in $e$ >>
          ] ];

    where_binding:
      [ LEFTA
          [ b1 = SELF; "and"; b2 = SELF -> <:binding< $b1$ and $b2$ >>
          | p = ipatt; e = fun_binding' -> <:binding< $p$ = $e$ >> ] ];
    fun_binding':
      [ RIGHTA
          [ p = labeled_ipatt; e = SELF ->
              <:expr< fun $p$ -> $e$ >>
              | "="; e = expr LEVEL "top" -> <:expr< $e$ >>
              | ":"; t = ctyp; "="; e = expr LEVEL "top" -> <:expr< ($e$ : $t$) >>
              | ":>"; t = ctyp; "="; e = expr LEVEL "top" -> <:expr< ($e$ :> $t$) >> ] ];
    END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
