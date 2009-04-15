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

   Associativity : a where b where c ==> a where (b where c)
   Precedence : let a = b where c and d ==> let a = (b where c and d)

   Example Input :
     let a =
       b c
       where b = d

     where val c = f

   Output :
     let c = f
  
     let a =
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
    (* we don't ask for the 2-deep npeek directly because, in the
       toplevel, it would hang and wait for two more tokens (wich is
       problematic if the first token was ";;" and the user is waiting
       for feedback). We only ask for the second token if the first is
       a "where" *)
    (fun strm ->
       match Stream.peek strm with
       | Some (KEYWORD "where", _) ->
           (match Stream.npeek 2 strm with
            | [ (KEYWORD "where", _); (KEYWORD ("let" | "rec"), _) ] -> ()
            | [ (KEYWORD "where", _); (KEYWORD _, _) ] -> raise Stream.Failure
            | [ (KEYWORD "where", _); _ ] -> ()
            | _ -> raise Stream.Failure)
       | _ -> raise Stream.Failure)
       

  EXTEND Gram
    GLOBAL: expr str_item;

    str_item: BEFORE "top"
      [ NONA
          [ e = str_item; "where"; "val";
            rf = opt_rec; lb = binding ->
              <:str_item< value $rec:rf$ $lb$ ; $e$ >>
          ] ];

    (* the test_where_let is necessary because of the dangling
       str_item/expr case :

       let a = b where val b = 2 *)
    expr: AFTER "top"
      [ "where"
          [ e = SELF; test_where_let; "where"; OPT "let";
            rf = opt_rec; lb = binding ->
              <:expr< let $rec:rf$ $lb$ in $e$ >> ] ];
    END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
