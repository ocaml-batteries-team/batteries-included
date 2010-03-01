(*
 * Pa_comprehension -- Syntax extension for comprehensions expressions
 * Copyright (C)   2007 Nicolas Pouillard
 *                 2008 David Teller
 *                 2008 Gabriel Scherer
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

open Camlp4;

module Id : Sig.Id = struct
  value name = "pa_comprehension";
  value version = "0.4";
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;


  (* "[?" and "?]" are not recognized as delimiters by the Camlp4
     lexer; This token parser will spot "["; "?" and "?"; "]" token
     and insert "[?" and "?]" instead.
     
     Thanks to Jérémie Dimino for the idea. *)
  value rec delim_filter older_filter stream = 
    let rec filter = parser 
    [ [: `(KEYWORD "[", loc); rest :] ->
        match rest with parser
        [ [: `(KEYWORD "?", _) :] -> [: `(KEYWORD "[?", loc); filter rest :]
        | [: :] -> [: `(KEYWORD "[", loc); filter rest :] ]
    | [: `(KEYWORD "?", loc); rest :] ->
        match rest with parser
        [ [: `(KEYWORD "]", loc) :] -> [: `(KEYWORD "?]", loc); filter rest :]
        | [: :] -> [: `(KEYWORD "?", loc); filter rest :] ]
    | [: `other; rest :] -> [: `other; filter rest :] ] in
    older_filter (filter stream);

  value _ = Token.Filter.define_filter (Gram.get_filter ()) delim_filter;

  value rec loop n =
    fun
    [ [] -> None
    | [(x, _)] -> if n = 0 then Some x else None
    | [_ :: l] -> loop (n - 1) l ];

  value stream_peek_nth n strm = loop n (Stream.npeek (n+1) strm);

  (* copied from Camlp4ListComprehension *)
  value test_patt_lessminus =
    Gram.Entry.of_parser "test_patt_lessminus"
      (fun strm ->
        let rec skip_patt n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD "<-") -> n
          | Some (KEYWORD ("[" | "[<")) ->
              skip_patt (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") -> 
              skip_patt (ignore_upto ")" (n + 1) + 1)
          | Some (KEYWORD "{") -> 
              skip_patt (ignore_upto "}" (n + 1) + 1)
          | Some (KEYWORD ("as" | "::" | "," | "_"))
          | Some (LIDENT _ | UIDENT _) -> skip_patt (n + 1)
          | Some _ | None -> raise Stream.Failure ]
        and ignore_upto end_kwd n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD prm) when prm = end_kwd -> n 
          | Some (KEYWORD ("[" | "[<")) ->
              ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") ->
              ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1)
          | Some (KEYWORD "{") -> 
              ignore_upto end_kwd (ignore_upto "}" (n + 1) + 1)        
          | None | Some EOI -> raise Stream.Failure
          | Some _ -> ignore_upto end_kwd (n + 1) ]
        in
        skip_patt 0);

  value test_custom_module =
    Gram.Entry.of_parser "test_comprehension_custom_module"
      (fun strm ->
         let rec after_longident n =
           match stream_peek_nth n strm with
           [ Some (UIDENT _) ->         
               match stream_peek_nth (n+1) strm with
               [ Some (KEYWORD ".") ->
                   let n' = after_longident (n+2) in
                   (* if n = n + 2, the last longident token is the '.' : Failure *)
                   if n' > n+2 then n' else raise Stream.Failure
               | _ -> n+1 ]
           | _ -> n ]
         in
      match after_longident 0 with
      [ 0 -> raise Stream.Failure
      | n ->
          match stream_peek_nth n strm with
          [ Some (KEYWORD ":") -> ()
          | _ -> raise Stream.Failure ]]); 

  (* map, filter, concat are generalized version of
     Camlp4ListComprehension, abstracted over the module name *)
  value map _loc m p e l =
    match (p, e) with
    [ (<:patt< $lid:x$ >>, <:expr< $lid:y$ >>) when x = y -> l
    | _ -> 
        if Ast.is_irrefut_patt p then
          <:expr< $id:m$.map (fun $p$ -> $e$) $l$ >>
        else
          <:expr< $id:m$.filter_map (fun [ $p$ -> Some $e$ | _ -> None ] ) $l$ >> ];

  value filter _loc m p b l =
    if Ast.is_irrefut_patt p then
      <:expr< $id:m$.filter (fun $p$ -> $b$) $l$ >>
    else
      <:expr< $id:m$.filter (fun [ $p$ when True -> $b$ | _ -> False ]) $l$ >>;

  value concat _loc m l = <:expr< $id:m$.concat $l$ >>;


  (** An item of a data structure comprehension *)
  type comprehension_item =
    [ Guard of Ast.expr
    | Gen of option Ast.ident and Ast.patt and Ast.expr ];

  value rec eq_ident a b =
      match (a, b) with
        [ ( <:ident< $a$ $b$ >>, <:ident< $a'$ $b'$ >> )
        | ( <:ident< $a$.$b$ >>, <:ident< $a'$.$b'$ >> )
          -> eq_ident a a' && eq_ident b b'
        | ( <:ident< $lid:a$ >>, <:ident< $lid:a'$ >> )
        | ( <:ident< $uid:a$ >>, <:ident< $uid:a'$ >> )
          -> a = a'
        | _ -> False ];
        
  (* comprehension building function :
     
     comprehension may use numerous data structures modules, eg.
     [? List : (a,b) | a <- List : foo; b <- Array : bar ]
     
     When different modules are used, the "lingua franca" is enum :
     the input module (here Array) is converted to Enum, and the
     enumeration is then converted back into the output module (here
     the second List module). All comprehension operations in the
     between (map, filter, concat) are performed on enumerations (it
     is assumed that they are generally more efficient than the unkown
     data structure modules, when the structure of the data need not
     to be preserved).
     
     In the special case were the input module and output module are
     the same (here the second List and the first List module), we use
     the internal map/filter/filter_map/concat operations. That means
     that the user should not use the "Module : value" syntax with
     a module wich doesn't support map, filter and concat; She can
     still use "Module.enum value" (in generator position) or
     "Module.of_enum [? ... ]" (in output position) instead

     Guards are accumulated and combined in one filter with "&&"
     instead of several filters *)
  value compr _loc modu expr comp_items =
    let enum = <:ident< Enum >> in
    let (to_enum, of_enum) =
      let wrapper str = fun m e ->
        if eq_ident enum m then e
        else <:expr< $id:m$.$lid:str$ $e$ >> in
      (wrapper "enum", wrapper "of_enum") in
    let get = fun [ None -> enum | Some m -> m ] in
    let apply_guards m p gs e =
      match gs with
      [ [] -> e
      | [hd::tl] ->
        let g = List.fold_left (fun e g -> <:expr< $g$ && $e$ >> ) hd tl in  
        filter _loc m p g e ] in
    let rec build m expr guards = fun
      [ [Gen m' p gen] -> (* final output, last generator : map *)
          let m' = get m' in
          if eq_ident m m'
          then map _loc m p expr (apply_guards m p guards gen)
          else
            let filtered = apply_guards enum p guards (to_enum m' gen) in
            of_enum m (map _loc enum p expr filtered)
      | [ (Gen _ _ _ as gen) ; Guard g :: tail ] ->
          build m expr [g :: guards] [gen :: tail]
      | [ (Gen m' p gen) :: tail ] -> (* middle generator (map + concat) *)
          let m' = get m' in
          if eq_ident m m'
          then concat _loc m (map _loc m p (build m expr [] tail) gen)
          else
            let filtered = apply_guards enum p guards (to_enum m' gen) in
            let product = map _loc enum p (build enum expr [] tail) filtered in
            of_enum m (concat _loc enum product)
      | _ -> raise Stream.Failure ] in
    build (get modu) expr [] comp_items;


  (* proper syntax extension code *)
  value comp_item = Gram.Entry.mk "comprehension item";
  value comp_expr = Gram.Entry.mk "comp_expr";

  EXTEND Gram
    expr: LEVEL "simple"
    [[ "[?"; (m, output) = comp_expr; "|"; comp = LIST1 comp_item SEP ";"; "?]" ->
         compr _loc m output comp ]];
    
    comp_item:
      [[ test_patt_lessminus; p = patt; "<-"; (m, gen) = comp_expr -> Gen (m, p, gen)
       | guard = expr LEVEL "top" -> Guard guard ]];

    comp_expr:
      [[ test_custom_module; m = module_longident; ":"; e = expr LEVEL "top" -> (Some m, e)
       | e = expr LEVEL "top" -> (None, e) ]];
    END;
end;

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ();
