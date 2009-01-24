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

  (* We include Camlp4ListComprehension,
     in order to reuse test_patt_lessminus *)
     
  (* First, an ugly hack : upon loading, Camlp4ListComprehension will
     delete a specific rule wich is not always present in the initial
     grammar. We add it now to avoid a Not_found failure during
     deletion *)
  EXTEND Gram
    expr: [[ "["; sem_expr_for_list; "]" -> assert False ]];
  END;
  
  module ListComprehension = Camlp4ListComprehension.Make Syntax;
  
  value test_patt_lessminus = ListComprehension.test_patt_lessminus;

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
          then apply_guards m p guards (map _loc m p expr gen)
          else
            let unfiltered = map _loc enum p expr (to_enum m' gen) in
            of_enum m (apply_guards enum p guards unfiltered)
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
    build modu expr [] comp_items;


  (* proper syntax extension code *)
  value comp_item = Gram.Entry.mk "comprehension item";

  EXTEND Gram
    expr: LEVEL "simple"
    [[ "["; "?"; m = OPT [ m = module_longident; ":" -> m ];
       e = expr; "|"; comp = LIST1 comp_item SEP ";"; "]" ->
         let m = match m with [ None -> <:ident< Enum >> | Some m -> m ] in
         compr _loc m e comp ]];
    
    comp_item:
      [[ test_patt_lessminus; p = patt; "<-";
         m = OPT [ m = module_longident; ":" -> m ];
         gen = expr LEVEL "top" -> Gen (m, p, gen)
       | guard = expr LEVEL "top" -> Guard guard ]];
    END;
end;

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ();
