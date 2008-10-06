(*pp camlp4orf *)

(* File: pa_type_conv.ml

    Copyright (C) 2005-

      Jane Street Holding, LLC
      Author: Markus Mottl
      email: mmottl\@janestcapital.com
      WWW: http://www.janestcapital.com/ocaml

   This file is derived from file "pa_tywith.ml" of version 0.45 of the
   library "Tywith".

   Tywith is Copyright (C) 2004, 2005 by

      Martin Sandin  <msandin@hotmail.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* Pa_type_conv: Preprocessing Module for Registering Type Conversions *)

open Printf
open Lexing

open Camlp4
open PreCast
open Ast

(* Utility functions *)

let both fa fb (a, b) = fa a, fb b

let get_loc_err loc msg =
  sprintf "File \"%s\", line %d, characters %d-%d: %s"
    (Loc.file_name loc) (Loc.start_line loc)
    (Loc.start_off loc - Loc.start_bol loc)
    (Loc.stop_off loc - Loc.stop_bol loc)
    msg

let hash_variant str =
  let acc_ref = ref 0 in
  for i = 0 to String.length str - 1 do
    acc_ref := 223 * !acc_ref + Char.code str.[i]
  done;
  if Sys.word_size = 32 then !acc_ref
  else !acc_ref land int_of_string "0x7FFFFFFF"


(* Module/File path management *)

(* Reference storing the path to the currently preprocessed module *)
let conv_path_ref : (string * string list) option ref = ref None

let get_conv_path_el () =
  match !conv_path_ref with
  | None -> failwith "Pa_type_conv: path not set";
  | Some el -> el

(* Get path to the currently preprocessed module *)
let get_conv_path () = fst (get_conv_path_el ())

(* Set path to the currently preprocessed module *)
let set_conv_path conv_path =
  if !conv_path_ref <> None then failwith "Pa_type_conv: module name set twice";
  conv_path_ref := Some (conv_path, [conv_path])

let push_conv_path mod_name =
  let str, rev_lst = get_conv_path_el () in
  conv_path_ref := Some (str ^ "." ^ mod_name, mod_name :: rev_lst)

let pop_conv_path () =
  match get_conv_path_el () with
  | _, _ :: rev_lst ->
      conv_path_ref := Some (String.concat "." (List.rev rev_lst), rev_lst)
  | _ -> assert false  (* impossible *)


(* Generator registration *)

module GeneratorMap = Map.Make(String)

(* Map of "with"-generators *)
let generators = ref GeneratorMap.empty

let sig_generators = ref GeneratorMap.empty

(* Register a "with"-generator *)
let add_generator id e = generators := GeneratorMap.add id e !generators

(* Removes a "with"-generator *)
let rem_generator id = generators := GeneratorMap.remove id !generators

(* Register a "with"-generator to be used in a module signature *)
let add_sig_generator id e =
  sig_generators := GeneratorMap.add id e !sig_generators

(* Removes a "with"-generator *)
let rem_sig_generator id =
  sig_generators := GeneratorMap.remove id !sig_generators


(* General purpose code generation module *)

module Gen = struct
  let rec ty_var_list_of_ctyp tp acc =
    match tp with
    | <:ctyp< $tp1$ $tp2$ >> ->
        ty_var_list_of_ctyp tp1 (ty_var_list_of_ctyp tp2 acc)
    | <:ctyp< '$param$ >> -> param :: acc
    | _ -> invalid_arg "ty_var_list_of_ctyp"

  let rec get_rev_id_path tp acc =
    match tp with
    | <:ident< $id1$ . $id2$ >> -> get_rev_id_path id2 (get_rev_id_path id1 acc)
    | <:ident< $lid:id$ >> | <:ident< $uid:id$ >> -> id :: acc
    | _ -> invalid_arg "get_rev_id_path"

  let mk_ident _loc str =
    let first = str.[0] in
    if first >= 'A' && first <= 'Z' then <:ident< $uid:str$ >>
    else <:ident< $lid:str$ >>

  let rec ident_of_rev_path _loc = function
    | [str] -> mk_ident _loc str
    | str :: strs ->
        <:ident< $ident_of_rev_path _loc strs$ . $mk_ident _loc str$ >>
    | _ -> invalid_arg "ident_of_rev_path"

  let rec get_appl_path _loc = function
    | <:ctyp< $id:id$ >> -> id
    | <:ctyp< $tp$ $_$ >> -> get_appl_path _loc tp
    | _ -> failwith "get_appl_path: unknown type"

  let abstract _loc = List.fold_right (fun p e -> <:expr< fun $p$ -> $e$ >>)
  let apply _loc = List.fold_left (fun f arg -> <:expr< $f$ $arg$ >>)

  let idp _loc id = <:patt< $lid:id$ >>
  let ide _loc id = <:expr< $lid:id$ >>

  let switch_tp_def _loc ~alias ~sum ~record ~variants ~mani ~nil = function
    | <:ctyp< [ $alts$ ] >> -> sum _loc alts
    | <:ctyp< [= $row_fields$ ] >> -> variants _loc row_fields
    | <:ctyp< $id:_$ >>
    | <:ctyp< ( $tup:_$ ) >>
    | <:ctyp< $_$ -> $_$ >>
    | <:ctyp< '$_$ >>
    | <:ctyp< $_$ $_$ >> as tp_def -> alias _loc tp_def
    | <:ctyp< { $flds$ } >> -> record _loc flds
    | <:ctyp< $tp1$ == $tp2$ >> -> mani _loc tp1 tp2
    | <:ctyp< ? >> -> nil _loc
    | _ -> failwith "switch_tp_def: unknown type"

  let rec mk_expr_lst _loc = function
    | [] -> <:expr< [] >>
    | e :: es -> <:expr< [$e$ :: $mk_expr_lst _loc es$] >>

  let rec mk_patt_lst _loc = function
    | [] -> <:patt< [] >>
    | p :: ps -> <:patt< [$p$ :: $mk_patt_lst _loc ps$] >>

  let get_tparam_id = function
    | <:ctyp< '$id$ >> -> id
    | _ -> failwith "get_tparam_id: not a type parameter"

  let type_is_recursive _loc type_name tp =
    let rec loop = function
      | <:ctyp< $tp1$ $tp2$ >>
      | <:ctyp< $tp1$ * $tp2$ >>
      | <:ctyp< $tp1$; $tp2$ >>
      | <:ctyp< $tp1$ -> $tp2$ >>
      | <:ctyp< $tp1$ == $tp2$ >>
      | <:ctyp< $tp1$ and $tp2$ >>
      | <:ctyp< $tp1$ | $tp2$ >> -> loop tp1 || loop tp2
      | <:ctyp< ( $tup:tp$ ) >> | <:ctyp< { $tp$ } >>
      | <:ctyp< [ $tp$ ] >>
      | <:ctyp< $_$ : $tp$ >>
      | <:ctyp< mutable $tp$ >>
      | <:ctyp< $_$ of $tp$ >>
      | <:ctyp< [< $tp$ ] >> | <:ctyp< [> $tp$ ] >> | <:ctyp< [= $tp$ ] >>
      | <:ctyp< ! $_$ . $tp$ >> -> loop tp
      | <:ctyp< $lid:id$ >> -> id = type_name
      | <:ctyp< $id:_$ >>
      | <:ctyp< `$_$ >>
      | <:ctyp< '$_$ >>
      | <:ctyp< ? >> -> false
      | _ ->
          prerr_endline (
            get_loc_err _loc "type_is_recursive: unknown type construct");
          exit 1
    in
    loop tp

  let drop_variance_annotations _loc =
    (map_ctyp (function
      | <:ctyp< +'$var$ >> | <:ctyp< -'$var$ >> -> <:ctyp< '$var$ >>
      | tp -> tp))#ctyp
end


(* Functions for interpreting derivation types *)

(* Generates a tuple of lists of functions and types. *)
let generate tp drv =
  try GeneratorMap.find drv !generators tp
  with Not_found ->
    failwith ("Pa_type_conv: '" ^ drv ^ "' is not a supported generator.")

let gen_derived_defs _loc tp drvs =
  let coll drv der_sis = <:str_item< $der_sis$; $generate tp drv$ >> in
  List.fold_right coll drvs <:str_item< ? >>

let sig_generate tp drv =
  try GeneratorMap.find drv !sig_generators tp
  with Not_found ->
    failwith (
      "Pa_type_conv: '" ^ drv ^ "' is not a supported signature generator.")

let gen_derived_sigs _loc tp drvs =
  let coll drv der_sis = <:sig_item< $der_sis$; $sig_generate tp drv$ >> in
  List.fold_right coll drvs (SgNil _loc)


(* Syntax extension *)

open Syntax

let found_module_name =
  Gram.Entry.of_parser "found_module_name" (fun strm ->
    match Stream.npeek 1 strm with
    | [(UIDENT name, _)] ->
        push_conv_path name;
        Stream.junk strm;
        name
    | _ -> raise Stream.Failure)

DELETE_RULE Gram str_item: "module"; a_UIDENT; module_binding0 END;

EXTEND Gram
  GLOBAL: str_item sig_item;
  str_item:
   [[
     "type"; tds = type_declaration; "with";
     drvs = LIST1 [ id = LIDENT -> id ] SEP "," ->
       <:str_item< type $tds$; $gen_derived_defs _loc tds drvs$ >>
  ]];

  str_item:
  [[
    "TYPE_CONV_PATH"; conv_path = STRING ->
      set_conv_path conv_path;
      <:str_item< ? >>
  ]];

  sig_item:
   [[
     "type"; tds = type_declaration; "with";
     drvs = LIST1 [ id = LIDENT -> id ] SEP "," ->
       <:sig_item< type $tds$; $gen_derived_sigs _loc tds drvs$ >>
  ]];

  str_item:
  [[
    "module"; i = found_module_name; mb = module_binding0 ->
      pop_conv_path ();
      <:str_item< module $i$ = $mb$ >>
  ]];

END
