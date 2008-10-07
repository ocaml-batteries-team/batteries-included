(*
 * Pa_type_conv -- Syntax extension for local module opening
 * Copyright (C)    2004, 2005 Martin Sandin  <msandin@hotmail.com> (as "tywith")
 *                  2005-?     Makus Mottl, Jane Street Holding, LLC
 *                  2008       David Teller, LIFO, Universite d'Orleans
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

open Batteries.Toolchain.Boilerplate.Type_conv
open Camlp4.PreCast
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

module MapGenerator =
struct
  let rec implem = function
    | 
    
end

let _ = add_generator "map" MapGemerator.implem
