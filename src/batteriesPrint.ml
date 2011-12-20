(* 
 * Batteries_print - Pretty-printers for the toplevel
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

let print_uchar fmt t =
  Format.fprintf fmt "UChar.of_char '%s'" (Ulib.UTF8.init 1 (fun _ -> t))

let print_rope fmt t =
  Format.fprintf fmt "r%S" (Ulib.Text.to_string t)

let print_ustring fmt t =
  Format.fprintf fmt "u%S" t

let string_of_cap t = BatString.Cap.to_string (BatString.Cap.copy t)

let print_string_cap_rw fmt t =
  Format.fprintf fmt "rw%S" (string_of_cap t)

let print_string_cap_ro fmt t =
  Format.fprintf fmt "ro%S" (string_of_cap t)

let to_format printer = (* TODO: BETTER INTERFACE *)
  fun fmt t -> Format.pp_print_string fmt (BatIO.to_string printer t)

let string_dynarray = to_format (BatDynArray.print BatString.print)
let int_dynarray = to_format (BatDynArray.print BatInt.print)
let char_dynarray = to_format (BatDynArray.print BatChar.print)
let float_dynarray = to_format (BatDynArray.print BatFloat.print)

module IntSet = BatSet.Make(BatInt)
let int_set = to_format (IntSet.print BatInt.print)
module StringSet = BatSet.Make(String)
let string_set = to_format (StringSet.print BatString.print)
module TextSet = BatSet.Make(Ulib.Text)
let text_set = to_format (TextSet.print Ulib.Text.print)
(*module CharSet = BatSet.Make(BatChar)
let char_set = to_format (CharSet.print BatChar.print) *)

let int_pset = to_format (BatSet.print BatInt.print)
let string_pset = to_format (BatSet.print BatString.print)
let rope_pset = to_format (BatSet.print Ulib.Text.print)
let char_pset = to_format (BatSet.print BatChar.print)

let (|>) x f = f x
let enum_print p oc e = BatEnum.clone e |> BatEnum.take 20 |> BatEnum.print p oc

let int_enum = to_format (enum_print BatInt.print)
let string_enum = to_format (enum_print BatString.print)
let rope_enum = to_format (enum_print Ulib.Text.print)
let char_enum = to_format (enum_print BatChar.print)

(*let iset = to_format BatISet.print *)

let int_int_pmap = to_format (BatMap.print BatInt.print BatInt.print)
let int_str_pmap = to_format (BatMap.print BatInt.print BatString.print)
let str_int_pmap = to_format (BatMap.print BatString.print BatInt.print)
let str_str_pmap = to_format (BatMap.print BatString.print BatString.print)

