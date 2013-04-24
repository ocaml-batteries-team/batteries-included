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
  Format.fprintf fmt "UChar.of_char '%s'" (BatUTF8.init 1 (fun _ -> t))

let print_rope fmt t =
  Format.fprintf fmt "r%S" (BatText.to_string t)

let print_ustring fmt t =
  Format.fprintf fmt "u%S" t

let string_of_cap t = BatString.Cap.to_string (BatString.Cap.copy t)

let print_string_cap_rw fmt t =
  Format.fprintf fmt "rw%S" (string_of_cap t)

let print_string_cap_ro fmt t =
  Format.fprintf fmt "ro%S" (string_of_cap t)

let string_dynarray = BatIO.to_f_printer (BatDynArray.print BatString.print)
let int_dynarray = BatIO.to_f_printer (BatDynArray.print BatInt.print)
let char_dynarray = BatIO.to_f_printer (BatDynArray.print BatChar.print)
let float_dynarray = BatIO.to_f_printer (BatDynArray.print BatFloat.print)

module IntSet = BatSet.Make(BatInt)
let int_set = BatIO.to_f_printer (IntSet.print BatInt.print)
module StringSet = BatSet.Make(String)
let string_set = BatIO.to_f_printer (StringSet.print BatString.print)
module TextSet = BatSet.Make(BatText)
let text_set = BatIO.to_f_printer (TextSet.print BatText.print)
(*module CharSet = BatSet.Make(BatChar)
  let char_set = BatIO.to_f_printer (CharSet.print BatChar.print) *)

let int_pset = BatIO.to_f_printer (BatSet.print BatInt.print)
let string_pset = BatIO.to_f_printer (BatSet.print BatString.print)
let rope_pset = BatIO.to_f_printer (BatSet.print BatText.print)
let char_pset = BatIO.to_f_printer (BatSet.print BatChar.print)

let (|>) x f = f x
let enum_print_limit = ref 20
let enum_print p oc e =
  let e = BatEnum.clone e in
  for _i = 1 to !enum_print_limit-1 do
    match BatEnum.get e with
    | None -> ()
    | Some x -> p oc x; BatIO.write oc ' '
  done;
  if not (BatEnum.is_empty e) then BatIO.nwrite oc "..."

let int_enum = BatIO.to_f_printer (enum_print BatInt.print)
let string_enum = BatIO.to_f_printer (enum_print BatString.print)
let rope_enum = BatIO.to_f_printer (enum_print BatText.print)
let char_enum = BatIO.to_f_printer (enum_print BatChar.print)

(*let iset = BatIO.to_f_printer BatISet.print *)

let int_int_pmap = BatIO.to_f_printer (BatMap.print BatInt.print BatInt.print)
let int_str_pmap = BatIO.to_f_printer (BatMap.print BatInt.print BatString.print)
let str_int_pmap = BatIO.to_f_printer (BatMap.print BatString.print BatInt.print)
let str_str_pmap = BatIO.to_f_printer (BatMap.print BatString.print BatString.print)

  (*let bitset = BatIO.to_f_printer BatBitSet.print*)
