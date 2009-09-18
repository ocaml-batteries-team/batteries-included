(*
 * ExtStr - Additional functions for regular expressions manipulation
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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

open Sexplib
open Conv
TYPE_CONV_PATH "" (*For Sexplib, Bin-prot...*)

module Str =
struct
  include Str

  let search ?(offset=0) ?(backwards=false) r s =
    let next r s o =
      if backwards then (
        let _ = search_backward r s o in
        match_beginning () - 1
      ) else (
        let _ = search_forward r s o in
        match_end ()
      ) in
    let aux offset =
      if offset < 0 then None
      else (
        try let next_offset = next r s offset in
          Some ((match_beginning (), match_end (), matched_string s), next_offset)
        with Not_found  -> None
      )
    in Enum.unfold offset aux

end
