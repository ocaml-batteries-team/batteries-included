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

open Str

  let search ?(offset=0) ?(backwards=false) r s =
    let next,next_offset = if backwards then search_backward, -1
                           else              search_forward,   1
    in
    let aux offset =
      try let offset' = next r s offset in
        Some ((match_beginning (), match_end (), matched_string s), offset' + next_offset)
      with Not_found -> None
    in BatEnum.unfold offset aux
