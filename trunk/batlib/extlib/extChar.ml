(* 
 * ExtChar - Additional character operations
 * Copyright (C) 1996 Xavier Leroy
 *               2008 David Teller
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

module Char = struct
  include Char

  let is_whitespace = function
    ' ' | '\010' | '\013' | '\009' | '\026' | '\012' -> true
  | _ -> false

  let of_digit = function
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | _ -> raise (Invalid_argument "Char.of_digit")

  external unsafe_int : char-> int  = "%identity"

  let enum () = 
    EnumLabels.map (Enum.( -- ) 0 255) ~f:unsafe_chr

  let ( -- ) from last =
    EnumLabels.map (Enum.( -- ) (unsafe_int from) (unsafe_int last)) ~f:unsafe_chr

  let range ?until from =
    let last = match until with
      | None   -> unsafe_chr 255
      | Some s -> s in
      from -- last


	
end
