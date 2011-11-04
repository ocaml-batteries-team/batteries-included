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


  let is_whitespace = function
    ' ' | '\010' | '\013' | '\009' | '\026' | '\012' -> true
  | _ -> false

  let is_newline = function
      '\010' | '\013' -> true
    | _               -> false

  let is_digit = function
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false

  let is_uppercase c = 'A' <= c && c <= 'Z'
  let is_lowercase c = 'a' <= c && c <= 'z'

let is_uppercase_latin1 c = is_uppercase c || ( '\192' (*À*)<= c && c <= '\214' (*Ö*) )||( '\216' (*Ø*) <= c && c <= '\221'(*Ý*) )
let is_lowercase_latin1 c = is_lowercase c || ( '\222' (*Þ*) <= c && c <= '\246'(*ö*) )||( '\248'(*ø*) <= c && c <= '\255' (*'ÿ'*) )
let is_latin1 c = is_uppercase_latin1 c || is_lowercase_latin1 c

  let is_symbol             = function
    | '!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '=' |
          '>' | '?' | '@' | '\\' | '~' | '^' | '|' | '*' -> true
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

  let is_letter c =
    is_uppercase c || is_lowercase c

  external unsafe_int : char-> int  = "%identity"
  external unsafe_chr : int -> char = "%identity"

  let enum () = 
    BatEnum.map unsafe_chr (BatEnum.( -- ) 0 255)

  let ( -- ) from last =
    BatEnum.map unsafe_chr (BatEnum.( -- ) (unsafe_int from) (unsafe_int last))

  let range ?until from =
    let last = match until with
      | None   -> unsafe_chr 255
      | Some s -> s in
      from -- last

  module Infix = struct
    let (--) = (--)
  end

  let print out t = BatInnerIO.write out t
  let t_printer paren out t = print out t

