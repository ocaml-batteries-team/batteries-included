(*
 * BatChar - Additional character operations
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

include Char

##V<4.3##let lowercase_ascii = function
##V<4.3##  | ('A'..'Z') as c -> unsafe_chr (code c - code 'A' + code 'a')
##V<4.3##  | c -> c
##V<4.3##
##V<4.3##let uppercase_ascii = function
##V<4.3##  | ('a'..'z') as c -> unsafe_chr (code c - code 'a' + code 'A')
##V<4.3##  | c -> c

(*$T lowercase_ascii
  lowercase_ascii 'A' = 'a'
  lowercase_ascii '�' = '�'
 *)

(*$T uppercase_ascii
  uppercase_ascii 'a' = 'A'
  uppercase_ascii '�' = '�'
 *)

let is_whitespace = function
  | ' ' | '\010' | '\013' | '\009' | '\026' | '\012' -> true
  | _ -> false

let is_newline = function
  | '\010' | '\013' -> true
  | _               -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_uppercase c = 'A' <= c && c <= 'Z'
let is_lowercase c = 'a' <= c && c <= 'z'

let is_uppercase_latin1 c =
  is_uppercase c ||
  ( '\192' (*�*)<= c && c <= '\214' (*�*) ) ||
  ( '\216' (*�*) <= c && c <= '\221'(*�*) )

let is_lowercase_latin1 c =
  is_lowercase c ||
  ( '\222' (*�*) <= c && c <= '\246'(*�*) ) ||
  ( '\248'(*�*) <= c && c <= '\255' (*'�'*) )

let is_latin1 c =
  is_uppercase_latin1 c || is_lowercase_latin1 c

let is_symbol = function
  | '!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '='
  | '>' | '?' | '@' | '\\' | '~' | '^' | '|' | '*' -> true
  | _ -> false

let is_letter c =
  is_uppercase c || is_lowercase c

external unsafe_int : char-> int  = "%identity"
external unsafe_chr : int -> char = "%identity"

let of_digit i =
  if i >= 0 && i < 10 then
    Char.unsafe_chr (i + Char.code '0')
  else
    invalid_arg "Char.of_digit"
(*$T of_digit
  of_digit 6 = '6'
  try ignore (of_digit (-2)); false with Invalid_argument _ -> true
  try ignore (of_digit (46)); false with Invalid_argument _ -> true
*)

let enum () =
  BatEnum.map unsafe_chr (BatEnum.( -- ) 0 255)
(*$T enum
  let e = enum () in for i = 0 to 255 do assert (Char.chr i = BatEnum.get_exn e) done; BatEnum.is_empty e
*)

let ( -- ) from last =
  BatEnum.map unsafe_chr (BatEnum.( -- ) (unsafe_int from) (unsafe_int last))
(*$T (--)
  let e = Char.chr 12 -- Char.chr 52 in for i = 12 to 52 do assert (Char.chr i = BatEnum.get_exn e) done; BatEnum.is_empty e
*)

let range ?until:(last = unsafe_chr 255) from =
  from -- last

module Infix = struct
  let (--) = (--)
end

let print out t = BatInnerIO.write out t
(*$T print
  BatIO.to_string print 'a' = "a"
  BatIO.to_string print '\n' = "\n"
*)

let ord (x:char) y =
  if x > y then BatOrd.Gt
  else if y > x then BatOrd.Lt
  else BatOrd.Eq
let equal (x:char) y = x == y (* safe because int-like value *)
let hash = code

module Incubator = struct
  module Comp = struct
    type t = char
    let compare = compare
  end

  module Ord = BatOrd.Ord(Comp)
  module Eq = BatOrd.EqComp(Comp)
end
