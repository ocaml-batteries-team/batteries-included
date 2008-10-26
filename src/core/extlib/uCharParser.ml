(* 
 * CharParser - Parsing character strings
 * Copyright (C) 2008 David Teller
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


open ParserCo
open ExtUChar
open ExtInt
open ExtFloat

(** {6 Entry point} *)
type position = CharParser.position

let start_position =
{ CharParser.offset = 1;
  CharParser.line   = 1 }

let advance c p =
  if UChar.is_newline c then((*Printf.eprintf "[Have reached line %i]\n%!" (p.line + 1);*) { CharParser.offset = 1; CharParser.line = p.CharParser.line + 1})
  else                      { (p) with CharParser.offset = p.CharParser.offset + 1}

let source_of_enum   s = Source.of_enum s start_position advance

let source_of_rope   s = source_of_enum (Rope.enum s)

let source_of_utf8   s = source_of_enum (UTF8.enum s)

let parse_rope p s =
  run p (source_of_rope s)

let parse_utf8 p s =
  run p (source_of_utf8 s)


(** {6 Utilities}*)
let char   c = label ("\"" ^ ( UTF8.to_string (UTF8.of_char c) ) ^ "\"") (exactly c)

let string s = label ("\"" ^ ( UTF8.to_string s ) ^ "\"") (
  let len = UTF8.length s in
  let rec aux i = 
    if i < len then exactly (UTF8.get s i) >>= fun _ -> aux ( i + 1 )
    else return s
  in aux 0
)

let rope s = label ("\"" ^ ( Rope.to_string s ) ^ "\"") (
  Enum.fold (fun acc c -> exactly c >>= acc ) (return s) (Rope.backwards s)
)

let case_char c =
  let utf8 = UTF8.of_char c in
  either [UTF8.uppercase utf8; UTF8.lowercase utf8]


(*That one is somewhat harder as the lower-cased/upper-cased string don't necessarily
  have the same length...*)
let case_rope s = label ("case insensitive \"" ^ (Rope.to_string s) ^ "\"") (
  let lower_rope  = Rope.lowercase s       in                          (*lowercase the original string*)
  let pick enum   =
    match get enum with
      | None   -> raise Not_found
      | Some x -> x
  in
  let rec aux enum acc =
    if Enum.is_empty enum then return acc
    else
      ParserCo.any >>= fun c ->                                         (*lowercase the next char*)
	let lower_char = UTF8.lowercase (UTF8.of_char c) in             (*check that's what follows in the string*)
	let char_enum  = UTF8.enum lower_char            in
	match try Some (Enum.for_all (fun c -> Enum.pick enum = c) char_enum)
	      with Not_found -> None
	with Some false 
	  |  None       -> fail
	  |  Some true  -> aux enum (Rope.append (Rope.of_ustring lower_char))
  in aux (UTF8.first lower_string) 
)

let case_string s = (case_rope (Rope.of_ustring s)) >>= fun s' -> return Rope.to_ustring s'

let whitespace = satisfy UChar.is_whitespace

let uppercase = label "upper case char" (satisfy UChar.is_uppercase)
let lowercase = label "lower case char" (satisfy UChar.is_lowercase)
let digit = label "digit"
  ( satisfy UChar.is_digit)

let first s = String.get s 0

let not_char c = label ("anything but '" ^ UChar.to_string c ^ "'")
  (satisfy (fun x -> x <> c) (*>>=
     fun x -> Printf.eprintf "(%c)\n" x; return x*)
)

let none_of l = label (
  String.of_list (Vect.to_list (Vect.append ']'
    (List.fold_left (fun acc x -> Vect.append x acc)
       (Vect.of_list (String.to_list "anything but ['"))
       l))))
  (none_of l)

let newline = satisfy UChar.is_newline

