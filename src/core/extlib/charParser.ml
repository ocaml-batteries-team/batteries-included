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
open ExtString
open ExtChar
open ExtInt
open ExtFloat

(** {6 Entry point} *)
type position =
{
  offset: int;
  line:   int
}
let start_position =
{ offset = 1;
  line   = 1 }

let advance c p =
  if Char.is_newline c then ((*Printf.eprintf "[Have reached line %i]\n%!" (p.line + 1);*) { offset = 1; line = p.line + 1})
  else                      { (p) with offset = p.offset + 1}

let source_of_enum   s = Source.of_enum s start_position advance

let source_of_string s = source_of_enum (String.enum s)

let parse p s =
  run p (source_of_string s)

(*let parse_enum p e =
  let latest = ref "" in
  let lines  = lines_of (input_enum e) in
  let chars  = Enum.concat (Enum.from (fun () -> match get lines with
    | None   -> raise Enum.No_more_elements
    | Some l -> latest := l;
	String.enum l)) in
  let source = source_of_enum chars in
    match run p source with
      | Std.Ok _ as result -> result
      | Std.Error report   -> Std.Error (report, ?(*Furthest position*), ?(*List of labels at that point*), !latest)*)

(** {6 Utilities}*)
let char   c = label ("\"" ^ String.of_char c ^ "\"") (exactly c)

let string s = label ("\"" ^ s ^ "\"") (
  let len = String.length s in
  let rec aux i = 
    if i < len then exactly s.[i] >>= fun _ -> aux (i+1)
    else return s
  in aux 0
)

let case_char c =
  if Char.is_letter c then one_of [Char.uppercase c; Char.lowercase c]
  else char c

let case_string s = label ("case insensitive \"" ^ s ^ "\"") (
  let s'  = String.lowercase s in
  let len = String.length s'   in
  let rec aux i = 
    if i < len then case_char s'.[i] >>= fun _ -> aux (i+1)
    else return s
  in aux 0
)   

let whitespace = satisfy Char.is_whitespace

let uppercase = label "upper case char" (satisfy Char.is_uppercase)
let lowercase = label "lower case char" (satisfy Char.is_lowercase)
let letter    = label "letter" (satisfy Char.is_letter)

let uppercase_latin1   = label "upper case char (possibly accentuated)"
  ( satisfy Char.is_uppercase_latin1 )

let lowercase_latin1   = label "lower case char (possibly accentuated)"  
  ( satisfy Char.is_lowercase_latin1 )
let latin1    = label "letter (possibly accentuated)" (satisfy Char.is_latin1)

let digit = label "digit"
  ( satisfy Char.is_digit)

let hex = label "hex"
  ( satisfy (fun x -> ( '0' <= x && x <= '9' ) || ('a' <= x && x <= 'f') || ('A' <= x && x <= 'F')))

let first s = String.get s 0

let not_char c = label ("anything but '" ^ String.of_char c ^ "'")
  (satisfy (fun x -> x <> c) (*>>=
     fun x -> Printf.eprintf "(%c)\n" x; return x*)
)

let none_of l = label (
  String.of_list (Vect.to_list (Vect.append ']'
    (List.fold_left (fun acc x -> Vect.append x acc)
       (Vect.of_list (String.to_list "anything but ['"))
       l))))
  (none_of l)

let newline = satisfy Char.is_newline

