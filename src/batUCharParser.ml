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



open BatParserCo
open BatCamomile
module UTF8 = BatUTF8

let string_of_uchar c = (UTF8.to_string (UTF8.of_char c))

(** {6 Entry point} *)
type position = BatCharParser.position = { offset : int; line : int; }

let start_position =
{ BatCharParser.offset = 1;
  BatCharParser.line   = 1 }

let advance c p =
  if BatUChar.is_newline c then((*Printf.eprintf "[Have reached line %i]\n%!" (p.line + 1);*) { BatCharParser.offset = 1; line = p.BatCharParser.line + 1})
  else                      { (p) with BatCharParser.offset = p.BatCharParser.offset + 1}

let source_of_enum   s = Source.of_enum s start_position advance

let source_of_rope   s = source_of_enum (BatRope.enum s)

let source_of_utf8   s = source_of_enum (UTF8.enum s)

let parse_rope p s =
  run p (source_of_rope s)

let parse_utf8 p s =
  run p (source_of_utf8 s)



(** {6 Utilities}*)
let char   c = label ("\"" ^ ( string_of_uchar c ) ^ "\"") (exactly c)

let ustring s = label ("\"" ^ ( UTF8.to_string s ) ^ "\"") (
  let len = UTF8.length s in
  let rec aux i = 
    if i < len then exactly (UTF8.get s i) >>= fun _ -> aux ( i + 1 )
    else return s
  in aux 0
)

let string s = label ("\"" ^ s ^ "\"") (
  let len = String.length s in
  let rec aux i = 
    if i < len then exactly (UChar.of_char (String.get s i)) >>= fun _ -> aux ( i + 1 )
    else return s
  in aux 0
)

let rope s = label ("\"" ^ ( UTF8.to_string (BatRope.to_ustring s) ) ^ "\"") (
  BatEnum.fold (fun (acc:(_, _, _) BatParserCo.t) c -> (exactly c) >>> acc ) (return s) (BatRope.backwards s)
)

let case_char c =
  let utf8 = UTF8.of_char c in
  either [ustring (UTF8.uppercase utf8); ustring (UTF8.lowercase utf8)]


(*That one is somewhat harder as the lower-cased/upper-cased string don't necessarily
  have the same length...*)
let case_rope s = label ("case insensitive \"" ^ (UTF8.to_string (BatRope.to_ustring s)) ^ "\"") (
  let lower_rope  = BatRope.lowercase s       in                          (*lowercase the original string*)
  let pick enum   =
    match BatEnum.get enum with
      | None   -> raise Not_found
      | Some x -> x
  in
  let rec aux enum acc =
    if BatEnum.is_empty enum then return acc
    else
      BatParserCo.any >>= fun c ->                                         (*lowercase the next char*)
	let lower_char = UTF8.lowercase (UTF8.of_char c) in             (*check that's what follows in the string*)
	let char_enum  = UTF8.enum lower_char            in
	match try Some (BatEnum.for_all (fun c -> pick enum = c) char_enum)
	      with Not_found -> None
	with Some false         (*The substring doesn't match.                                  *)
	  |  None       -> fail (*We have reached the end of the enumeration but not that of [s]*)
	  |  Some true  -> aux enum (BatRope.append (BatRope.of_ustring lower_char) acc)
(*  in aux (Rope.get 0 lower_rope)*)
  in aux (BatRope.enum lower_rope) s
)

let case_ustring s = (case_rope (BatRope.of_ustring s)) >>= fun s' -> return (BatRope.to_ustring s')

let case_string s = (case_rope (BatRope.of_ustring (UTF8.of_string s))) >>= fun s' -> return (UTF8.to_string (BatRope.to_ustring s'))

let whitespace = satisfy BatUChar.is_whitespace

let uppercase = label "upper case char" (satisfy BatUChar.is_uppercase)
let lowercase = label "lower case char" (satisfy BatUChar.is_lowercase)
let digit = label "digit"
  ( satisfy (fun c -> match BatUChar.category c with `Nd -> true | _ -> false) )

let first s = String.get s 0

let not_char c = label ("anything but '" ^ string_of_uchar c ^ "'")
  (satisfy (fun x -> x <> c) (*>>=
     fun x -> Printf.eprintf "(%c)\n" x; return x*)
)

let none_of l = (*label (
  String.of_list (Vect.to_list (Vect.append ']'
    (List.fold_left (fun acc x -> Vect.append x acc)
       (Vect.of_list (String.to_list "anything but ['"))
       l))))*)
(*  label (Printf.sprintf2 "anything but [%a]" (List.print *)
  label (BatList.sprint 
	   ~first:"anything but ["
	   ~sep:"; "
	   ~last:"]" 
	   (fun out c -> (BatPrintf.fprintf out "'%a'" BatUChar.print c))
	   l
	)
	  (none_of l)

let newline = satisfy BatUChar.is_newline

let hex_uchars =
  List.map UChar.of_char
  [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
    'a'; 'b'; 'c'; 'd'; 'e'; 'f';
    'A'; 'B'; 'C'; 'D'; 'E'; 'F' ]

let hex = label "hex" (one_of hex_uchars)
(*  ( satisfy (fun x -> ( '0' <= x && x <= '9' ) || ('a' <= x && x <= 'f') || ('A' <= x && x <= 'F')))*)

let letter = satisfy (fun c -> match BatUChar.category c with `Lu | `Ll | `Lt -> true | _ -> false )

let parse p s = run p (source_of_rope s)
