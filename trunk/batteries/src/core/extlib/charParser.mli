(* 
 * CharParser - Parsing character strings
 * Copyright (C) 2008 David Teller (contributor)
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

(** Parsing character strings. 

    @author David Teller
*)

open ParserCo


(** The position inside one file or one stream. *)
type position =
{
  offset: int;(**Offset on the line (starting at 0)*)
  line:   int (**Line number (starting at 0)*)
}

val advance : char -> position -> position

val source_of_string : string      -> (char, position) Source.t
val source_of_enum   : char Enum.t -> (char, position) Source.t

val parse_string : (char, 'a, position) t -> string -> ('a, position report) Std.result

(*val parse_enum : (char, 'a, position) t -> char Enum.t -> ('a, position report * position * string list * string) Std.result*)
(**{6 Utilities}*)

val char : char -> (char, char, position) t
  (** Recognize exactly one char*)

val none_of : char list -> (char, char, position) t
  (**Accept any value not in a list
     As [ParserCo.none_of], just with improved error message.*)

val not_char : char -> (char, char, position) t
  (**Accept any value not a given char
     As [none_of]. *)

val string : string -> (char, string, position) t
  (** Recognize exactly one string*)

val case_char : char -> (char, char, position) t
  (** As [char], but case-insensitive *)

val case_string : string -> (char, string, position) t
  (** As [case_string], but case-insensitive *)

val newline : (char, char, position) t
  (**Recognizes a newline*)

val whitespace : (char, char, position) t
  (**Recognizes white-space*)

val uppercase : (char, char, position) t
  (**Recognizes one upper-case ASCII character, including
     accentuated characters.*)

val lowercase : (char, char, position) t
  (**Recognizes one lower-case ASCII character, including
     accentuated characters.*)

val letter: (char, char, position) t
  (**Recognizes one lower- or upper-case ASCII character, including
     accentuated characters.*)

val uppercase_latin1 : (char, char, position) t  (*@TODO: test*)
  (**Recognizes one upper-case Latin-1 character, including
     accentuated characters.*)

val lowercase_latin1 : (char, char, position) t  (*@TODO: test*)
  (**Recognizes one lower-case Latin-1 character, including
     accentuated characters.*)

val latin1: (char, char, position) t
  (**Recognizes one lower- or upper-case Latin1 character, including
     accentuated characters.*)

val digit : (char, char, position) t
  (**Recognizes one decimal digit*)

val hex : (char, char, position) t
  (**Recognizes one hexadecimal digit (case-insensitive)*)

