(* 
 * CharParser - Parsing unicode text
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

(** Parsing unicode text

    This module defines common functions for parsing Unicode
    texts. These functions are meant to be used in conjunction with
    the {!ParserCo} module.

    {b Note} As ParserCo, this module is still very rough and needs testing.

    @author David Teller
*)

open BatCamomile
open BatParserCo

(** The position inside one file or one stream. *)
type position = BatCharParser.position = 
{
  offset: int;(**Offset on the line (starting at 0)*)
  line:   int (**Line number (starting at 0)*)
}

val advance : UChar.t -> position -> position
(**Advance by one char. 

   [advance c p] returns a new position advanced by one char. If [c] is '\r' or '\n',
   the result is [{offset = 0; line = p.line + 1}]. Other wise, the result is
   [{offset = p.offset + 1; line = p.line}].*)

val source_of_rope : BatRope.t      -> (UChar.t, position) Source.t
(** Create a source from a Unicode Rope.*)

val source_of_enum : UChar.t BatEnum.t -> (UChar.t, position) Source.t
(** Create a source from an enumeration of unicode characters.*)

val parse : (UChar.t, 'a, position) t -> BatRope.t -> ('a, position report) BatStd.result
(**Apply a parser to a Unicode Rope.*)

(**{6 Utilities}*)

val char : UChar.t -> (UChar.t, UChar.t, position) t
  (** Recognize exactly one char*)

val none_of : UChar.t list -> (UChar.t, UChar.t, position) t
  (**Accept any value not in a list
     As [ParserCo.none_of], just with improved error message.*)

val not_char : UChar.t -> (UChar.t, UChar.t, position) t
  (**Accept any value not a given char
     As [none_of]. *)

val string : string -> (UChar.t, string, position) t
  (** Recognize exactly one string*)

val rope : BatRope.t -> (UChar.t, BatRope.t, position) t
  (** Recognize exactly one string*)

val ustring : BatUTF8.t -> (UChar.t, BatUTF8.t, position) t
  (** Recognize exactly one string*)

val case_char : UChar.t -> (UChar.t, BatUTF8.t, position) t
  (** As [char], but case-insensitive *)

val case_string : string -> (UChar.t, string, position) t
  (** As [string], but case-insensitive *)

val case_ustring : BatUTF8.t -> (UChar.t, BatUTF8.t, position) t
  (** As [ustring], but case-insensitive *)

val case_rope : BatRope.t -> (UChar.t, BatRope.t, position) t
  (** As [rope], but case-insensitive *)

val newline : (UChar.t, UChar.t, position) t
  (**Recognizes a newline*)

val whitespace : (UChar.t, UChar.t, position) t
  (**Recognizes white-space*)

val uppercase : (UChar.t, UChar.t, position) t
  (**Recognizes one upper-case ASCII character, including
     accentuated characters.*)

val lowercase : (UChar.t, UChar.t, position) t
  (**Recognizes one lower-case character, including
     accentuated characters.*)

val letter: (UChar.t, UChar.t, position) t
  (**Recognizes one lower- or upper-case character.*)

val digit : (UChar.t, UChar.t, position) t
  (**Recognizes one decimal digit*)

val hex : (UChar.t, UChar.t, position) t
  (**Recognizes one hexadecimal digit (case-insensitive)*)

