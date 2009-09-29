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

(** Operations on characters. 

    Characters range upon Latin-1 encoding, i.e. languages used in
    Western Europe and North America. For international characters,
    another, richer, module is provided: {!UChar}.

    @author Xavier Leroy (base module)
    @author David Teller

    @documents Char
*)
module Char : sig

type t = char
(** An alias for the type of characters. *)


val is_whitespace : char -> bool
(** Determine if a character is a whitespace.
    Whitespace characters are defined as 
    [' '], ['\010'], ['\013'], ['\009'], ['\026']
    and ['\012']. *)

val is_uppercase : char -> bool
(** Determine if a character is uppercase ASCII.
    A character is uppercase ASCII if it is between
    ['A'] and ['Z'] *)

val is_lowercase : char -> bool
(** Determine if a character is lowercase ASCII.
    A character is lowercase ASCII if it is between
    ['a'] and ['z'] *)


val is_uppercase_latin1: char -> bool
(** Determine if a character is uppercase Latin 1.
    A character is uppercase ASCII if it is between
    ['A'] and ['Z'], between ['À'] and ['Ö'] or
    between ['Ø'] and ['İ'] *)

val is_lowercase_latin1: char -> bool
(** Determine if a character is lowercase Latin 1.
    A character is lowercase ASCII if it is between
    ['a'] and ['z'], between ['Ş'] and ['ö'] or
    between ['ø'] and ['ÿ']*)

val is_latin1: char -> bool
(** Determine if a character is a Latin 1 letter.
    A character is a Latin 1 letter if it is either
    an uppercase or a lowercase Latin 1 character.*)

val is_digit     : char -> bool
  (** Determine if a character represents a digit.  Digits are ['0'],
      ['1'], ... ['9']. *)

val is_symbol    : char -> bool
  (** Determine if a character represents a (OCaml-style)
      symbol. Symbols are ['!'], ['%'], ['&'], ['$'], ['#'], ['+'],
      ['-'], ['/'], [':'], ['<'], ['='] ['>'], ['?'], ['@'], ['\\'],
      ['~'], ['^'], ['|'], ['*'] *)

val is_letter    : char -> bool
  (** Determine if a character represents a ASCII letter.*)

val is_newline : char -> bool
  (** Determine if a character is a newline.
      Newline characters are defined as ['\010']
      and ['\013']*)

val of_digit : int -> char
(** Return the character representing a given digit.
    Raise [Invalid_argument "Char.of_digit"] if the
    argument is outside the range 0--9*)

val chr : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "Char.chr"] if the argument is
   outside the range 0--255. *)

val escaped : char -> string
(** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of Objective Caml. *)

val lowercase : char -> char
(** Convert the given character to its equivalent lowercase character. *)

val uppercase : char -> char
(** Convert the given character to its equivalent uppercase character. *)

val compare: t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val enum: unit -> char Enum.t
(** Produce the enumeration of all characters *)

val range: ?until:char -> char -> char Enum.t

val ( -- ): char -> char -> char Enum.t
(** Produce the enumeration of a segment of characters.

    ['a' -- 'z'] is the enumeration of all characters
    between ['a'] and ['z'] included.*)

external code : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print: 'a InnerIO.output -> t -> unit

(**/**)

external unsafe_chr : int -> char = "%identity"
external unsafe_int : char-> int  = "%identity"

end
