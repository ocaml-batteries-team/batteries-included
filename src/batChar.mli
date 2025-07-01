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

(** Operations on characters.

    Characters range upon Latin-1 encoding, i.e. languages used in
    Western Europe and North America. For international characters,
    another, richer, module is provided: {!UChar}.

    @author Xavier Leroy (base module)
    @author David Teller
*)

external code : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val chr : int -> char
(** Return the character with the given ASCII code.
    @raise Invalid_argument if the argument is
    outside the range 0--255. *)

val escaped : char -> string
(** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of OCaml. *)

##V<5##val lowercase : char -> char
##V<5##(** Convert the given character to its equivalent lowercase character. *)

##V<5##val uppercase : char -> char
##V<5##(** Convert the given character to its equivalent uppercase character. *)

##V>=5.4##(** {1:ascii_characters ASCII characters} *)
##V>=5.4##
##V>=5.4##(** ASCII character set support.
##V>=5.4##
##V>=5.4##    These functions give meaning to the integers \[[0x00];[0x7F]\] of the
##V>=5.4##    {{:https://en.wikipedia.org/wiki/ASCII#Character_set}ASCII
##V>=5.4##    character set}.
##V>=5.4##
##V>=5.4##    Since the UTF-8 encoding of Unicode has the same encoding and
##V>=5.4##    character semantics (U+0000 to U+001F) for these bytes, the
##V>=5.4##    functions can be safely used on elements of UTF-8 encoded [string]
##V>=5.4##    and [bytes] values. However the functions only deal with ASCII
##V>=5.4##    related matters. For example the notion of Unicode whitespace is
##V>=5.4##    much larger than the ASCII whitespace determined by
##V>=5.4##    {!Char.Ascii.is_white}.
##V>=5.4##
##V>=5.4##    @since 5.4 *)
##V>=5.4##module Ascii : sig
##V>=5.4##
##V>=5.4##  (** {1:characters Characters} *)
##V>=5.4##
##V>=5.4##  val min : char
##V>=5.4##  (** [min] is ['\x00']. *)
##V>=5.4##
##V>=5.4##  val max : char
##V>=5.4##  (** [max] is ['\x7F']. *)
##V>=5.4##
##V>=5.4##  (** {1:predicates Predicates} *)
##V>=5.4##
##V>=5.4##  val is_valid : char -> bool
##V>=5.4##   (** [is_valid c] is [true] if and only if [c] is an ASCII character,
##V>=5.4##       that is a byte in the range \[{!min};{!max}\]. *)
##V>=5.4##
##V>=5.4##  val is_upper : char -> bool
##V>=5.4##  (** [is_upper c] is [true] if and only if [c] is an ASCII uppercase letter
##V>=5.4##      ['A'] to ['Z'], that is a byte in the range \[[0x41];[0x5A]\]. *)
##V>=5.4##
##V>=5.4##  val is_lower : char -> bool
##V>=5.4##  (** [is_lower c] is [true] if and only if [c] is an ASCII lowercase letter
##V>=5.4##      ['a'] to ['z'], that is a byte in the range \[[0x61];[0x7A]\]. *)
##V>=5.4##
##V>=5.4##  val is_letter : char -> bool
##V>=5.4##  (** [is_letter c] is {!is_lower}[ c || ]{!is_upper}[ c]. *)
##V>=5.4##
##V>=5.4##  val is_alphanum : char -> bool
##V>=5.4##  (** [is_alphanum c] is {!is_letter}[ c || ]{!is_digit}[ c]. *)
##V>=5.4##
##V>=5.4##  val is_white : char -> bool
##V>=5.4##  (** [is_white c] is [true] if and only if [c] is an ASCII white space
##V>=5.4##      character, that is one of
##V>=5.4##      tab ['\t'] ([0x09]), newline ['\n'] ([0x0A]),
##V>=5.4##      vertical tab ([0x0B]), form feed ([0x0C]),
##V>=5.4##      carriage return ['\r'] ([0x0D]) or space [' '] ([0x20]),  *)
##V>=5.4##
##V>=5.4##  val is_blank : char -> bool
##V>=5.4##  (** [is_blank c] is [true] if and only if [c] is an ASCII blank character,
##V>=5.4##      that is either space [' '] ([0x20]) or tab ['\t'] ([0x09]). *)
##V>=5.4##
##V>=5.4##  val is_graphic : char -> bool
##V>=5.4##  (** [is_graphic c] is [true] if and only if [c] is an ASCII graphic
##V>=5.4##      character, that is a byte in the range \[[0x21];[0x7E]\]. *)
##V>=5.4##
##V>=5.4##  val is_print : char -> bool
##V>=5.4##  (** [is_print c] is {!is_graphic}[ c || c = ' ']. *)
##V>=5.4##
##V>=5.4##  val is_control : char -> bool
##V>=5.4##  (** [is_control c] is [true] if and only if [c] is an ASCII control character,
##V>=5.4##      that is a byte in the range \[[0x00];[0x1F]\] or [0x7F]. *)
##V>=5.4##
##V>=5.4##  (** {1:decimal_digits Decimal digits} *)
##V>=5.4##
##V>=5.4##  val is_digit : char -> bool
##V>=5.4##  (** [is_digit c] is [true] if and only if [c] is an ASCII decimal digit
##V>=5.4##      ['0'] to ['9'], that is a byte in the range \[[0x30];[0x39]\]. *)
##V>=5.4##
##V>=5.4##  val digit_to_int : char -> int
##V>=5.4##  (** [digit_to_int c] is the numerical value of a digit
##V>=5.4##      that satisfies {!is_digit}. Raises [Invalid_argument] if
##V>=5.4##      {!is_digit}[ c] is [false]. *)
##V>=5.4##
##V>=5.4##  val digit_of_int : int -> char
##V>=5.4##  (** [digit_of_int n] is an ASCII decimal digit for the decimal
##V>=5.4##      value [abs (n mod 10)]. *)
##V>=5.4##
##V>=5.4##  (** {1:hex_digits Hexadecimal digits} *)
##V>=5.4##
##V>=5.4##  val is_hex_digit : char -> bool
##V>=5.4##  (** [is_hex_digit c] is [true] if and only if [c] is an ASCII hexadecimal
##V>=5.4##      digit ['0'] to ['9'], ['a'] to ['f'] or ['A'] to ['F'],
##V>=5.4##      that is a byte in one of the ranges \[[0x30];[0x39]\],
##V>=5.4##      \[[0x41];[0x46]\], \[[0x61];[0x66]\]. *)
##V>=5.4##
##V>=5.4##  val hex_digit_to_int : char -> int
##V>=5.4##  (** [hex_digit_to_int c] is the numerical value of a digit that
##V>=5.4##      satisfies {!is_hex_digit}. Raises [Invalid_argument] if
##V>=5.4##      {!is_hex_digit}[ c] is [false]. *)
##V>=5.4##
##V>=5.4##  val lower_hex_digit_of_int : int -> char
##V>=5.4##  (** [lower_hex_digit_of_int n] is a lowercase ASCII hexadecimal digit for
##V>=5.4##      the hexadecimal value [abs (n mod 16)]. *)
##V>=5.4##
##V>=5.4##  val upper_hex_digit_of_int : int -> char
##V>=5.4##  (** [upper_hex_digit_of_int n] is an uppercase ASCII hexadecimal
##V>=5.4##      digit for the hexadecimal value [abs (n mod 16)]. *)
##V>=5.4##
##V>=5.4##  (** {1:casing Casing transforms} *)
##V>=5.4##
##V>=5.4##  val uppercase : char -> char
##V>=5.4##  (** [uppercase c] is [c] with ASCII characters ['a'] to ['z'] respectively
##V>=5.4##      mapped to uppercase characters ['A'] to ['Z']. Other characters are left
##V>=5.4##      untouched. *)
##V>=5.4##
##V>=5.4##  val lowercase : char -> char
##V>=5.4##  (** [lowercase c] is [c] with ASCII characters ['A'] to ['Z'] respectively
##V>=5.4##      mapped to lowercase characters ['a'] to ['z']. Other characters are
##V>=5.4##      left untouched. *)
##V>=5.4##end

val lowercase_ascii : char -> char
(** Convert the given character to its equivalent lowercase character,
   using the US-ASCII character set.
   @since 2.5.0 *)

val uppercase_ascii : char -> char
(** Convert the given character to its equivalent uppercase character,
   using the US-ASCII character set.
   @since 2.5.0 *)

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
    A character is uppercase Latin 1 if it is between
    ['A'] and ['Z'], between ['À'] and ['Ö'] or
    between ['Ø'] and ['Ý'] *)

val is_lowercase_latin1: char -> bool
(** Determine if a character is lowercase Latin 1.
    A character is lowercase Latin 1 if it is between
    ['a'] and ['z'], between ['Þ'] and ['ö'] or
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
(** Determine if a character is a newline.  Newline characters are
      defined as ['\010'] and ['\013']*)

val of_digit : int -> char
(** Return the character representing a given digit.
    @raise Invalid_argument if the
    argument is outside the range 0--9*)

val enum: unit -> char BatEnum.t
(** Produce the enumeration of all characters *)

val range: ?until:char -> char -> char BatEnum.t
(** [range from ?until] produces an enumeration of the
    characters from [from] to [until] included
    [until] defaults to ['\255']
*)

val ( -- ): char -> char -> char BatEnum.t
(** Produce the enumeration of a segment of characters.

    ['a' -- 'z'] is the enumeration of all characters
    between ['a'] and ['z'] included.*)

(** {1 Infix submodule regrouping all infix operators} *)
module Infix : sig
  val ( -- ): char -> char -> char BatEnum.t
end

(** {1 Boilerplate code}*)

val print: 'a BatInnerIO.output -> Char.t -> unit

val compare: t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal : t -> t -> bool
val hash : t -> int

##V>=5.1##val seeded_hash : int -> t -> int

val ord : char BatOrd.ord

module Incubator : sig
  module Comp : BatOrd.Comp with type t = char
  module Ord : BatOrd.Ord with type t = char
  module Eq : BatOrd.Eq with type t = char
end

(**/**)

external unsafe_chr : int -> char = "%identity"
external unsafe_int : char-> int  = "%identity"

(**/**)
