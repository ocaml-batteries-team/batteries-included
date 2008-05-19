(*
 * Genlex - Generic lexer
 * Copyright (C) 2002 Jacques Garrigue
 *               2008 David Teller (Contributor)
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

module Genlex : sig
(** A generic lexical analyzer.


   This module implements a simple ``standard'' lexical analyzer, presented
   as a function from character streams to token streams. It implements
   roughly the lexical conventions of Caml, but is parameterized by the
   set of keywords of your language.
*)

type token = 
  | 	Kwd of string
  | 	Ident of string
  | 	Int of int
  | 	Float of float
  | 	String of string
  | 	Char of char

(*Genlex.token*)

type t
(** A lexer*)

val of_list            : string list ->  t
(** Create a lexer from a list of keywords*)

val to_stream_filter   :  t -> char Stream.t   ->  token Stream.t
(** Apply the lexer to a stream.*)

val to_enum_filter     :  t -> char Enum.t     ->  token Enum.t
(** Apply the lexer to an enum.*)

val to_lazy_list_filter:  t -> char LazyList.t ->  token LazyList.t
(** Apply the lexer to a lazy list.*)

(**
   {6 Old functions}
*)
val make_lexer : string list -> char Stream.t -> token Stream.t
(** Construct the lexer function. The first argument is the list of
   keywords. An identifier s is returned as Kwd s if s belongs to this
   list, and as Ident s otherwise. A special character s is returned
   as Kwd s if s belongs to this list, and cause a lexical error
   (exception Parse_error) otherwise. Blanks and newlines are
   skipped. Comments delimited by (* and *) are skipped as well, and
   can be nested. *)

(**{6 Extending to other languages}*)

module Languages :
sig
module type Definition = 
sig
  val comment_delimiters : (string * string) option
  val line_comment_start : string option
  val nested_comments    : bool
  val ident_start        : (char, char) ParserCo.t
  val ident_letter       : (char, char) ParserCo.t
  val op_start           : (char, char) ParserCo.t
  val op_letter          : (char, char) ParserCo.t
  val reserved_names     : string list
  val reserved_op_names  : string list
  val case_sensitive     : bool
end

module Library :
sig
  module OCaml : Definition
  module C     : Definition
end

module Make(M:Definition) :
sig
  (**Create a lexer from a language definition*)

  (** {6 High-level API} *)

  (** Drop comments, present reserved operators and reserved
      names as [Kwd], operators and identifiers as [Ident],
      integer numbers as [Int], floating-point numbers as
      [Float] and characters as [Char].*)

  val as_parser        : (char, token) ParserCo.t

  (** {6 Medium-level API} *)
  val any_identifier   : (char, string) ParserCo.t
    (**Accepts any identifier*)

  val any_operator     : (char, string) ParserCo.t
    (**Accepts any operator*)

  val identifier       : (char, string) ParserCo.t -> (char, string) ParserCo.t
    (**Accepts a given identifier*)

  val operator         : (char, string) ParserCo.t -> (char, string) ParserCo.t
    (**Accepts a given operator *)

  val reserved         : (char, string) ParserCo.t -> (char, string) ParserCo.t

  val reserved_op      : (char, string) ParserCo.t -> (char, string) ParserCo.t

  val char_literal : (char, char) ParserCo.t
    (**Accepts a character literal, i.e. one character
       (or an escape) between two quotes.*)

  val string_literal:(char, string) ParserCo.t
    (**Accepts a string, i.e. one sequence of
       characters or escapes between two double
       quotes, on one line.*)

  val integer:       (char, int) ParserCo.t
    (**Parse an integer.*)

  val float:         (char, float) ParserCo.t
    (**Parse a floating-point number.*)

  val number:        (char, [`Float of float | `Integer of int]) ParserCo.t
    (**Parse either an integer or a floating-point number.*)

  (** {6 Low-level API} *)
  val char         : char -> (char, char) ParserCo.t
    (** As {!ParserCo.char}, but case-insensitive if specified
	by {!case_sensitive}. *)

  val string       : string -> (char, string) ParserCo.t
    (** As {!ParserCo.string}, but case-insensitive if specified
	by {!case_sensitive}. *)

  val line_comment : (char, unit) ParserCo.t
  val multiline_comment : (char, unit) ParserCo.t
  val comment      : (char, unit) ParserCo.t
  val whitespaces  : (char, unit) ParserCo.t
  val lexeme       : (char, 'a) ParserCo.t -> (char, 'a) ParserCo.t
    (**Apply this filter to your own parsers if you want them
       to ignore following comments.*)

end
  
end
end
