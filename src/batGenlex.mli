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

(** A generic lexical analyzer.


   This module implements a simple ``standard'' lexical analyzer, presented
   as a function from character streams to token streams. It implements
   roughly the lexical conventions of Caml, but is parameterized by the
   set of keywords of your language.

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Genlex.html}Genlex}
    module, go there for documentation on the rest of the functions
    and types.

    @author Jacques Garrigue
    @author David Teller
*)

open Genlex

type t
(** A lexer*)

val of_list            : string list ->  t
(** Create a lexer from a list of keywords*)

val to_stream_filter   :  t -> char Stream.t   ->  token Stream.t
(** Apply the lexer to a stream.*)

val to_enum_filter     :  t -> char BatEnum.t     ->  token BatEnum.t
(** Apply the lexer to an enum.*)

val to_lazy_list_filter:  t -> char BatLazyList.t ->  token BatLazyList.t
(** Apply the lexer to a lazy list.*)

(**{6 Extending to other languages}*)
open BatCharParser
module Languages :
sig
module type Definition = 
sig
  val comment_delimiters : (string * string) option
  val line_comment_start : string option
  val nested_comments    : bool
  val ident_start        : (char, char, position) BatParserCo.t
  val ident_letter       : (char, char, position) BatParserCo.t
  val op_start           : (char, char, position) BatParserCo.t
  val op_letter          : (char, char, position) BatParserCo.t
  val reserved_names     : string list
  val case_sensitive     : bool
    (**[true] if the language is case-sensitive, [false] otherwise.
       If the language is not case-sensitive, every identifier is returned
       as lower-case.*)
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
      [Float] and characters as [Char].

      If the language is not [case_sensitive], identifiers and
      keywords are returned in lower-case.
  *)
  val feed               : (char, position) BatParserCo.Source.t -> (token, position) BatParserCo.Source.t


  (** {6 Medium-level API} *)
  val start  : (char, unit, position) BatParserCo.t
    (**Remove any leading whitespaces*)


  val ident  : (char, string, position) BatParserCo.t
    (**Accepts any non-reserved identifier/operator.
       If the language is not [case_sensitive], the identifier
       is returned in lower-case.*)

  val kwd     : (char, string, position) BatParserCo.t
    (**Accepts any identifier.
       If the language is not [case_sensitive], the identifier
       is returned in lower-case.*)

  val identifier : string -> (char, unit, position) BatParserCo.t
  val keyword    : string -> (char, unit, position) BatParserCo.t

  val char_literal     : (char, char, position) BatParserCo.t
    (**Accepts a character literal, i.e. one character
       (or an escape) between two quotes.*)

  val string_literal   :(char, string, position) BatParserCo.t
    (**Accepts a string, i.e. one sequence of
       characters or escapes between two double
       quotes, on one line.*)

  val integer:       (char, int , position) BatParserCo.t
    (**Parse an integer.*)

  val float:         (char, float , position) BatParserCo.t
    (**Parse a floating-point number.*)

  val number:        (char, [`Float of float | `Integer of int] , position) BatParserCo.t
    (**Parse either an integer or a floating-point number.*)

  (** {6 Low-level API} *)
  val char         : char -> (char, char , position) BatParserCo.t
    (** As {!CharParser.char}, but case-insensitive if specified
	by {!case_sensitive}. *)

  val string       : string -> (char, string, position) BatParserCo.t
    (** As {!CharParser.string}, but case-insensitive if specified
	by {!case_sensitive}. *)

  val line_comment : (char, unit , position) BatParserCo.t
  val multiline_comment : (char, unit , position) BatParserCo.t
  val comment      : (char, unit , position) BatParserCo.t
  val whitespaces  : (char, unit , position) BatParserCo.t
(*  val lexeme       : (char, 'a , position) BatParserCo.t -> (char, 'a , position) BatParserCo.t*)
    (**Apply this filter to your own parsers if you want them
       to ignore following comments.*)

end
  
end

