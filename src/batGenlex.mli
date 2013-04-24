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
    roughly the lexical conventions of OCaml, but is parameterized by the
    set of keywords of your language.


    Example: a lexer suitable for a desk calculator is obtained by
    {[     let lexer = make_lexer ["+";"-";"*";"/";"let";"="; "("; ")"]  ]}

    The associated parser would be a function from [token stream]
    to, for instance, [int], and would have rules such as:

    {[
      let parse_expr = parser
          [< 'Int n >] -> n
                     | [< 'Kwd "("; n = parse_expr; 'Kwd ")" >] -> n
                     | [< n1 = parse_expr; n2 = parse_remainder n1 >] -> n2
      and parse_remainder n1 = parser
          [< 'Kwd "+"; n2 = parse_expr >] -> n1+n2
                             | ...
    ]}

    @author Jacques Garrigue
    @author David Teller
*)

(** The type of tokens. The lexical classes are: [Int] and [Float]
    for integer and floating-point numbers; [String] for
    string literals, enclosed in double quotes; [Char] for
    character literals, enclosed in single quotes; [Ident] for
    identifiers (either sequences of letters, digits, underscores
    and quotes, or sequences of ``operator characters'' such as
    [+], [*], etc); and [Kwd] for keywords (either identifiers or
    single ``special characters'' such as [(], [}], etc). *)
type token = Genlex.token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char

val make_lexer : string list -> char Stream.t -> token Stream.t
(** Construct the lexer function. The first argument is the list of
    keywords. An identifier [s] is returned as [Kwd s] if [s]
    belongs to this list, and as [Ident s] otherwise.
    A special character [s] is returned as [Kwd s] if [s]
    belongs to this list, and cause a lexical error (exception
    [Parse_error]) otherwise. Blanks and newlines are skipped.
    Comments delimited by [(*] and [*)] are skipped as well,
    and can be nested. *)

(* {6 Batteries extensions to genlex } *)
type lexer_error =
  | IllegalCharacter of char
  | NotReallyAChar
  | NotReallyAnEscape
  | EndOfStream

exception LexerError of lexer_error * int

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

val string_of_token : token -> string

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
