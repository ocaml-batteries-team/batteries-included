(*
 * Camlp4_PreCast -- Parsing with Camlp4
 * Copyright (C) Daniel de Rauglaudre
 *               2006 Nicolas Pouillard
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

(**
   Camlp4, the Caml Pre-Processor Pretty-Printer.

   Camlp4 is a very powerful tool which may be used either as a
   parser, a pretty-printer, or as a pre-processor. A few uses of
   Camlp4 as a pre-processor include expanding/customizing the syntax
   of OCaml, for instance to support new keywords or syntactic
   constructions, to generate boilerplate code or as type-unaware
   staged computation.

   This module provides support for using Camlp4 as a parser or a
   pretty-printer. For more information regarding the usage of Camlp4
   as a pre-processor, please see
   {{:http://brion.inria.fr/gallium/index.php/Camlp4}the official
   documentation}.

   @documents PreCast
*)

(** {6 Lexical analysis} *)

(**
   Camlp4 tokens used 
*)
type camlp4_token = Sig.camlp4_token =
  | KEYWORD       of string (**A well-known keyword. For the list of keywords, see 
			       {{:http://caml.inria.fr/pub/docs/manual-ocaml/manual044.html} the manual of OCaml}.*)
  | SYMBOL        of string
  | LIDENT        of string (**An identifier starting with a lower-case character, fit
			       for use as a value name.*)
  | UIDENT        of string (**An identifier starting with an upper-case character, fit
			       for use as a constructor name.*)
  | ESCAPED_IDENT of string
  | INT           of int * string       (**[INT (i, s)] represents an integer with value [i]
					   and written as [s].*)
  | INT32         of int32 * string     (**[INT32 (i, s)] represents an {!int32} with value [i]
					   and written as [s].*)
  | INT64         of int64 * string     (**[INT64 (i, s)] represents an {!int64} with value [i]
					   and written as [s].*)
  | NATIVEINT     of nativeint * string (**[NATIVEINT (i, s)] represents a {!nativeint} with value [i]
					     and written as [s].*)
  | FLOAT         of float * string     (**[FLOAT (x, s)] represents a {!float} with value [x]
					     and written as [s].*)
  | CHAR          of char * string      (**[CHAR (c, s)] represents a {!char} with value [c]
					     and written as [s].*)
  | STRING        of string * string    (**[STRING (s, s')] represents a {!string} with value [s]
					     and written as [s'].*)
  | LABEL         of string             (**[LABEL s] represents a function label such as [~foo],
					   where [s] is the actual name of the label*)
  | OPTLABEL      of string             (**[LABEL s] represents an optional function label such as [?foo],
					   where [s] is the actual name of the label.*)
  | QUOTATION     of Sig.quotation      (**[QUOTATION q] represents a Camlp4 quotation [q]*)
  | ANTIQUOT      of string * string
  | COMMENT       of string             (**[COMMENT c] represents a comment with content [c].
					   The contents of comments are preserved to allow
					   pretty-printing.*)
  | BLANKS        of string             (**Non-comment blanks.*)
  | NEWLINE                             (**A new line*)
  | LINE_DIRECTIVE of int * string option (**Information regarding the line position.*)
  | EOI                                 (**End of input*)

module Id         : Sig.Id;;
module Loc        : Sig.Loc;;
module Ast        : Sig.Camlp4Ast with module Loc = Loc;;
module Token      : Sig.Token
                      with module Loc = Loc
                       and type t = camlp4_token;;
module Lexer      : Sig.Lexer
                      with module Loc = Loc
                       and module Token = Token;;

(** {6 Parsing} *)

(**
   Parsing the language.
*)
module Gram       : Sig.Grammar.Static
                      with module Loc = Loc
                       and module Token = Token;;

val parse_implem ?directive_handler: (Ast.str_item -> Ast.str_item option) ->
            ?position:Ast.loc -> char Enum.t -> Ast.str_item
(** Parse an implementation (e.g. a type definition, a top-level value definition,
    ...)*)

val parse_interf ?directive_handler: (Ast.sig_item -> Ast.sig_item option) ->
            ?position:Ast.loc -> char Enum.t -> Ast.sig_item
(** Parse an interface (e.g. a type declaration, a value declaration, ...)*)

val parse_expr: ?position:Ast.loc -> char Enum.t -> Ast.expr
(** Parse a single expression.*)

val parse_patt: ?position:Ast.loc -> char Enum.t -> Ast.patt
(** Parse a single pattern.*)

(**
   Domain-specific quotations.

   The quotation mechanism permits embedding one language into
   another/
*)
module Quotation  : Sig.Quotation with module Ast = Sig.Camlp4AstToAst Ast;;


(** {6 Ast manipulation }*)

module AstFilters : Sig.AstFilters with module Ast = Ast;;
module Syntax     : Sig.Camlp4Syntax
                      with module Loc     = Loc
                       and module Token   = Token
                       and module Ast     = Ast
                       and module Gram    = Gram
                       and module Quotation = Quotation;;

(** {6 Pretty-printing}*)

module Printers : sig
  module OCaml         : (Sig.Printer Ast).S;;
  module OCamlr        : (Sig.Printer Ast).S;;
  module DumpOCamlAst  : (Sig.Printer Ast).S;;
  module DumpCamlp4Ast : (Sig.Printer Ast).S;;
  module Null          : (Sig.Printer Ast).S;;
end;;

(** {6 Plug-in management}*)

module DynLoader  : Sig.DynLoader;;

module MakeGram (Lexer : Sig.Lexer with module Loc = Loc)
  : Sig.Grammar.Static with module Loc = Loc and module Token = Lexer.Token;;

module MakeSyntax (U : sig end) : Sig.Syntax;;
