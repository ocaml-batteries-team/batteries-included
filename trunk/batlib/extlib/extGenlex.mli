(*
 * Genlex - Generic lexer
 * Copyright (C) 2002 Jacques Garrigue
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
   {6} Old functions
*)
val make_lexer : string list -> char Stream.t -> token Stream.t
(** Construct the lexer function. The first argument is the list of
   keywords. An identifier s is returned as Kwd s if s belongs to this
   list, and as Ident s otherwise. A special character s is returned
   as Kwd s if s belongs to this list, and cause a lexical error
   (exception Parse_error) otherwise. Blanks and newlines are
   skipped. Comments delimited by (* and *) are skipped as well, and
   can be nested. *)

end
