(*
 * BatLexing - Additional functions for string manipulations.
 * Copyright (C) 1996 Xavier Leroy, INRIA Rocquencourt
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
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
open Lexing
(** Simple lexing using ocaml conventions 

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html}Lexing}
    module, go there for documentation on the rest of the functions
    and types.
    
*)


val from_input   : BatIO.input   -> lexbuf
(** Create a lexer buffer on the given input
   [Lexing.from_input inp] returns a lexer buffer which reads
   from the input [inp], at the current reading position. *)


(** {6 Deprecated}*)

val from_channel : BatIO.input -> lexbuf
(** @deprecated As {!from_input}*)

(**/**)
