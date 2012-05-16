(*
 * BatText - Unicode text library
 * Copyright (C) 2012 The Batteries Included Team
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

open BatIO


(* From BatIO *)
val read_char: input -> Uniclib.UChar.t
(** Read one Unicode char from a UTF-8 encoded input*)

val read_text: input -> int -> Uniclib.Text.t
(** Read up to n chars from a UTF-8 encoded input*)

val read_line: input -> Uniclib.Text.t
(** Read a line of UTF-8*)

val read_all : input -> Uniclib.Text.t
(** Read the whole contents of a UTF-8 encoded input*)

val write_char: (Uniclib.UChar.t, _) printer
(** Write one uchar to a UTF-8 encoded output.*)

val write_text : (Uniclib.Text.t, _) printer
(** Write a character text onto a UTF-8 encoded output.*)

val write_line: (Uniclib.Text.t, _) printer
(** Write one line onto a UTF-8 encoded output.*)

val lines_of : input -> Uniclib.Text.t BatEnum.t
(** offer the lines of a UTF-8 encoded input as an enumeration*)

val chars_of : input -> Uniclib.UChar.t BatEnum.t
(** offer the characters of an UTF-8 encoded input as an enumeration*)

(* BatPrint functions *)

val sprintf : ('a, Uniclib.Text.t) BatPrint.format -> 'a
  (** [rprintf fmt] returns the result as a rope *)

val ksprintf : (Uniclib.Text.t -> 'b) -> ('a, 'b) BatPrint.format -> 'a
  (** [krprintf k fmt] creates a rope from the format and other
      arguments and pass it to [k] *)


(* From pervasives *)
val output_text : unit BatIO.output -> Uniclib.Text.t -> unit
(** Write the text on the given output channel. *)
