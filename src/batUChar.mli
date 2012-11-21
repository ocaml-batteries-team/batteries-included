(** Unicode characters.

   This module implements Unicode characters.
*)

(* Copyright (C) 2002, 2003, 2004, 2011 Yamagata Yoriyuki. *)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2 of *)
(* the License, or (at your option) any later version. *)

(* As a special exception to the GNU Library General Public License, you *)
(* may link, statically or dynamically, a "work that uses this library" *)
(* with a publicly distributed version of this library to produce an *)
(* executable file containing portions of this library, and distribute *)
(* that executable file under terms of your choice, without any of the *)
(* additional requirements listed in clause 6 of the GNU Library General *)
(* Public License. By "a publicly distributed version of this library", *)
(* we mean either the unmodified Library as distributed by the authors, *)
(* or a modified version of this library that is distributed under the *)
(* conditions defined in clause 3 of the GNU Library General Public *)
(* License. This exception does not however invalidate any other reasons *)
(* why the executable file might be covered by the GNU Library General *)
(* Public License . *)

(* This library is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* Lesser General Public License for more details. *)

(* You should have received a copy of the GNU Lesser General Public *)
(* License along with this library; if not, write to the Free Software *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA *)

(* You can contact the authour by sending email to *)
(* yori@users.sourceforge.net *)

type t

exception Out_of_range

(** [char_of u] returns the Latin-1 representation of [u].
    If [u] can not be represented by Latin-1, raises Out_of_range *)
val char_of : t -> char

(** [of_char c] returns the Unicode character of the Latin-1 character [c] *)
val of_char : char -> t

(** [code u] returns the Unicode code number of [u]. *)
external code : t -> int = "%identity"

(** [chr n] returns the Unicode character with the code number [n].
    If n does not lay in the valid range of Unicode or designates a
    surrogate charactor, raises Out_of_range *)
val chr : int -> t

(** Equality by code point comparison *)
val eq : t -> t -> bool

(** [compare u1 u2] returns,
    a value > 0 if [u1] has a larger Unicode code number than [u2],
    0 if [u1] and [u2] are the same Unicode character,
    a value < 0 if [u1] has a smaller Unicode code number than [u2]. *)
val compare : t -> t -> int

(** Aliases of [type t] *)
type uchar = t

(** Alias of [code] *)
val int_of : uchar -> int

(** Alias of [chr] *)
val of_int : int -> uchar

(**/**)

val unsafe_chr : int -> t

(**/**)
