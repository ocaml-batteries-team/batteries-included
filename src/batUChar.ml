(** Unicode (ISO-UCS) characters.

   This module implements Unicode characters.
*)

(* Copyright (C) 2002, 2003, 2004 Yamagata Yoriyuki. *)

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
(* yoriyuki.y@gmail.com *)

type t = int

exception Out_of_range

external code : t -> int = "%identity"

let char_of c =
  if c >= 0 && c < 0x100 then Char.chr c else raise Out_of_range

let of_char = Char.code

  (* valid range: U+0000..U+D7FF and U+E000..U+10FFFF *)
let chr n =
  if (n >= 0 && n <= 0xd7ff) or (n >= 0xe000 && n <= 0x10ffff)
  then n
  else raise Out_of_range

let unsafe_chr n = n

let eq (u1 : t) (u2 : t) = u1 = u2

let compare u1 u2 = u1 - u2

type uchar = t

let int_of u = code u
let of_int n = chr n
