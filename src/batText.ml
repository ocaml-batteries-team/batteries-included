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

(** {6 Unicode}*)

(** {7 Reading unicode}

All these functions assume that the input is UTF-8 encoded.
*)

(*val read_uchar: input -> UChar.t*)
(** read one UChar from a UTF-8 encoded input*)
let read_char i =
  let n0  = read i in
  let len = Uniclib.UTF8.length0 (Char.code n0) in
  if len = 1 then Uniclib.UChar.of_char n0
  else
    let s = String.create len in
    String.set s 0 n0;
    ignore(really_input i s 1 ( len - 1));
    Uniclib.UTF8.get s 0


(*val uchars_of : input -> UChar.t BatEnum.t*)
(** offer the characters of an UTF-8 encoded input as an enumeration*)

let chars_of i = make_enum read_char i

(*val read_rope: input -> int -> Rope.t*)
(** read up to n uchars from a UTF-8 encoded input*)
let read_text i n =
  let rec loop r j =
    if j = 0 then r
    else loop (Uniclib.Text.append_char (read_char i) r) (j-1) (* TODO: make more efficient by appending a string of Rope.leaf_size (256) chars at a time *)
  in
  if n <= 0 then Uniclib.Text.empty
  else loop Uniclib.Text.empty n

(** read the whole contents of a UTF-8 encoded input*)
let read_all i = Uniclib.Text.of_string (BatIO.read_all i)
(* TODO: make efficient - possibly similar to above - buffering leaf_size chars at a time *)

(** read a line of UTF-8*)
let read_line i =
  let line = read_line i in
  Uniclib.UTF8.validate line;
  Uniclib.Text.of_string line

(** offer the lines of a UTF-8 encoded input as an enumeration*)
let lines_of i = BatIO.make_enum read_line i

(** {7 Writing unicode}

All these functions assume that the output is UTF-8 encoded.*)

let write_string o c = write_string o c

(*val write_uchar: _ output -> UChar.t -> unit*)
let write_char o c = write_string o (Uniclib.UTF8.init 1 (fun _ -> c))

(*val write_rope : _ output -> Rope.t -> unit*)
let write_text o t = Uniclib.Text.print o t

(*val write_uline: _ output -> Rope.t -> unit*)
let write_line o r = write_text o r; write o '\n'

(*val write_ulines : _ output -> Rope.t BatEnum.t -> unit*)
let write_lines o re = BatEnum.iter (write_line o) re

(*val write_ropes : _ output -> Rope.t BatEnum.t -> unit*)
let write_texts o re = BatEnum.iter (write_text o) re

(*val write_uchars : _ output -> UChar.t BatEnum.t -> unit*)
let write_chars o uce = BatEnum.iter (write_char o) uce

let sprintf fmt =
  BatPrint.ksprintf Uniclib.Text.of_string fmt

let ksprintf k fmt =
  BatPrint.ksprintf (fun s -> k (Uniclib.Text.of_string s)) fmt

let output_text       = Uniclib.Text.print
