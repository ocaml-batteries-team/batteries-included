(*
 * Base64 - Base64 codec
 * Copyright (C) 2003 Nicolas Cannasse
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

exception Invalid_char
exception Invalid_table
(* UNUSED exception Invalid_padding *)

external unsafe_char_of_int : int -> char = "%identity"

type encoding_table = char array
type decoding_table = int array

let chars = [|
  'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
  'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
  'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
  'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/'
|]

let make_decoding_table tbl =
  if Array.length tbl <> 64 then raise Invalid_table;
  let d = Array.make 256 (-1) in
  for i = 0 to 63 do
    Array.unsafe_set d (int_of_char (Array.unsafe_get tbl i)) i;
  done;
  d

let inv_chars = make_decoding_table chars

let encode ?(tbl=chars) ch =
  if Array.length tbl <> 64 then raise Invalid_table;
  let data = ref 0 in
  let count = ref 0 in
  let flush() =
    if !count > 0 then begin
      let d = (!data lsl (6 - !count)) land 63 in
      BatIO.write ch (Array.unsafe_get tbl d);
    end;
    count := 0;
  in
  let write c =
    let c = int_of_char c in
    data := (!data lsl 8) lor c;
    count := !count + 8;
    while !count >= 6 do
      count := !count - 6;
      let d = (!data asr !count) land 63 in
      BatIO.write ch (Array.unsafe_get tbl d)
    done;
  in
  let output s p l =
    for i = p to p + l - 1 do
      write (String.unsafe_get s i)
    done;
    l
  in
  BatIO.create_out ~write ~output
    ~flush:(fun () -> flush(); BatIO.flush ch)
    ~close:(fun() -> flush(); BatIO.close_out ch)


let decode ?(tbl=inv_chars) ch =
  if Array.length tbl <> 256 then raise Invalid_table;
  let data = ref 0 in
  let count = ref 0 in
  let rec fetch() =
    if !count >= 8 then begin
      count := !count - 8;
      let d = (!data asr !count) land 0xFF in
      unsafe_char_of_int d
    end else
      let c = int_of_char (BatIO.read ch) in
      let c = Array.unsafe_get tbl c in
      if c = -1 then raise Invalid_char;
      data := (!data lsl 6) lor c;
      count := !count + 6;
      fetch()
  in
  let read = fetch in
  let input s p l =
    let i = ref 0 in
    try
      while !i < l do
        String.unsafe_set s (p + !i) (fetch());
        incr i;
      done;
      l
    with
      BatIO.No_more_input when !i > 0 ->
      !i
  in
  let close() =
    count := 0;
    BatIO.close_in ch
  in
  BatIO.create_in ~read ~input ~close

let str_encode ?(tbl=chars) s =
  let ch = encode ~tbl (BatIO.output_string()) in
  BatIO.nwrite ch s;
  BatIO.close_out ch

let str_decode ?(tbl=inv_chars) s =
  let ch = decode ~tbl (BatIO.input_string s) in
  BatIO.nread ch ((String.length s * 6) / 8)

  (*$Q str_decode; str_encode
    (Q.string) (fun s -> s = str_decode (str_encode s))
    (Q.string) (fun s -> let e = str_encode s in e = str_encode (str_decode e))
  *)

  (*$T make_decoding_table
    try ignore (make_decoding_table [|'1'|]); false \
      with Invalid_table -> true
    try ignore (make_decoding_table (Array.make 2000 '1')); false \
      with Invalid_table -> true
  *)

  (*$T str_encode
    try ignore (str_encode ~tbl:[|'1'|] "mlk"); false \
      with Invalid_table -> true
    try ignore (str_encode ~tbl:(Array.make 2000 '1') "mlk"); false \
      with Invalid_table -> true
  *)

  (*$T str_decode
    try ignore (str_decode ~tbl:[|1|] "mlk"); false \
      with Invalid_table -> true
    try ignore (str_decode ~tbl:(Array.make 2000 1) "mlk"); false \
      with Invalid_table -> true
  *)
