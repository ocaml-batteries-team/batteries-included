(** UTF-8 encoded Unicode strings. The type is normal string. *)

(* Copyright (C) 2002, 2003 Yamagata Yoriyuki.  *)

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

type t = string

let empty = ""

type index = int

let length0 n =
  if n < 0x80 then 1 else
    if n < 0xe0 then 2 else
      if n < 0xf0 then 3 else 4

let look s i =
  let n' =
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 then n else
      if n <= 0xdf then
	(n - 0xc0) lsl 6 lor (0x7f land (Char.code (String.unsafe_get s (i + 1))))
      else if n <= 0xef then
	let n' = n - 0xe0 in
	let m = Char.code (String.unsafe_get s (i + 1)) in
	let n' = n' lsl 6 lor (0x7f land m) in
	let m = Char.code (String.unsafe_get s (i + 2)) in
	n' lsl 6 lor (0x7f land m)
      else
	let n' = n - 0xf0 in
	let m = Char.code (String.unsafe_get s (i + 1)) in
	let n' = n' lsl 6 lor (0x7f land m) in
	let m = Char.code (String.unsafe_get s (i + 2)) in
	let n' = n' lsl 6 lor (0x7f land m) in
	let m = Char.code (String.unsafe_get s (i + 3)) in
	n' lsl 6 lor (0x7f land m)
  in
  BatUChar.unsafe_chr n'

let next s i =
  let n = Char.code s.[i] in
  if n < 0x80 then i + 1 else
    if n <= 0xdf then i + 2
    else if n <= 0xef then i + 3
    else i + 4

let rec search_head_backward s i =
  if i < 0 then -1 else
    let n = Char.code s.[i] in
    if n < 0x80 || n >= 0xc2 then i else
      search_head_backward s (i - 1)

let prev s i = search_head_backward s (i - 1)

let move s i n =
  if n >= 0 then
    let rec loop i n = if n <= 0 then i else loop (next s i) (n - 1) in
    loop i n
  else
    let rec loop i n = if n >= 0 then i else loop (prev s i) (n + 1) in
    loop i n

let rec nth_aux s i n =
  if n = 0 then i else
    nth_aux s (next s i) (n - 1)

let nth s n = nth_aux s 0 n

let first _ = 0

let last s = search_head_backward s (String.length s - 1)

let out_of_range s i = i < 0 || i >= String.length s

let compare_index _ i j = i - j

let get s n = look s (nth s n)

let add_uchar buf u =
  let masq = 0b111111 in
  let k = BatUChar.code u in
  if k <= 0x7f then
    Buffer.add_char buf (Char.unsafe_chr k)
  else if k <= 0x7ff then begin
    Buffer.add_char buf (Char.unsafe_chr (0xc0 lor (k lsr 6)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)))
  end else if k <= 0xffff then begin
    Buffer.add_char buf (Char.unsafe_chr (0xe0 lor (k lsr 12)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end else  begin
    Buffer.add_char buf (Char.unsafe_chr (0xf0 + (k lsr 18)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end

let init len f =
  let buf = Buffer.create len in
  for c = 0 to len - 1 do add_uchar buf (f c) done;
  Buffer.contents buf

let make len u = init len (fun _ -> u)

let of_char u = make 1 u

let of_string_unsafe s = s
let to_string_unsafe s = s

let rec length_aux s c i =
  if i >= String.length s then c else
    let n = Char.code (String.unsafe_get s i) in
    let k =
      if n < 0x80 then 1 else
	if n < 0xe0 then 2 else
	  if n < 0xf0 then 3 else 4
    in
    length_aux s (c + 1) (i + k)

let length s = length_aux s 0 0

let rec iter_aux proc s i =
  if i >= String.length s then () else
    let u = look s i in
    proc u;
    iter_aux proc s (next s i)

let iter proc s = iter_aux proc s 0

let rec iteri_aux f s i count =
  if i >= String.length s then () else
    let u = look s i in
    f u count;
    iteri_aux f s (next s i) (count + 1)

let iteri f s = iteri_aux f s 0 0

let compare s1 s2 = String.compare s1 s2

let sub s n len =
  let ipos = move s (first s) n in
  let jpos = move s ipos len in
  String.sub s ipos (jpos-ipos)

exception Malformed_code

let validate s =
  let rec trail c i a =
    if c = 0 then a else
      if i >= String.length s then raise Malformed_code else
	let n = Char.code (String.unsafe_get s i) in
	if n < 0x80 || n >= 0xc0 then raise Malformed_code else
	  trail (c - 1) (i + 1) (a lsl 6 lor (0x7f land n)) in
  let rec main i =
    if i >= String.length s then () else
      let n = Char.code (String.unsafe_get s i) in
      if n < 0x80 then main (i + 1) else
	if n < 0xc2 then raise Malformed_code else
	  if n <= 0xdf then
	    if trail 1 (i + 1) (n - 0xc0) < 0x80 then raise Malformed_code else
	      main (i + 2)
	  else if n <= 0xef then
	    let n' = trail 2 (i + 1) (n - 0xe0) in
	    if n' < 0x800 then raise Malformed_code else
	      if n' >= 0xd800 && n' <= 0xdfff then raise Malformed_code else
		main (i + 3)
	  else if n <= 0xf4 then
	    let n = trail 3 (i + 1) (n - 0xf0) in
	    if n < 0x10000 or n > 0x10FFFF then raise Malformed_code else
	      main (i + 4)
	  else raise Malformed_code in
  main 0

let of_ascii s =
  for i = 0 to String.length s - 1 do
    if Char.code s.[i] >= 0x80 then raise Malformed_code;
  done;
  String.copy s

let of_latin1 s = init (String.length s) (fun i -> BatUChar.of_char s.[i])

module Buf =
struct
  include Buffer
  type buf = t
  let add_char = add_uchar
end

let map f us =
  let b = Buf.create (length us) in
  iter (fun c -> Buf.add_char b (f c)) us;
  Buf.contents b

let filter_map f us =
  let b = Buf.create (length us) in
  iter (fun c -> match f c with None -> () | Some c -> Buf.add_char b c) us;
  Buf.contents b

let filter p us =
  let b = Buf.create (length us) in
  iter (fun c -> if p c then Buf.add_char b c) us;
  Buf.contents b

let fold f a s =
  let rec loop a i =
    if out_of_range s i then a else
      let a' = f a (look s i) in
      loop a' (next s i) in
  loop a 0

let escaped = String.escaped

module ByteIndex : sig
  type t = string
  type b_idx(* = private int*)
  type char_idx = int
  val of_int_unsafe : int -> b_idx
  val to_int : b_idx -> int
  val next : t -> b_idx -> b_idx
  val prev : t -> b_idx -> b_idx
  val of_char_idx : t -> char_idx -> b_idx
  val at_end : t -> b_idx -> bool
  val out_of_range : t -> b_idx -> bool
  val first : b_idx
  val last : t -> b_idx
  val move : t -> b_idx -> int -> b_idx
  val look : t -> b_idx -> BatUChar.t
end = struct
  type t = string
  type b_idx = int
  type char_idx = int
  external of_int_unsafe : int -> b_idx = "%identity"
  external to_int : b_idx -> int = "%identity"
  let look = look
  let next = next
  let prev = prev
  let first = 0
  let last us = prev us (String.length us)
  let at_end us bi = bi = String.length us
  let out_of_range us bi = bi < 0 || bi >= String.length us
  let move us bi n = (* faster moving positive than negative n *)
    let bi = ref bi in
    let step = if n > 0 then next else prev in
    for j = 1 to abs n do bi := step us !bi done;
    !bi
  let of_char_idx us ci = move us first ci
end

(* Could be improved. *)
let rindex us ch =
  let rec aux ci bi =
    if ByteIndex.out_of_range us bi then raise Not_found;
    if ByteIndex.look us bi = ch then ci
    else aux (ci-1) (ByteIndex.prev us bi)
  in
  aux 0 (ByteIndex.last us)

let rec contains_aux step bi us ch =
  if ByteIndex.out_of_range us bi then false
  else if ByteIndex.look us bi = ch then true
  else contains_aux step (step us bi) us ch

let contains us ch = contains_aux ByteIndex.next ByteIndex.first us ch
