(*
 * BatInt32 - Extended 32-bit integers
 * Copyright (C) 2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
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


open BatNumber

let (|>) x f = f x

let to_byte n = Int32.logand 0xffl n |> Int32.to_int |> Char.chr
let of_byte b = Char.code b |> Int32.of_int

(*$Q to_byte; of_byte
  Q.char (fun c -> Pervasives.(=) (to_byte (of_byte c)) c)
*)

(*$T to_byte
  to_byte 256l = to_byte 0l
*)

(* really need to just blit an int32 word into a string and vice versa *)
let pack str pos item =
  if String.length str < pos + 4 then invalid_arg "Int32.pack: pos too close to end of string";
  if pos < 0 then invalid_arg "Int32.pack: pos negative";
  str.[pos] <- to_byte item;
  let item = Int32.shift_right item 8 in
  str.[pos+1] <- to_byte item;
  let item = Int32.shift_right item 8 in
  str.[pos+2] <- to_byte item;
  let item = Int32.shift_right item 8 in
  str.[pos+3] <- to_byte item (* optimize out last logand? *)

(*$T pack
  let str = "    " in pack str 0 0l; (str = "\000\000\000\000")
  let str = "     " in pack str 0 0l; (str = "\000\000\000\000 ")
  let str = "     " in pack str 1 0l; (str = " \000\000\000\000")
  let str = "   " in try pack str 0 0l; false with Invalid_argument _ -> true
  let str = "    " in try pack str 1 0l; false with Invalid_argument _ -> true
*)

let pack_big str pos item =
  if String.length str < pos + 4 then invalid_arg "Int32.pack_big: pos too close to end of string";
  if pos < 0 then invalid_arg "Int32.pack_big: pos negative";
  str.[pos+3] <- to_byte item;
  let item = Int32.shift_right item 8 in
  str.[pos+2] <- to_byte item;
  let item = Int32.shift_right item 8 in
  str.[pos+1] <- to_byte item;
  let item = Int32.shift_right item 8 in
  str.[pos] <- to_byte item (* optimize out last logand? *)

(*$T pack_big
  let str = "    " in pack_big str 0 0l; (str = "\000\000\000\000")
  let str = "     " in pack_big str 0 0l; (str = "\000\000\000\000 ")
  let str = "     " in pack_big str 1 0l; (str = " \000\000\000\000")
  let str = "   " in try pack_big str 0 0l; false with Invalid_argument _ -> true
  let str = "    " in try pack_big str 1 0l; false with Invalid_argument _ -> true
*)

let unpack str pos =
  if String.length str < pos + 4 then invalid_arg "Int32.unpack: pos + 4 not within string";
  if pos < 0 then invalid_arg "Int32.unpack: pos negative";
  let shift n = Int32.shift_left n 8
  and add b n = Int32.add (of_byte b) n in
  of_byte str.[pos+3] |> shift |> add str.[pos+2] |> shift
  |> add str.[pos+1] |> shift |> add str.[pos]
(* TODO: improve performance of bit twiddling?  will these curried functions get inlined? *)

(*$T unpack
  unpack "\000\000\000\000" 0 = 0l
  unpack "\000\000\000\000 " 0 = 0l
  unpack " \000\000\000\000" 1 = 0l
  unpack "\255\000\000\000" 0 = 255l
*)

(*$Q pack; unpack
  Q.int (let str = "    " in fun x -> let x = Int32.of_int x in pack str 0 x; unpack str 0 = x)
*)


let unpack_big str pos =
  if String.length str < pos + 4 then invalid_arg "Int32.unpack: pos + 4 not within string";
  if pos < 0 then invalid_arg "Int32.unpack: pos negative";
  let shift n = Int32.shift_left n 8
  and add b n = Int32.add (of_byte b) n in
  of_byte str.[pos] |> shift |> add str.[pos+1] |> shift
  |> add str.[pos+2] |> shift |> add str.[pos+3]

(*$T unpack_big
  unpack_big "\000\000\000\000" 0 = 0l
  unpack_big "\000\000\000\000 " 0 = 0l
  unpack_big " \000\000\000\000 " 1 = 0l
  unpack_big "\000\000\000\255" 0 = 255l
*)

(*$Q pack_big; unpack_big
  Q.int (let str = "    " in fun x -> let x = Int32.of_int x in pack_big str 0 x; unpack_big str 0 = x)
*)

module BaseInt32 = struct
  include Int32

  let modulo = rem
  let pow = generic_pow ~zero ~one ~div_two:(fun n -> shift_right n 1) ~mod_two:(logand one) ~mul:mul
end

include BatNumber.MakeNumeric(BaseInt32)

let min_int = Int32.min_int
let max_int = Int32.max_int
let minus_one = Int32.minus_one
let lognot = Int32.lognot
external neg : int32 -> int32 = "%int32_neg"
external add : int32 -> int32 -> int32 = "%int32_add"
external sub : int32 -> int32 -> int32 = "%int32_sub"
external mul : int32 -> int32 -> int32 = "%int32_mul"
external div : int32 -> int32 -> int32 = "%int32_div"
external rem : int32 -> int32 -> int32 = "%int32_mod"
external logand : int32 -> int32 -> int32 = "%int32_and"
external logor : int32 -> int32 -> int32 = "%int32_or"
external logxor : int32 -> int32 -> int32 = "%int32_xor"
external shift_left : int32 -> int -> int32 = "%int32_lsl"
external shift_right : int32 -> int -> int32 = "%int32_asr"
external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
external of_int : int -> int32 = "%int32_of_int"
external to_int : int32 -> int = "%int32_to_int"
external of_float : float -> int32 = "caml_int32_of_float"
external to_float : int32 -> float = "caml_int32_to_float"
external of_string : string -> int32 = "caml_int32_of_string"
external of_int64 : int64 -> int32 = "%int64_to_int32"
external to_int64 : int32 -> int64 = "%int64_of_int32"
external of_nativeint : nativeint -> int32 = "%int32_of_nativeint"
external to_nativeint : int32 -> nativeint = "%int32_to_nativeint"

external bits_of_float : float -> int32 = "caml_int32_bits_of_float"
external float_of_bits : int32 -> float = "caml_int32_float_of_bits"
external format : string -> int32 -> string = "caml_int32_format"




type bounded = t
let min_num, max_num = min_int, max_int

let print out t = BatInnerIO.nwrite out (to_string t)
let print_hex out t = BatPrintf.fprintf out "%lx" t
