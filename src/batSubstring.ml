(*
 * re-implementation of SML's Substring library in OCaml.
 * Copyright (C) 2008 Edgar Friendly <thelema314@gmail.com>
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
 *
 * See http://www.itu.dk/~sestoft/mosmllib/Substring.html for documentation
 *
 *)

include BatInnerSubstring

(*
let of_chan chan =
  let tempsize = 16384 in
  let buf = Buffer.create tempsize
  and tmp = String.create tempsize in
  let n = ref 0 in
  while n := input chan tmp 0 tempsize; !n > 0 do
    Buffer.add_substring buf tmp 0 !n;
  done;
  Buffer.contents buf, 0, Buffer.length buf
*)

let of_input inp =
  let tempsize = 16384 in
  let buf = Buffer.create tempsize
  and tmp = Bytes.create tempsize in
  let n = ref 0 in
  while n := BatIO.input inp tmp 0 tempsize; !n > 0 do
    BatBytesCompat.buffer_add_subbytes buf tmp 0 !n;
  done;
  unsafe_substring (Buffer.contents buf) 0 (Buffer.length buf)

let translate f sol =
  let (str,off,len) = base sol in
  BatString.init len (fun i -> f str.[off+i])
(*$T translate
  translate (function 'o' -> 'a' | x -> x)(substring "foobar" 1 3) = "aab"
  translate (fun x -> x) (empty ()) = ""
*)

let tokens p sol =
  let (str,off,len) = base sol in
  let i = ref 0 and j = ref 0 and acc = BatRefList.empty () in
  while !j < len do
    while !i < len && p str.[off+ !i] do incr i; done;
    j := !i+1;
    while !j < len && not (p str.[off+ !j]) do incr j; done;
    BatRefList.push acc (unsafe_substring str !i (!j - !i));
    i := !j+1;
  done;
  BatRefList.to_list acc
(*$T tokens
   tokens (fun x -> x = ';') (substring "foo;bar" 0 7) = [substring "foo;bar" 4 3; substring "foo;bar" 0 3]
   tokens (fun x -> x = ';') (substring "foo;;bar" 0 8) = [substring "foo;;bar" 5 3; substring "foo;;bar" 0 3]
   tokens (fun x -> x = ';') (empty ()) = []
*)

let fields p sol =
  let (str, off, len) = base sol in
  let i = ref 0 and j = ref 0 and acc = BatRefList.empty() in
  while !j < len do
    while !j < len && not (p str.[off+ !j]) do incr j; done;
    BatRefList.push acc (unsafe_substring str !i (!j - !i));
    incr j; i := !j;
  done;
  BatRefList.to_list acc
(*$T fields
   fields (fun x -> x = ';') (substring "foo;;bar" 0 8) = [substring "foo;;bar" 5 3; substring "foo;;bar" 4 0; substring "foo;;bar" 0 3]
   fields (fun x -> x = ';') (substring "foo;bar" 0 7) = [substring "foo;bar" 4 3; substring "foo;bar" 0 3]
   fields (fun x -> x = ';') (empty ()) = []
*)

let trim x = dropl BatChar.is_whitespace (dropr BatChar.is_whitespace x)
(*$T trim
   trim (empty ()) = empty ()
   trim (of_string " foobar ") = substring " foobar " 1 6
   trim (of_string "foobar") = of_string "foobar"
*)

let rec enum sol =
  let (str, off, len) = base sol in
  let last_element = off + len - 1 in
  let i = ref off in
  BatEnum.make
    ~next:(fun () ->
      if !i > last_element then raise BatEnum.No_more_elements
      else str.[BatRef.post_incr i] )
    ~count:(fun () -> len - !i)
    ~clone:(fun () -> enum (unsafe_substring str !i (len - !i)))
(*$T enum
   Enum.compare Char.compare (enum (of_string "foo")) (String.enum "foo") = 0
   Enum.compare Char.compare (enum (of_string "foo")) (String.enum "fob") <> 0
   Enum.compare Char.compare (enum (empty ())) (String.enum "") = 0
   Enum.compare Char.compare (enum (empty ())) (String.enum "P") <> 0
*)
let print oc ss = iter (fun c -> BatIO.write oc c) ss
