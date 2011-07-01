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

type t = string * int * int (* string, offset, length *)

let empty () = "", 0, 0

let to_string (s,o,l) = String.sub s o l

let of_string s = s, 0, String.length s

let make len c = String.make len c, 0, len

let create len = String.make len '\000', 0, len

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
  and tmp = String.create tempsize in
  let n = ref 0 in
  while n := BatIO.input inp tmp 0 tempsize; !n > 0 do
    Buffer.add_substring buf tmp 0 !n;
  done;
  Buffer.contents buf, 0, Buffer.length buf
    
let substring str off len = 
  let sl = String.length str in
  if off < 0 then invalid_arg "Substring.substring: negative offset not allowed";
  if len < 0 then invalid_arg "Substring.substring: negative length not allowed";
  if off + len > sl then invalid_arg "Substring.substring: offset + length past end of string";
  (str,off,len)

let extract s o = function
    Some len -> substring s o len
  | None -> substring s o (String.length s - o)

let all = of_string

let base s = s

let is_empty (_,_,len) = len = 0

let getc (str,off,len) = 
  if len = 0 then None else Some (str.[off], (str, off+1, len-1))

let first (str,off,len) = if len = 0 then None else Some str.[off]

let triml k (str,off,len) = 
  if k < 0 then invalid_arg "Substring.triml: negative trim not allowed";
  if k > len then (str, off+len, 0) else (str, off+k, len-k)

let trimr k (str,off,len) = 
  if k < 0 then invalid_arg "Substring.triml: negative trim not allowed";
  if k > len then (str, off, 0) else (str, off, len-k)

let get k (str, off, len) = 
  if k < 0 then invalid_arg "Substring.get: negative index not allowed";
  if k > len then invalid_arg "Substring.get: index outside of substring";
  str.[off+k]

let size (_,_,len) = len
let length = size

let slice (str,off,len) off2 len2_opt = 
  if off2 < 0 then invalid_arg "Substring.slice: negative offset not allowed";
  let len2 = match len2_opt with None -> len-off2 | Some i -> i in
  if len2 + off2 > len then invalid_arg "Substring.slice: invalid slice";
  (str, off+off2, len2)

let concat ssl = 
  let len = List.fold_left (fun acc (_,_,l) ->acc+l) 0 ssl in
  let item = String.create len in
  let write = 
    let pos = ref 0 in
    fun (s,o,len) -> String.unsafe_blit s o item !pos len; pos := !pos + len
  in
  List.iter write ssl;
  item

let explode (str,off,len) = 
  let rec exp i l =
    if i < off then l else exp (i - 1) (str.[i] :: l) in
  exp (off+len) []

let is_prefix str1 (str2, off, len) =
  let l1 = String.length str1 in
  if l1 > len then false
  else 
    let rec loop i = 
      if i < 0 then true 
      else if str1.[i] <> str2.[off+i] then false
      else loop (i-1) in
    loop l1

let compare (str1, off1, len1) (str2, off2, len2) =
  let rec loop i = 
    if i > len1 then if i > len2 then 0 else 1
    else if i > len2 then -1 
    else 
      let c1 = str1.[off1+i] and c2 = str2.[off2+i] in
      if c1 > c2 then -1 
      else if c1 < c2 then 1 
      else loop (i+1)
  in 
  loop 0

(** not implemented: collate *)

let dropl p (str,off,len) =
  let i = ref 0 in
  while !i < len && p str.[off+ !i] do incr i; done;
  (str, off+ !i, len- !i)
  
let dropr p (str, off, len) =
  let i = ref len in
  while !i > 0 && p str.[off+ !i - 1] do decr i; done;
  (str, off, !i)

let takel p (str,off,len) =
  let i = ref 0 in
  while !i < len && p str.[off+ !i] do incr i; done;
  (str, off, !i)

let taker p (str, off, len) =
  let i = ref len in
  while !i > 0 && p str.[off+ !i - 1] do decr i; done;
  (str, off+ !i, len- !i)

let splitl p (str, off, len) = 
  let i = ref 0 in
  while !i < len && p str.[off+ !i] do incr i; done;
  (str, off, !i), (str, off+ !i, len- !i)
  
let splitr p (str, off, len) =
  let i = ref len in
  while !i > 0 && p str.[off+ !i - 1] do decr i; done;
  (str, off, !i), (str, off+ !i, len- !i)

let split_at k (str, off, len) =
  if k < 0 then invalid_arg "Substring.split_at: negative index";
  if k > len then invalid_arg "Substring.split_at: can't split past end of string";
  (str, off, k), (str, off+k, len-k)

(** not implemented: position *)

let span (str1, off1, len1) (str2, off2, len2) =
  if str1 != str2 then invalid_arg 
    "Substring.span: must be substrings of same parent";
  if off1 > off2 + len2 then invalid_arg 
    "Substring.span: first substring must not be to the right of the second";
  (str1, off1, off2+len2-off1)

let translate f (str,off,len) =
  BatString.init len (fun i -> f str.[off+i])

let tokens p (str,off,len) =
  let i = ref 0 and j = ref 0 and acc = BatRefList.empty () in
  while !j < len do
    while !i < len && p str.[off+ !i] do incr i; done;
    j := !i+1;
    while !j < len && not (p str.[off+ !j]) do incr j; done;
    BatRefList.push acc (str, !i, !j);
    i := !j+1;
  done;
  BatRefList.to_list acc

let fields p (str, off, len) =
  let i = ref 0 and j = ref 0 and acc = BatRefList.empty() in
  while !j < len do
    while !j < len && not (p str.[off+ !j]) do incr j; done;
    BatRefList.push acc (str, !i, !j);
    incr j; i := !j; 
  done;
  BatRefList.to_list acc

let fold_left f init (str, off, len) =
  let rec loop i result =
    if i = len then result
    else loop (i + 1) (f result str.[i])
  in
  loop off init

let fold_right f (str, off, len) init =
  let rec loop i result =
    if i = off then result
    else loop (i - 1) (f str.[i-1] result)
  in
  loop (off+len) init

let iter f (str, off, len) =
  for i = off to off+len-1 do
    f str.[i];
  done

let trim x = dropl BatChar.is_whitespace (dropr BatChar.is_whitespace x)

let split_on_char c (str, off, len) = 
  let rec loop acc last_pos pos =
    if pos = -1 then
      (str, 0, last_pos) :: acc
    else
      if str.[pos] = c then
        let pos1 = pos + 1 in
        let sub_str = str,pos1,(last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1)
      else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)
  
let split_on_pipe str = split_on_char '|' str;;
let split_on_dot str = split_on_char '.' str;;
let split_on_comma str = split_on_char ',' str;;
let split_on_slash str = split_on_char '/' str;;

let print oc ss = iter (fun c -> BatIO.write oc c) ss 
