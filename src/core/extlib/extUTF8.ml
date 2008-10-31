(*
 * ExtUTF8 - Additional functions for UTF8 string manipulation
 * Copyright (C) 2008 Edgar Friendly, David Teller
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

open Sexplib
open Conv
TYPE_CONV_PATH "Batteries.Data.Text" (*For Sexplib, Bin-prot...*)

(*Inlined to avoid circular dependencies between IO, ExtUTF8 and ExtString*)
let string_splice s1 off len s2 = 
  let len1 = String.length s1 and len2 = String.length s2 in
  let out_len = len1 - len + len2 in
  let s = String.create out_len in
  String.blit s1 0 s 0 off; (* s1 before splice point *)
  String.blit s2 0 s off len2; (* s2 at splice point *)
  String.blit s1 (off+len) s (off+len2) (len1 - (off+len)); (* s1 after off+len *)
  s


module UTF8 = struct
  open CamomileLibrary
  open ExtString
  open ExtList
  include CamomileLibrary.UTF8
  module Case = CaseMap.Make(CamomileDefaultConfig)(UTF8)

  external of_string_unsafe : string -> t = "%identity"
  external to_string_unsafe : t -> string = "%identity"
 
  let append s1 s2 = s1 ^ s2
    
  let empty = ""
    
  let concat = String.concat
  let join   = concat
    
  let of_char u =
    let masq = 0b111111 in
    let k = UChar.uint_code u in
    if k < 0 || k >= 0x4000000 then begin
      let s = String.create 6 in
      s.[0] <- (Char.chr (0xfc + (k lsr 30)));
      s.[1] <- (Char.unsafe_chr (0x80 lor ((k lsr 24) land masq)));
      s.[2] <- (Char.unsafe_chr (0x80 lor ((k lsr 18) land masq)));
      s.[3] <- (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
      s.[4] <- (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
      s.[5] <- (Char.unsafe_chr (0x80 lor (k land masq)));
      s
    end else if k <= 0x7f then
      String.make 1 (Char.unsafe_chr k)
    else if k <= 0x7ff then begin
      let s = String.create 2 in
      s.[0] <- (Char.unsafe_chr (0xc0 lor (k lsr 6)));
      s.[1] <- (Char.unsafe_chr (0x80 lor (k land masq)));
      s
    end else if k <= 0xffff then begin
      let s = String.create 3 in
      s.[0] <- (Char.unsafe_chr (0xe0 lor (k lsr 12)));
      s.[1] <- (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
      s.[2] <- (Char.unsafe_chr (0x80 lor (k land masq)));
      s
    end else if k <= 0x1fffff then begin
      let s = String.create 4 in
      s.[0] <- (Char.unsafe_chr (0xf0 + (k lsr 18)));
      s.[1] <- (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
      s.[2] <- (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
      s.[3] <- (Char.unsafe_chr (0x80 lor (k land masq)));
      s
    end else begin
      let s = String.create 5 in
      s.[0] <- (Char.unsafe_chr (0xf8 + (k lsr 24)));
      s.[1] <- (Char.unsafe_chr (0x80 lor ((k lsr 18) land masq)));
      s.[2] <- (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
      s.[3] <- (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
      s.[4] <- (Char.unsafe_chr (0x80 lor (k land masq)));
      s
    end
      
  let make i c =
    if i = 1 then of_char c
    else
      let s0 = of_char c in
      let l0 = String.length s0 in
      let s = String.create (i * l0) in
      for j = 0 to i-1 do
  String.blit s0 0 s (j * l0) l0
      done;
      s
  
  let of_string s = validate s; String.copy s

  let to_string s = String.copy s

  let adopt     s = validate s; s
    
  let enum us =
    let l = length us in
    let rec make i =
      Enum.make
	~next:(fun () ->
		 if !i = l then
                   raise Enum.No_more_elements
		 else
                   let p = !i in
                     i := next us !i;
                     look us p
              )
  ~count:(fun () -> l - !i)
  ~clone:(fun () -> make (ref !i))
    in
    make (ref 0)
      
  let of_enum e =
    let buf = Buffer.create 16 in
      Enum.iter (fun c -> Buffer.add_string buf (of_char c)) e;
      adopt (Buffer.contents buf)


  let backwards us =
    let rec make i =
      Enum.make
	~next:(fun () ->
		 if !i < 0 then
                   raise Enum.No_more_elements
		 else
                   let p = !i in
                     i := prev us !i;
                     look us p
              )
  ~count:(fun () -> !i)
  ~clone:(fun () -> make (Ref.copy i))
    in
    make (ref (length us - 1))
      
  let of_backwards e =
    of_enum (List.enum (List.of_backwards e))


  let unsafe_get = get
 
  let copy_set s n c =
    let i = nth s n in let j = next s i in
    string_splice s i (j-i) (of_char c)
     
  let sub s n len =
    let i = nth s n in
    let j = move s i len in
    String.sub s i (j-i)
      
  let rec search_head s i =
    if i >= String.length s then i else
      let n = Char.code (String.unsafe_get s i) in
      if n < 0x80 || n >= 0xc2 then i else
  search_head s (i + 1)
 
  let length0 n =
    if n < 0x80 then 1 else
    if n < 0xc0 then invalid_arg "UTF8.length0 - Mid" else
    if n < 0xe0 then 2 else
    if n < 0xf0 then 3 else
    if n < 0xf8 then 4 else
    if n < 0xfc then 5 else
    if n < 0xfe then 6 else
    invalid_arg "UTF8.length0"
 
  let next s i =
    let n = Char.code s.[i] in
    try i + (length0 n)
    with Invalid_argument "UTF8.length0 - Mid" -> search_head s (i+1)
      | Invalid_argument _ -> invalid_arg "UTF8.next"
  
  let length_at s i =
    let n = Char.code (String.unsafe_get s i) in
    length0 n
      
  let rec length_aux s c i =
    if i >= String.length s then c else
      let k = length_at s i in
      length_aux s (c + 1) (i + k)
  
  let length s = length_aux s 0 0
    
  let rec iter_aux proc s i =
    if i >= String.length s then () else
      let u = look s i in
      proc u;
      iter_aux proc s (next s i)
  
  let iter proc s = iter_aux proc s 0
    
  let init i f = (* Buf from CamomileLibrary.UTF8 *)
    let b = Buf.create i in
    for j = 0 to i-1 do
      Buf.add_char b (f j)
    done;
    Buf.contents b

  let map f us = 
    let b = Buf.create (length us) in
    iter (fun c -> Buf.add_char b (f c)) us;
    Buf.contents b

  let filter_map f us = 
    let b = Buf.create (length us) in
    iter (fun c -> match f c with None -> () | Some c -> Buf.add_char b c) us;
    Buf.contents b

  let rec index_aux us c char_idx byte_idx =
    if look us byte_idx = c then char_idx
    else index_aux us c (char_idx+1) (next us byte_idx)

  let index us c = (* relies on exception at end of string *)
    try 
      index_aux us c 0 0
    with 
	Invalid_argument "UTF8.next" -> raise Not_found



  let compare s1 s2 = Pervasives.compare s1 s2
    
  let copy = String.copy

  let sexp_of_t = sexp_of_string
  let t_of_sexp = string_of_sexp


  let print out t = InnerIO.nwrite out (to_string t)

  let uppercase c = Case.uppercase c
  let lowercase c = Case.lowercase c
end
