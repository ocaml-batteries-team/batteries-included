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


(*Inlined to avoid circular dependencies between BatIO, ExtUTF8 and ExtString*)
(*
let string_splice s1 off len s2 = 
  let len1 = String.length s1 and len2 = String.length s2 in
  let out_len = len1 - len + len2 in
  let s = String.create out_len in
  String.blit s1 0 s 0 off; (* s1 before splice point *)
  String.blit s2 0 s off len2; (* s2 at splice point *)
  String.blit s1 (off+len) s (off+len2) (len1 - (off+len)); (* s1 after off+len *)
  s
*)

  open BatCamomile

  include UTF8
  module Case = CaseMap.Make(UTF8)

  external of_string_unsafe : string -> t = "%identity"
  external to_string_unsafe : t -> string = "%identity"
 
  type char_idx = int

  let length0 n =
    if n < 0x80 then 1 else
    if n < 0xc0 then invalid_arg "UTF8.length0 - Mid" else
    if n < 0xe0 then 2 else
    if n < 0xf0 then 3 else
    if n < 0xf8 then 4 else
    if n < 0xfc then 5 else
    if n < 0xfe then 6 else
    invalid_arg "UTF8.length0 - Reserved"

  (* non-start bytes have the form 0b10xx_xxxx *)
  let is_start_byte c = (Char.code c land 0b1100_0000) <> 0b10_000000

  module Byte : sig
    type b_idx(* = private int*)
    val of_int_unsafe : int -> b_idx 
    val to_int : b_idx -> int
    val next : t -> b_idx -> b_idx (* advance to the next unicode character *)
    val prev : t -> b_idx -> b_idx (* move to the previous unicode character *)
    val of_char_idx : t -> char_idx -> b_idx (* return position of the the nth character *)
    val at_end : t -> b_idx -> bool (* return true if b_idx is at the end of the given UTF8.t *)
    val out_of_range : t -> b_idx -> bool (* return true if b_idx is not a valid offset in the given UTF8.t *)
    val first : b_idx (* the first index of any UTF8.t (0) *)
    val last : t -> b_idx (* the byte index of the start of the last unicode char *)
    val move : t -> b_idx -> int -> b_idx (* go next or prev [int] times *)
  end = struct
    type b_idx = int
    external of_int_unsafe : int -> b_idx = "%identity"
    external to_int : b_idx -> int = "%identity"
    let next us bi = bi + (length0 (Char.code us.[bi]))
    let prev us bi = 
      if bi > String.length us || bi < 0 then invalid_arg "UTF8.Byte.prev: Byte index not within string";
      let rec loop bi =
	if bi < 0 then (-1)
	else if is_start_byte us.[bi] then bi
	else loop (bi-1)
      in
      loop (bi-1)
    let first = 0
    let last us = prev us (String.length us)
    let of_char_idx us ci = move us first ci
    let at_end us bi = bi = String.length us
    let out_of_range us bi = bi < 0 || bi >= String.length us
    let move us bi n = (* faster moving positive than negative n *)
      let bi = ref bi in
      let step = if n > 0 then next else prev in
      for j = 1 to abs n do bi := step us !bi done;
      !bi
  end

  let nth = Byte.of_char_idx
  let out_of_range = Byte.out_of_range
  let look s b_idx = look s (Byte.to_int b_idx)

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
    
  let rec length_aux s ci bi =
    if Byte.at_end s bi then ci 
    else length_aux s (ci + 1) (Byte.next s bi)
  
  let length s = length_aux s 0 Byte.first
    
  let enum us =
    let rec make i =
      BatEnum.make
	~next:(fun () ->
		 if Byte.at_end us !i then
                   raise BatEnum.No_more_elements
		 else
		   look us (BatRef.post i (Byte.next us))
              )
	~count:(fun () -> length_aux us 0 !i)
	~clone:(fun () -> make (BatRef.copy i))
    in
    make (ref Byte.first)
      
  let of_enum e =
    let buf = Buffer.create 16 in
      BatEnum.iter (fun c -> Buffer.add_string buf (of_char c)) e;
      adopt (Buffer.contents buf)

  let rec rev_length_aux s ci bi =
    if Byte.out_of_range s bi then ci 
    else length_aux s (ci + 1) (Byte.prev s bi)
  


  let backwards us =
    let rec make i =
      BatEnum.make
	~next:(fun () ->
		 if Byte.out_of_range us !i then
                   raise BatEnum.No_more_elements
		 else
                   look us (BatRef.post i (Byte.prev us))
              )
  ~count:(fun () -> rev_length_aux us 0 !i)
  ~clone:(fun () -> make (BatRef.copy i))
    in
    make (ref (Byte.last us))
      
  let of_backwards e =
    of_enum (BatList.enum (BatList.of_backwards e))

  let unsafe_get = get
 
  let copy_set us cpos c =
    let ipos = Byte.of_char_idx us cpos in 
    let jpos = Byte.next us ipos in
    let i = Byte.to_int ipos
    and j = Byte.to_int jpos in
    BatString.splice us i (j-i) (of_char c)
     
  let sub s n len =
    let ipos = Byte.of_char_idx s n in
    let jpos = Byte.move s ipos len in
    let i = Byte.to_int ipos
    and j = Byte.to_int jpos in
    String.sub s i (j-i)
      
  let rec search_head s i =
    if i >= String.length s then i else
      let n = Char.code (String.unsafe_get s i) in
      if n < 0x80 || n >= 0xc2 then i else
  search_head s (i + 1)
   
  let rec iter_aux proc s i =
    if Byte.out_of_range s i then () else
      let u = look s i in
      proc u;
      iter_aux proc s (Byte.next s i)
  
  let iter proc s = iter_aux proc s Byte.first
    
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

  let filter f us =
    let b = Buf.create (length us) in
    iter (fun c -> if f c then Buf.add_char b c) us;
    Buf.contents b

  let index us ch =
    let rec aux ci bi =
      if Byte.out_of_range us bi then raise Not_found;
      if look us bi = ch then ci
      else aux (ci+1) (Byte.next us bi)
    in
    aux 0 (Byte.first)

  let rindex us ch =
    let rec aux ci bi =
      if Byte.out_of_range us bi then raise Not_found;
      if look us bi = ch then ci
      else aux (ci-1) (Byte.prev us bi)
    in
    aux 0 (Byte.last us)

  let rec contains_aux step bi us ch =
    if Byte.out_of_range us bi then false
    else if look us bi = ch then true
    else contains_aux step (step us bi) us ch

  let contains us ch = contains_aux Byte.next Byte.first us ch

  let contains_from us ch bi = contains_aux Byte.next bi us ch

  let rcontains_from us ch bi = contains_aux Byte.prev bi us ch

  let escaped us = String.escaped us (* FIXME: think through whether this is correct for UTF8 *)

  let compare s1 s2 = Pervasives.compare s1 s2
    
  let copy = String.copy

  let print out t = BatInnerIO.nwrite out t
  let t_printer paren out t =
    BatInnerIO.nwrite out "u\"";
    print out (escaped t);
    BatInnerIO.write out '"'

  let uppercase c = Case.uppercase c
  let lowercase c = Case.lowercase c

  let casefold s = Case.casefolding s
