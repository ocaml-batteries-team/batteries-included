(* $Id: uTF8.ml,v 1.11 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

open ExtString

type t = string
type index = int
  
let look s i =
  let n' =
    let n = Char.code s.[i] in
    if n < 0x80 then n else
    if n <= 0xdf then
      (n - 0xc0) lsl 6 lor (0x7f land (Char.code s.[i + 1]))
    else if n <= 0xef then
      let n' = n - 0xe0 in
      let m0 = Char.code s.[i + 2] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)
    else if n <= 0xf7 then
      let n' = n - 0xf0 in
      let m0 = Char.code s.[i + 3] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)     
    else if n <= 0xfb then
      let n' = n - 0xf8 in
      let m0 = Char.code s.[i + 4] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 3)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)     
    else if n <= 0xfd then
      let n' = n - 0xfc in
      let m0 = Char.code s.[i + 5] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 3)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 4)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)
    else invalid_arg "UTF8.look"
  in
  UChar.of_int n'

let rec search_head s i =
  if i >= String.length s then i else
  let n = Char.code (String.unsafe_get s i) in
  if n < 0x80 || n >= 0xc2 then i else
  search_head s (i + 1)

  let length0 n  =
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

let unsafe_get = get

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
    
let copy_set s n c = 
  let i = nth s n in let j = next s i in
  String.splice s i (j-i) (of_char c)

(* shouldn't do this - requires resizing string
let unsafe_buf_set s str_end n c = 
  let s1 = of_char c and i = nth s n in 
  let j = next s i 
  and j' = i + String.length s1 in
  let e' = str_end - j + j'  in
  if e' > String.length s then failwith "Buffer too small";
  String.blit s j s j' (e-j);
  String.blit s1 0 s i (String.length s1);
  e'
*)

let sub s n len =
  let i = nth s n in 
  let j = move s i len in
  String.sub s i (j-i)

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

let compare s1 s2 = Pervasives.compare s1 s2

let copy = String.copy

let append s1 s2 = s1 ^ s2

let empty = ""

let join l = String.concat "" l

exception Malformed_code

let validate s =
  let rec trail c i a =
    if c = 0 then a else
    if i >= String.length s then raise Malformed_code else
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 || n >= 0xc0 then raise Malformed_code else
    trail (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
  let rec main i =
    if i >= String.length s then () else
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 then main (i + 1) else
    if n < 0xc2 then raise Malformed_code else
    if n <= 0xdf then 
      if trail 1 (i + 1) (n - 0xc0) < 0x80 then raise Malformed_code else 
      main (i + 2)
    else if n <= 0xef then 
      if trail 2 (i + 1) (n - 0xe0) < 0x800 then raise Malformed_code else 
      main (i + 3)
    else if n <= 0xf7 then 
      if trail 3 (i + 1) (n - 0xf0) < 0x10000 then raise Malformed_code else
      main (i + 4)
    else if n <= 0xfb then 
      if trail 4 (i + 1) (n - 0xf8) < 0x200000 then raise Malformed_code else
      main (i + 5)
    else if n <= 0xfd then 
      let n = trail 5 (i + 1) (n - 0xfc) in
      if n lsr 16 < 0x400 then raise Malformed_code else
      main (i + 6)
    else raise Malformed_code in
  main 0

(* End Yoriyuki's code *)

let of_string s = validate s; s

let to_string s = s

let to_enum us = 
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
  let buf = Buffer.create 10 in
  Enum.iter (fun c -> Buffer.add_string buf (of_char c)) e;
  Buffer.contents buf


