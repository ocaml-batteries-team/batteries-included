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
(*$T
   to_string (empty () ) = ""
*)

let to_string (s,o,l) = String.sub s o l
(*$T to_string
   to_string (substring "foobar" 1 3) = "oob"
   to_string (substring "" 0 0) = ""
*)

let of_string s = s, 0, String.length s
(*$T of_string
   of_string "foo" = substring "foo" 0 3
   of_string "" = empty ()
*)

let make len c = String.make len c, 0, len
(*$T make
  make 3 'f' = substring "fff" 0 3
  (make 3 'f' = substring "ffff" 0 3) = false
  make 0 ' ' = empty ()
*)

let create len = String.make len '\000', 0, len
(*$T create
   create 0 = empty ()
*)

let equal (s1,o1,l1) (s2,o2,l2) =
  if l1 <> l2 then false
  else BatReturn.label (fun label ->
      for i = 0 to l1-1 do
        if s1.[i+o1] <> s1.[i+o2] then BatReturn.return label false
      done; true)
(*$T equal
   equal (of_string "abc") (of_string "abc") = true
   equal (substring "aba" 0 1) (substring "aba" 2 1) = true
   equal (substring "aba" 1 1) (substring "aba" 2 1) = false
*)

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
    Buffer.add_substring buf tmp 0 !n;
  done;
  Buffer.contents buf, 0, Buffer.length buf

let substring str off len =
  let sl = String.length str in
  if off < 0 then invalid_arg "Substring.substring: negative offset not allowed";
  if len < 0 then invalid_arg "Substring.substring: negative length not allowed";
  if off + len > sl then invalid_arg "Substring.substring: offset + length past end of string";
  (str,off,len)
(*$T substring
   (try (substring "foo" (-1) 2) with Invalid_argument "Substring.substring: negative offset not allowed" -> (substring "foo" 0 2)) = (substring "foo" 0 2)
   (try (substring "foo" 0 (-1)) with Invalid_argument "Substring.substring: negative length not allowed" -> (substring "foo" 0 2)) = (substring "foo" 0 2)
   (try (substring "foo" 0 10) with Invalid_argument "Substring.substring: offset + length past end of string" -> (substring "foo" 0 2)) = (substring "foo" 0 2)
  to_string (substring "foobar" 1 3) = "oob"
*)

let unsafe_substring str off len =
  (str, off, len)
(*$T unsafe_substring
   (unsafe_substring "foobar" 1 3) = (substring "foobar" 1 3)
*)

let extract s o = function
    Some len -> substring s o len
  | None -> substring s o (String.length s - o)
(*$T extract
   extract "foobar" 1 None = substring "foobar" 1 5
   extract "foobar" 1 (Some 3) = substring "foobar" 1 3
*)

let all = of_string

let base s = s
(*$T base
  base (substring "foobar" 1 3) = ("foobar", 1, 3)
*)

let is_empty (_,_,len) = len = 0
(*$T is_empty
   is_empty (substring "foobar" 0 0) = true
   is_empty (substring "foobar" 0 2) = false
*)

let getc (str,off,len) =
  if len = 0 then None else Some (str.[off], (str, off+1, len-1))
(*$T getc
   getc (substring "foobar" 1 3) = Some ('o', substring "foobar" 2 2)
   getc (empty ()) = None
*)

let first (str,off,len) = if len = 0 then None else Some str.[off]
(*$T first
  first (substring "foobar" 1 3) = Some 'o'
  first (substring "foobar" 0 0) = None
*)

let triml k (str,off,len) =
  if k < 0 then invalid_arg "Substring.triml: negative trim not allowed";
  if k > len then (str, off+len, 0) else (str, off+k, len-k)
(*$T triml
   triml 10 ("foobar" |> of_string ) |> to_string = ""
   triml 0 (substring "foobar" 1 3) = (substring "foobar" 1 3)
   triml 1 (substring "foobar" 1 3) = (substring "foobar" 2 2)
   (try (triml (-5) ("foobar" |> of_string)) with Invalid_argument _ -> substring "foo" 0 3) = substring "foo" 0 3
*)

let trimr k (str,off,len) =
  if k < 0 then invalid_arg "Substring.triml: negative trim not allowed";
  if k > len then (str, off, 0) else (str, off, len-k)

(*$T trimr
   trimr 10 ("foobar" |> of_string ) |> to_string = ""
   trimr 0 (substring "foobar" 1 3) = (substring "foobar" 1 3)
   trimr 1 (substring "foobar" 1 3) = (substring "foobar" 1 2)
   (try (trimr (-5) ("foobar" |> of_string)) with Invalid_argument _ -> substring "foo" 0 3) = substring "foo" 0 3
*)

let get (str, off, len) k =
  if k < 0 then invalid_arg "Substring.get: negative index not allowed";
  if k > len then invalid_arg "Substring.get: index outside of substring";
  str.[off+k]
(*$T get
   get (substring "foobar" 1 3) 0 = 'o'
   (try (get (substring "foobar" 1 3) (-1)) with Invalid_argument "Substring.get: negative index not allowed" -> 'a') = 'a'
   (try (get (substring "foobar" 1 3) 15) with Invalid_argument "Substring.get: index outside of substring" -> 'a') = 'a'
*)

let size (_,_,len) = len
let length = size
(*$T size
   size (substring "foobar" 0 0) = 0
   size (substring "foobar" 1 3) = 3
*)

let slice (str,off,len) off2 len2_opt =
  if off2 < 0 then invalid_arg "Substring.slice: negative offset not allowed";
  let len2 = match len2_opt with None -> len-off2 | Some i -> i in
  if len2 + off2 > len then invalid_arg "Substring.slice: invalid slice";
  (str, off+off2, len2)
(*$T slice
   (try (slice (substring  "foobar" 1 3) (-1) None) with Invalid_argument "Substring.slice: negative offset not allowed" -> empty ()) = empty ()
   (try (slice (substring  "foobar" 1 3) 0 (Some 20)) with Invalid_argument "Substring.slice: invalid slice" -> empty ()) = empty ()
   slice (substring "foobar" 1 4) 2 None = (substring "foobar" 3 2)
   is_empty (slice (substring "foobar" 0 3) 3 None) = true
*)

let concat ssl =
  let len = List.fold_left (fun acc (_,_,l) ->acc+l) 0 ssl in
  let item = Bytes.create len in
  let write =
    let pos = ref 0 in
    fun (s,o,len) -> String.unsafe_blit s o item !pos len; pos := !pos + len
  in
  List.iter write ssl;
  item
(*$T concat
   concat [empty ()] = ""
   concat [substring "foobar" 1 3; empty ()] = "oob"
   concat [empty (); substring "foobar" 1 3] = "oob"
   concat [substring "foobar" 3 3 ; substring "foobar" 0 3] = "barfoo"
*)

let explode (str,off,len) =
  let rec exp i l =
    if i < off then l else exp (i - 1) (str.[i] :: l) in
  exp (off+len-1) []
(*$T explode
   explode (substring "foobar" 1 3) = ['o';'o';'b']
   explode (empty ()) = []
*)

let is_prefix str1 (str2, off, len) =
  let l1 = String.length str1 in
  if l1 > len then false
  else
    let rec loop i =
      if i < 0 then true
      else if str1.[i] <> str2.[off+i] then false
      else loop (i-1) in
    loop (pred l1)
(*$T is_prefix
   is_prefix "foo" (empty ()) = false
   is_prefix "oob" (substring "foobar" 1 4) = true
   is_prefix "" (empty ()) = true
*)

let compare (str1, off1, len1) (str2, off2, len2) =
  let rec loop i =
    if i >= len1 then if i >= len2 then 0 else -1
    else if i >= len2 then 1
    else
      let c1 = str1.[off1+i] and c2 = str2.[off2+i] in
      if c1 > c2 then 1
      else if c1 < c2 then -1
      else loop (i+1)
  in
  loop 0
(*$T compare
  compare (empty ()) (empty ()) = 0
  compare (empty ()) (substring "foobar" 1 3) = -1
  compare (substring "foobar" 1 3) (empty ()) = 1
  compare (substring "foobar" 1 3) (substring "barfoo" 1 3) = 1
*)

let index_from (str, off, len) i c =
  let rec aux k =
    if k = len then raise Not_found
    else if str.[off+k] = c then k
    else aux (k+1)
  in
  if i > len || i < 0 then invalid_arg "Substring.index_from"
  else aux i
(*$T index_from
   (try (index_from (substring "foobar" 1 3) 2 'o') with Not_found -> 0) = 0
   (try (index_from (substring "foobar" 1 3) (-3) 'o') with Invalid_argument "Substring.index_from" -> 0) = 0
   (try (index_from (substring "foobar" 1 3) 20 'o') with Invalid_argument "Substring.index_from" -> 0) = 0
   index_from (substring "foobar" 1 3) 1 'b' = 2
*)

let index sus c = index_from sus 0 c
(*$T index
  (try (index (substring "foobar" 1 3) 'y') with Not_found -> 0) = 0
   index (substring "foobar" 1 3) 'b' = 2
*)

let rindex_from (str, off, len) i c =
  let rec aux k =
    if k < 0 then raise Not_found
    else if str.[off+k] = c then k
    else aux (k-1)
  in
  if i > len || i < 0 then invalid_arg "Substring.rindex_from"
  else aux i
(*$T rindex_from
   (try (rindex_from (substring "foobar" 1 3) 2 'y') with Not_found -> 0) = 0
   (try (rindex_from (substring "foobar" 1 3) (-3) 'o') with Invalid_argument "Substring.rindex_from" -> 0) = 0
   (try (rindex_from (substring "foobar" 1 3) 20 'o') with Invalid_argument "Substring.rindex_from" -> 0) = 0
   rindex_from (substring "foobar" 1 3) 3 'b' = 2
*)

let rindex sus c = rindex_from sus (size sus - 1) c
(*$T rindex
  (try (rindex (substring "foobar" 1 3) 'y') with Not_found -> 0) = 0
   rindex (substring "foobar" 1 3) 'b' = 2
*)


let contains ss c = try ignore (index ss c); true with Not_found -> false
(*$T contains
   contains (of_string "foobar") 'c' = false
   contains (of_string "foobar") 'o' = true
   contains (of_string "") 'Z' = false
*)

(** not implemented: collate *)

let dropl p (str,off,len) =
  let i = ref 0 in
  while !i < len && p str.[off+ !i] do incr i; done;
  (str, off+ !i, len- !i)
(*$T dropl
  dropl (fun c -> c = 'f') (substring "foobar" 0 6) = (substring "foobar" 1 5)
  dropl (fun c -> c = 'o') (substring "foobar" 0 6) = (substring "foobar" 0 6)
   dropl (fun c -> c = 'o'||c='f') (substring "foobar" 0 6) = (substring "foobar" 3 3)
   dropl (fun c -> c = 'o'||c='f') (empty()) = empty ()
*)

let dropr p (str, off, len) =
  let i = ref len in
  while !i > 0 && p str.[off+ !i - 1] do decr i; done;
  (str, off, !i)
(*$T dropr
  dropr (fun c -> c = 'r') (substring "foobar" 0 6) = (substring "foobar" 0 5)
  dropr (fun c -> c = 'o') (substring "foobar" 0 6) = (substring "foobar" 0 6)
   dropr (fun c -> c = 'a'||c='r') (substring "foobar" 0 6) = (substring "foobar" 0 4)
   dropr (fun c -> c = 'o'||c='f') (empty()) = empty ()
*)

let takel p (str,off,len) =
  let i = ref 0 in
  while !i < len && p str.[off+ !i] do incr i; done;
  (str, off, !i)
(*$T takel
   takel (fun c -> c = 'f' || c = 'o') (substring "foobar" 0 6) = (substring "foobar" 0 3)
   (takel (fun c -> c = 'x') (substring "foobar" 0 6) |> is_empty) = true
   takel (fun c -> c = 'x') (empty ()) = empty ()
*)

let taker p (str, off, len) =
  let i = ref len in
  while !i > 0 && p str.[off+ !i - 1] do decr i; done;
  (str, off+ !i, len- !i)
(*$T taker
   taker (fun c -> c = 'r' || c = 'a') (substring "foobar" 0 6) = substring "foobar" 4 2
   taker (fun c -> c = 'b' || c = 'c') (substring "foobar" 0 6) |> is_empty = true
*)

let splitl p (str, off, len) =
  let i = ref 0 in
  while !i < len && p str.[off+ !i] do incr i; done;
  (str, off, !i), (str, off+ !i, len- !i)
(*$T splitl
   splitl (fun c -> c = 'f') (substring "foobar" 0 6) = (substring "foobar" 0 1, substring "foobar" 1 5)
   splitl (fun c -> c = 'f' || c = 'o') (substring "foobar" 0 6) = (substring "foobar" 0 3, substring "foobar" 3 3)
   splitl (fun c -> c = 'f') (empty ()) = (empty (), empty ())
   splitl (fun c -> c = 'o' || c = 'b') (substring "foobar" 0 6) = (substring "foobar" 0 0, substring "foobar" 0 6)
*)

let splitr p (str, off, len) =
  let i = ref len in
  while !i > 0 && p str.[off+ !i - 1] do decr i; done;
  (str, off, !i), (str, off+ !i, len- !i)
(*$T splitr
   splitr (fun c -> c = 'b' || c = 'o') (substring "foobar" 0 6) = (substring "foobar" 0 6, substring "foobar" 6 0)
   splitr (fun c -> c = 'b' || c = 'o') (substring "foobar" 0 6) = (substring "foobar" 0 6, substring "foobar" 6 0)
   splitr (fun c -> c = 'b' || c = 'a' || c = 'r') (substring "foobar" 0 6) = (substring "foobar" 0 3, substring "foobar" 3 3)
   splitr (fun c -> c = 'y') (empty ()) = (empty (), empty ())
*)

let split_at k (str, off, len) =
  if k < 0 then invalid_arg "Substring.split_at: negative index";
  if k > len then invalid_arg "Substring.split_at: can't split past end of string";
  (str, off, k), (str, off+k, len-k)
(*$T split_at
   (try (Some (split_at (-1) (empty ()))) with Invalid_argument "Substring.split_at: negative index" -> None ) = None
   (try (Some (split_at 12 (substring "foobar" 0 6))) with Invalid_argument "Substring.split_at: can't split past end of string" -> None ) = None
   split_at 3 (substring "foobar" 0 6) = (substring "foobar" 0 3, substring "foobar" 3 3)
   split_at 0 (empty ()) = (empty (), empty ())
*)

(** not implemented: position *)

let span (str1, off1, _len1) (str2, off2, len2) =
  if str1 <> str2 then invalid_arg
      "Substring.span: must be substrings of same parent";
  if off1 > off2 + len2 then invalid_arg
      "Substring.span: first substring must not be to the right of the second";
  (str1, off1, off2+len2-off1)
(*$T span
   (try (span (substring "foo" 0 3) (substring "bar" 0 3)) with Invalid_argument  "Substring.span: must be substrings of same parent" -> empty ()) = empty ()
   (try (span (substring "foobar" 4 2) (substring "foobar" 0 3)) with Invalid_argument "Substring.span: first substring must not be to the right of the second" -> empty ()) = empty ()
   span (substring "foobar" 0 3) (substring "foobar" 3 3) = (substring "foobar" 0 6)
   span (substring "foobar" 3 3) (substring "foobar" 0 3) = (substring "foobar" 3 0)
*)

let translate f (str,off,len) =
  BatString.init len (fun i -> f str.[off+i])
(*$T translate
  translate (function 'o' -> 'a' | x -> x)(substring "foobar" 1 3) = "aab"
  translate (fun x -> x) (empty ()) = ""
*)

let tokens p (str,off,len) =
  let i = ref 0 and j = ref 0 and acc = BatRefList.empty () in
  while !j < len do
    while !i < len && p str.[off+ !i] do incr i; done;
    j := !i+1;
    while !j < len && not (p str.[off+ !j]) do incr j; done;
    BatRefList.push acc (str, !i, !j - !i);
    i := !j+1;
  done;
  BatRefList.to_list acc
(*$T tokens
   tokens (fun x -> x = ';') (substring "foo;bar" 0 7) = [substring "foo;bar" 4 3; substring "foo;bar" 0 3]
   tokens (fun x -> x = ';') (substring "foo;;bar" 0 8) = [substring "foo;;bar" 5 3; substring "foo;;bar" 0 3]
   tokens (fun x -> x = ';') (empty ()) = []
*)

let fields p (str, off, len) =
  let i = ref 0 and j = ref 0 and acc = BatRefList.empty() in
  while !j < len do
    while !j < len && not (p str.[off+ !j]) do incr j; done;
    BatRefList.push acc (str, !i, !j - !i);
    incr j; i := !j;
  done;
  BatRefList.to_list acc
(*$T fields
   fields (fun x -> x = ';') (substring "foo;;bar" 0 8) = [substring "foo;;bar" 5 3; substring "foo;;bar" 4 0; substring "foo;;bar" 0 3]
   fields (fun x -> x = ';') (substring "foo;bar" 0 7) = [substring "foo;bar" 4 3; substring "foo;bar" 0 3]
   fields (fun x -> x = ';') (empty ()) = []
*)

let fold_left f init (str, off, len) =
  let rec loop i result =
    if (i-off) = len then result
    else loop (i + 1) (f result str.[i])
  in
  loop off init
(*$T fold_left
   fold_left (fun a c -> c::a) [] (substring "foobar" 1 3)=['b';'o';'o']
   fold_left (fun a _ -> a+1) 0 (empty ()) = 0
*)

let fold_lefti f init (str, off, len) =
  let rec loop i result =
    if (i-off) = len then result
    else loop (i + 1) (f result (i-off) str.[i])
  in loop off init
(*$T fold_lefti
   fold_lefti (fun a i _ -> a+i) 0 (substring "foobar" 1 3) = 3
   fold_lefti (fun a i _ -> a+i) 1 (empty ()) = 1
*)

let fold_right f (str, off, len) init =
  let rec loop i result =
    if i = off then result
    else loop (i - 1) (f str.[i-1] result)
  in
  loop (off+len) init
(*$T fold_right
   fold_right (fun c a -> c::a) (substring "foobar" 0 3) []=['f';'o';'o']
   fold_right (fun c a -> c::a) (empty ()) [] = []
*)

let fold_righti f (str, off, len) init =
  let rec loop i result =
    if i = off then result
    else
      let i' = i - 1 in
      loop (i - 1) (f (i' - off) str.[i'] result)
  in loop (off+len) init

(*$T fold_righti
   fold_righti (fun i _ a -> a+i) (substring "foobar" 1 4) 0 = 6
   fold_righti (fun i _ a -> a+i) (empty ()) 12 = 12
*)

let iter f (str, off, len) =
  for i = off to off+len-1 do
    f str.[i];
  done

let iteri f (str, off, len) =
  for i = 0 to len-1 do f i str.[i+off] done

let trim x = dropl BatChar.is_whitespace (dropr BatChar.is_whitespace x)
(*$T trim
   trim (empty ()) = empty ()
   trim (of_string " foobar ") = substring " foobar " 1 6
   trim (of_string "foobar") = of_string "foobar"
*)

let split_on_char c (str, off, len) =
  let rec loop acc last_pos pos =
    if pos = off - 1 then
      (str, off, last_pos - off) :: acc
    else
    if str.[pos] = c then
      let pos1 = pos + 1 in
      let sub_str = str,pos1,(last_pos - pos1) in
      loop (sub_str :: acc) pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] (off+len) (off + len - 1)
(*$T split_on_char
   split_on_char ';' (of_string "foo;bar;oof") = [substring "foo;bar;oof" 0 3;substring "foo;bar;oof" 4 3; substring "foo;bar;oof" 8 3]
   split_on_char ';' (of_string "foo;;bar;oof") = [substring "foo;;bar;oof" 0 3; substring "foo;;bar;oof" 4 0; substring "foo;;bar;oof" 5 3; substring "foo;;bar;oof" 9 3]
   split_on_char ';' (empty ()) = [empty ()]
*)

let split_on_pipe str = split_on_char '|' str
(*$T split_on_pipe
   split_on_pipe (of_string "foo|bar|oof") = [substring "foo|bar|oof" 0 3;substring "foo|bar|oof" 4 3; substring "foo|bar|oof" 8 3]
   split_on_pipe (empty ()) = [empty ()]
*)

let split_on_dot str = split_on_char '.' str
(*$T split_on_dot
   split_on_dot (of_string "foo.bar.oof") = [substring "foo.bar.oof" 0 3;substring "foo.bar.oof" 4 3; substring "foo.bar.oof" 8 3]
   split_on_dot (empty ()) = [empty ()]
*)

let split_on_comma str = split_on_char ',' str
(*$T split_on_comma
   split_on_comma (of_string "foo,bar,oof") = [substring "foo,bar,oof" 0 3;substring "foo,bar,oof" 4 3; substring "foo,bar,oof" 8 3]
   split_on_comma (empty ()) = [empty ()]
*)

let split_on_slash str = split_on_char '/' str
(*$T split_on_slash
   split_on_slash (of_string "foo/bar/oof") = [substring "foo/bar/oof" 0 3;substring "foo/bar/oof" 4 3; substring "foo/bar/oof" 8 3]
   split_on_slash (empty ()) = [empty ()]
*)

let rec enum (str, off, len) =
  let last_element = off + len - 1 in
  let i = ref off in
  BatEnum.make
    ~next:(fun () ->
      if !i > last_element then raise BatEnum.No_more_elements
      else str.[BatRef.post_incr i] )
    ~count:(fun () -> len - !i)
    ~clone:(fun () -> enum (str, !i, len - !i))
(*$T enum
   Enum.compare Char.compare (enum (of_string "foo")) (String.enum "foo") = 0
   Enum.compare Char.compare (enum (of_string "foo")) (String.enum "fob") <> 0
   Enum.compare Char.compare (enum (empty ())) (String.enum "") = 0
   Enum.compare Char.compare (enum (empty ())) (String.enum "P") <> 0
*)
let print oc ss = iter (fun c -> BatIO.write oc c) ss
