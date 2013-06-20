(*
 * BatString - Additional functions for string manipulations.
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller
 *               2008 Edgar Friendly
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

include String

let compare = String.compare
(*$T compare
   compare "FOO" "bar" = -1
*)

let equal a b = String.compare a b = 0
let ord = BatOrd.ord String.compare


let init len f =
  let s = create len in
  for i = 0 to len - 1 do
    unsafe_set s i (f i)
  done;
  s

(*$T init
   init 5 (fun i -> Char.chr (i + int_of_char '0')) = "01234";
*)

let starts_with str p =
  let len = length p in
  if length str < len then false
  else
    BatReturn.label
      (fun label ->
        for i = 0 to len - 1 do
          if unsafe_get str i <> unsafe_get p i then
            BatReturn.return label false
        done;
        true)
(*$T starts_with
  starts_with "foobarbaz" "foob"
  starts_with "foobarbaz" ""
  starts_with "" ""
  not (starts_with "bar" "foobar")
  not (starts_with "" "foo")
  starts_with "Jon \"Maddog\" Orwant" "Jon"
  not (starts_with "Jon \"Maddog\" Orwant" "Jon \"Maddog\" Orwants")
  not (starts_with "Jon \"Maddog\" Orwant" "Orwants")
*)


let ends_with str p =
  let el = length p
  and sl = length str in
  let diff = sl - el in
  if diff < 0 then false (*string is too short*)
  else
    BatReturn.label
      (fun label ->
        for i = 0 to el - 1 do
          if get str (diff + i) <> get p i then
            BatReturn.return label false
        done;
        true)
(*$T ends_with
  ends_with "foobarbaz" "rbaz"
  ends_with "foobarbaz" ""
  ends_with "" ""
  not (ends_with "foo" "foobar")
  not (ends_with "" "foo")
  ends_with "Jon \"Maddog\" Orwant" "want"
  not (ends_with "Jon \"Maddog\" Orwant" "I'm Jon \"Maddog\" Orwant")
  not (ends_with "Jon \"Maddog\" Orwant" "Jon")
*)

let find_from str pos sub =
  let len = length str in
  let sublen = length sub in
  if pos < 0 || pos > len then raise (Invalid_argument "String.find_from");
  if sublen = 0 then pos else
    BatReturn.label (fun label ->
      for i = pos to len - sublen do
        let j = ref 0 in
        while unsafe_get str (i + !j) = unsafe_get sub !j do
          incr j;
          if !j = sublen then BatReturn.return label i
        done;
      done;
      raise Not_found
    )
(*$Q find_from
  (Q.triple Q.string Q.char Q.small_int) ~count:1000 (fun (s, c, ofs) -> \
    let v1 = try `res (find_from s ofs (String.make 1 c)) with Not_found -> `nf | Invalid_argument _ -> `inv in \
    let v2 = try `res (String.index_from s ofs c) with Not_found -> `nf | Invalid_argument _ -> `inv in \
    (match v1, v2 with `res s1, `res s2 when s1 = s2 -> true | `nf, `nf | `inv, `inv -> true | _ -> false) \
  )
  (Q.triple Q.string Q.string Q.small_int) ~count:1000 (fun (s, s2, ofs) -> \
    let v1 = try `res (ofs + find (String.sub s ofs (String.length s - ofs)) s2) with Not_found -> `nf | Invalid_argument _ -> `inv in \
    let v2 = try `res (find_from s ofs s2) with Not_found -> `nf | Invalid_argument _ -> `inv in \
    (match v1, v2 with `res s1, `res s2 when s1 = s2 -> true | `nf, `nf | `inv, `inv -> true | _ -> false) \
  )
*)
(*$T find_from
  find_from "foobarbaz" 4 "ba" = 6
  find_from "foobarbaz" 7 "" = 7
  try ignore (find_from "" 0 "a"); false with Not_found -> true
  try ignore (find_from "foo" 2 "foo"); false with Not_found -> true
  try ignore (find_from "foo" 3 "foo"); false with Not_found -> true
  try ignore (find_from "foo" 4 "foo"); false with Invalid_argument _ -> true
  try ignore (find_from "foo" (-1) "foo"); false with Invalid_argument _ -> true
*)

let find str sub = find_from str 0 sub
(*$T find
  find "foobarbaz" "bar" = 3
  try ignore (find "foo" "bar"); false with Not_found -> true
*)

let rfind_from str pos sub =
  let sublen = length sub
  and len = length str in
  if pos + 1 < 0 || pos + 1 > len then raise (Invalid_argument "String.rfind_from");
  if sublen = 0 then pos + 1 else
    BatReturn.label (fun label ->
      for i = pos - sublen + 1 downto 0 do
        let j = ref 0 in
        while unsafe_get str (i + !j) = unsafe_get sub !j do
          incr j;
          if !j = sublen then BatReturn.return label i
        done;
      done;
      raise Not_found
    )
(*$Q rfind_from
  (Q.triple Q.string Q.char Q.small_int) ~count:1000 (fun (s, c, ofs) -> \
    let v1 = try `res (rfind_from s ofs (String.make 1 c)) with Not_found -> `nf | Invalid_argument _ -> `inv in \
    let v2 = try `res (String.rindex_from s ofs c) with Not_found -> `nf | Invalid_argument _ -> `inv in \
    (match v1, v2 with `res s1, `res s2 when s1 = s2 -> true | `nf, `nf | `inv, `inv -> true | _ -> false) \
  )
  (Q.triple Q.string Q.string Q.small_int) ~count:1000 (fun (s, s2, ofs) -> \
    let v1 = try `res (rfind (String.sub s 0 (ofs + 1)) s2) with Not_found -> `nf | Invalid_argument _ -> `inv in \
    let v2 = try `res (rfind_from s ofs s2) with Not_found -> `nf | Invalid_argument _ -> `inv in \
    (match v1, v2 with `res s1, `res s2 when s1 = s2 -> true | `nf, `nf | `inv, `inv -> true | _ -> false) \
  )
*)
(*$T rfind_from
  rfind_from "foobarbaz" 5 "ba" = 3
  rfind_from "foobarbaz" 7 "ba" = 6
  rfind_from "foobarbaz" 6 "ba" = 3
  rfind_from "foobarbaz" 7 "" = 8
  try ignore (rfind_from "" 3 ""); false with Invalid_argument _ -> true
  try ignore (rfind_from "" (-1) "a"); false with Not_found -> true
  try ignore (rfind_from "foobarbaz" 2 "ba"); false with Not_found -> true
  try ignore (rfind_from "foo" 3 "foo"); false with Invalid_argument _ -> true
  try ignore (rfind_from "foo" (-2) "foo"); false with Invalid_argument _ -> true
*)

let rfind str sub = rfind_from str (String.length str - 1) sub
(*$T rfind
  rfind "foobarbaz" "ba" = 6
  try ignore (rfind "foo" "barr"); false with Not_found -> true
*)

let exists str sub =
  try
    ignore (find str sub);
    true
  with
    Not_found -> false
(*$T exists
   exists "foobarbaz" "obar"
   exists "obar" "obar"
   exists "foobarbaz" ""
   exists "" ""
   not (exists "foobar" "obb")
   not (exists "" "foo")
  exists "a" ""
  not (exists "" "a")
  exists "ab" "a"
  exists "ab" "b"
  not (exists "ab" "c")
*)

let strip ?(chars = " \t\r\n") s =
  let p = ref 0 in
  let l = length s in
  while !p < l && contains chars (unsafe_get s !p) do
    incr p;
  done;
  let p = !p in
  let l = ref (l - 1) in
  while !l >= p && contains chars (unsafe_get s !l) do
    decr l;
  done;
  sub s p (!l - p + 1)
(*$T strip
   strip ~chars:" ,()" " boo() bar()" = "boo() bar"
   strip ~chars:"abc" "abbcbab" = ""
*)



let left s len =
  if len >= length s then s else sub s 0 len

let right s len =
  let slen = length s in
  if len >= slen then s else sub s (slen - len) len

let head s pos =
  left s pos

let tail s pos =
  let slen = length s in
  if pos >= slen then "" else sub s pos (slen - pos)

(*$T left
  left "abc" 1 = "a"
  left "ab" 3 = "ab"
  left "abc" 3 = "abc"
  left "abc" 10 = "abc"
  left "abc" 0 = ""
*) (*$T right
     right "abc" 1 = "c"
     right "ab" 3 = "ab"
     right "abc" 3 = "abc"
     right "abc" 0 = ""
     right "abc" 10 = "abc"
   *) (*$T tail
        tail "abc" 1 = "bc"
        tail "ab" 3 = ""
        tail "abc" 3 = ""
        tail "abc" 10 = ""
        tail "abc" 0 = "abc"
      *) (*$T head
        head "abc" 0 = ""
        head "abc" 10 = "abc"
        head "abc" 3 = "abc"
      *)

let split str ~by:sep =
  let p = find str sep in
  let len = length sep in
  let slen = length str in
  sub str 0 p, sub str (p + len) (slen - p - len)

(*$T split
   split "abcGxyzG123" ~by:"G" = ("abc","xyzG123")
   split "abcGHIzyxGHI123" ~by:"GHI" = ("abc", "zyxGHI123")
   split "abcGHIzyxGHI123" ~by:"" = ("", "abcGHIzyxGHI123")
   try split "abcxyz" ~by:"G" |> ignore; false with Not_found -> true
   split "abcabc" ~by:"abc" = ("", "abc")
   split "abcabcd" ~by:"abcd" = ("abc", "")
*)

let rsplit str ~by:sep =
  let p = rfind str sep in
  let len = length sep in
  let slen = length str in
  sub str 0 p, sub str (p + len) (slen - p - len)
(*$T rsplit
  rsplit "abcGxyzG123" ~by:"G" = ("abcGxyz","123")
   rsplit "abcGHIzyxGHI123" ~by:"GHI" = ("abcGHIzyx", "123")
   rsplit "abcGHIzyxGHI123" ~by:"" = ("abcGHIzyxGHI123", "")
   try rsplit "abcxyz" ~by:"G" |> ignore; false with Not_found -> true
*)

(*
   An implementation of [nsplit] in one pass.

   This implementation traverses the string backwards, hence building the list
   of substrings from the end to the beginning, so as to avoid a call to [List.rev].
*)
let nsplit str ~by:sep =
  if str = "" then []
  else if sep = "" then invalid_arg "nsplit: empty sep not allowed"
  else
    (* str is non empty *)
    let seplen = String.length sep in
    let rec aux acc ofs =
      if ofs >= 0 then (
        match
          try Some (rfind_from str ofs sep)
          with Not_found -> None
        with
        | Some idx -> (* sep found *)
          let end_of_sep = idx + seplen - 1 in
          if end_of_sep = ofs (* sep at end of str *)
          then aux (""::acc) (idx - 1)
          else
            let token = sub str (end_of_sep + 1) (ofs - end_of_sep) in
            aux (token::acc) (idx - 1)
        | None -> (* sep NOT found *)
          (sub str 0 (ofs + 1))::acc
      )
      else
        (* Negative ofs: the last sep started at the beginning of str *)
        ""::acc
    in
    aux [] (length str - 1 )

(*$T nsplit
  nsplit "a;b;c" ~by:";" = ["a"; "b"; "c"]
  nsplit "" ~by:"x" = []
  try nsplit "abc" ~by:"" = ["a"; "b"; "c"] with Invalid_argument _ -> true
  nsplit "a/b/c" ~by:"/" = ["a"; "b"; "c"]
  nsplit "/a/b/c//" ~by:"/" = [""; "a"; "b"; "c"; ""; ""]
  nsplit "FOOaFOObFOOcFOOFOO" ~by:"FOO" = [""; "a"; "b"; "c"; ""; ""]
*)

let join = concat

let unsafe_slice i j s =
  if i >= j || i = length s then
    create 0
  else
    sub s i (j-i)

let clip ~lo ~hi (x:int) = if x < lo then lo else if x > hi then hi else x
let wrap (x:int) ~hi = if x < 0 then hi + x else x
let slice ?(first = 0) ?(last = Sys.max_string_length) s =
  let lo = 0 and hi = length s in
  let i = clip ~lo ~hi (wrap first ~hi) in
  let j = clip ~lo ~hi (wrap last ~hi) in
  unsafe_slice i j s

(*$T slice
   slice ~first:1 ~last:(-3) " foo bar baz" = "foo bar "
   slice "foo" = "foo"
   slice ~first:0 ~last:10 "foo" = "foo"
   slice ~first:(-2) "foo" = "oo"
   slice ~first:(-3) ~last:(-1) "foob" = "oo"
   slice ~first:5 ~last:4 "foobarbaz" = ""
*)


let lchop ?(n = 1) s =
  if n < 0 then
    invalid_arg "lchop: number of characters to chop is negative"
  else
    let slen = length s in
    if slen <= n then "" else sub s n (slen - n)
(*$T lchop
   lchop "Weeble" = "eeble"
   lchop "" = ""
   lchop ~n:3 "Weeble" = "ble"
   lchop ~n:1000 "Weeble" = ""
   lchop ~n:0 "Weeble" = "Weeble"
   try ignore (lchop ~n:(-1) "Weeble"); false with Invalid_argument _ -> true
*)

let rchop ?(n = 1) s =
  if n < 0 then
    invalid_arg "rchop: number of characters to chop is negative"
  else
    let slen = length s in
    if slen <= n then "" else sub s 0 (slen - n)
(*$T rchop
   rchop "Weeble" = "Weebl"
   rchop "" = ""
   rchop ~n:3 "Weeble" = "Wee"
   rchop ~n:1000 "Weeble" = ""
   try ignore (rchop ~n:(-1) "Weeble"); false with Invalid_argument _ -> true
*)

let of_int = string_of_int
(*$T of_int
   of_int 56 = "56"
   of_int (-1) = "-1"
*)

let of_float = string_of_float

let of_char = make 1
(*$T of_char
   of_char 's' = "s"
   of_char '\000' = "\000"
*)

let to_int s = int_of_string s
(*$T to_int
   to_int "8_480" = to_int "0x21_20"
   try ignore (to_int ""); false with Failure "int_of_string" -> true
   try ignore (to_int "2,3"); false with Failure "int_of_string" -> true
*)

let to_float s = float_of_string s
(*$T to_float
   to_float "12.34e-1" = to_float "1.234"
   to_float "1" = 1.
   try ignore (to_float ""); false with Failure _ -> true
*)

let enum s =
  let l = length s in
  let rec make i =
    BatEnum.make
      ~next:(fun () ->
        if !i = l then
          raise BatEnum.No_more_elements
        else
          unsafe_get s (BatRef.post_incr i)
      )
      ~count:(fun () -> l - !i)
      ~clone:(fun () -> make (BatRef.copy i))
  in
  make (ref 0)
(*$T enum
   "" |> enum |> List.of_enum = []
   "foo" |> enum |> List.of_enum = ['f'; 'o'; 'o']
   let e = enum "abcdef" in \
   for _i = 0 to 2 do BatEnum.junk e done; \
   let e2 = BatEnum.clone e in \
   implode (BatList.of_enum e) = "def" && implode (BatList.of_enum e2) = "def"
*)

let backwards s =
  let rec make i =
    BatEnum.make
      ~next:(fun () ->
        if !i <= 0 then
          raise BatEnum.No_more_elements
        else
          unsafe_get s (BatRef.pre_decr i)
      )
      ~count:(fun () -> !i)
      ~clone:(fun () -> make (BatRef.copy i))
  in
  make (ref (length s))
(*$T backwards
   "" |> backwards |> of_enum = ""
   "foo" |> backwards |> of_enum = "oof"
   let e = backwards "abcdef" in \
   for _i = 0 to 2 do BatEnum.junk e done; \
   let e2 = BatEnum.clone e in \
   implode (BatList.of_enum e) = "cba" && implode (BatList.of_enum e2) = "cba"
*)

let of_enum e =
  (* TODO: use a buffer when not fast_count *)
  let l = BatEnum.count e in
  let s = create l in
  let i = ref 0 in
  BatEnum.iter (fun c -> unsafe_set s (BatRef.post_incr i) c) e;
  s
(*$T of_enum
    Enum.init 3 (fun i -> char_of_int (i + int_of_char '0')) |> of_enum = "012"
    Enum.init 0 (fun _i -> ' ') |> of_enum = ""
*)

let of_backwards e =
  (* TODO: use a buffer when not fast_count *)
  let l = BatEnum.count e in
  let s = create l in
  let i = ref (l - 1) in
  BatEnum.iter (fun c -> unsafe_set s (BatRef.post_decr i) c) e;
  s
(*$T of_backwards
   "" |> enum |> of_backwards = ""
   "foo" |> enum |> of_backwards = "oof"
   "foo" |> backwards |> of_backwards = "foo"
*)

let map f s =
  let len = length s in
  let sc = create len in
  for i = 0 to len - 1 do
    unsafe_set sc i (f (unsafe_get s i))
  done;
  sc
(*$T map
   map Char.uppercase "Five" = "FIVE"
   map Char.uppercase "" = ""
   map (String.of_char %> failwith) "" = ""
*)

let filter_map f s =
  let len = length s in
  let sc = Buffer.create len in
  for i = 0 to len - 1 do
    match f (unsafe_get s i) with
    | Some c -> Buffer.add_char sc c
    | None -> ()
  done;
  Buffer.contents sc
(*$T filter_map
   filter_map (function 'a'..'z' as c -> Some (Char.uppercase c) | _ -> None) "a b c" = "ABC"
*)

let filter f s =
  let len = length s in
  let sc = Buffer.create len in
  for i = 0 to len - 1 do
    let c = unsafe_get s i in
    if f c then Buffer.add_char sc c
  done;
  Buffer.contents sc
(*$T filter
   filter ((<>) ' ') "a b c" = "abc"
*)

(* fold_left and fold_right by Eric C. Cooper *)
let fold_left f init str =
  let n = String.length str in
  let rec loop i result =
    if i = n then result
    else loop (i + 1) (f result str.[i])
  in
  loop 0 init
(*$T fold_left
   fold_left (fun li c -> c::li) [] "foo" = ['o';'o';'f']
   fold_left max 'a' "apples" = 's'
*)


let fold_right f str init =
  let n = String.length str in
  let rec loop i result =
    if i = 0 then result
    else
      let i' = i - 1 in
      loop i' (f str.[i'] result)
  in
  loop n init
(*$T fold_right
   fold_right List.cons "foo" [] = ['f';'o';'o']
   fold_right (fun c a -> if c = ' ' then a+1 else a) "a b c" 0 = 2
*)

let iteri f str =
  for i = 0 to (String.length str) - 1 do f i str.[i] done

(*$R iteri
  let letter_positions word =
    let positions = Array.make 256 [] in
    let count_letter pos c =
      positions.(int_of_char c) <- pos :: positions.(int_of_char c) in
    iteri count_letter word;
    Array.mapi (fun c pos -> (char_of_int c, List.rev pos)) positions
    |> Array.to_list
    |> List.filter (fun (_c, pos) -> pos <> [])
  in
  assert_equal ~msg:"String.iteri test"
     (letter_positions "hello")
     ['e',[1]; 'h',[0]; 'l',[2;3]; 'o',[4] ]
*)

(* explode and implode from the OCaml Expert FAQ. *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
(*$T explode
   explode "foo" = ['f'; 'o'; 'o']
   explode "" = []
*)


let to_list = explode
(*$T to_list
   to_list "string" |> List.interleave ';' |> of_list = "s;t;r;i;n;g"
*)

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l
(*$T implode
   implode ['b';'a';'r'] = "bar"
   implode [] = ""
*)


let of_list = implode
(*$T of_list
   ['c'; 'h'; 'a'; 'r'; 's'] |> of_list = "chars"
   [] |> of_list = ""
*)

let replace_chars f s =
  let len = String.length s in
  let tlen = ref 0 in
  let rec loop i acc =
    if i = len then
      acc
    else
      let s = f (unsafe_get s i) in
      tlen := !tlen + length s;
      loop (i+1) (s :: acc)
  in
  let strs = loop 0 [] in
  let sbuf = create !tlen in
  let pos = ref !tlen in
  let rec loop2 = function
    | [] -> ()
    | s :: acc ->
      let len = length s in
      pos := !pos - len;
      blit s 0 sbuf !pos len;
      loop2 acc
  in
  loop2 strs;
  sbuf
(*$T replace_chars
   replace_chars (function ' ' -> "(space)" | c -> of_char c) "foo bar" = "foo(space)bar"
   replace_chars (fun _ -> "") "foo" = ""
   replace_chars (fun _ -> assert false) "" = ""
*)

let replace ~str ~sub ~by =
   try
     let subpos = find str sub in
     let strlen = length str in
     let sublen = length sub in
     let bylen  = length by in
     let newstr = create (strlen - sublen + bylen) in
     blit str 0 newstr 0 subpos ;
     blit by 0 newstr subpos bylen ;
     blit str (subpos + sublen) newstr (subpos + bylen) (strlen - subpos - sublen) ;
     (true, newstr)
   with Not_found ->  (* find failed *)
     (false, str)
(*$T replace
   replace ~str:"foobarbaz" ~sub:"bar" ~by:"rab" = (true, "foorabbaz")
   replace ~str:"foo" ~sub:"bar" ~by:"" = (false, "foo")
*)


let nreplace ~str ~sub ~by =
  if sub = "" then invalid_arg "nreplace: cannot replace all empty substrings" ;
  let strlen = length str in
  let sublen = length sub in
  let bylen = length by in
  let dlen = bylen - sublen in
  let rec loop_subst idxes newlen i =
    match (try rfind_from str (i-1) sub with Not_found -> -1) with
    | -1 -> idxes, newlen
    | i' -> loop_subst (i'::idxes) (newlen+dlen) i' in
  let idxes, newlen = loop_subst [] strlen strlen in
  let newstr = create newlen in
  let rec loop_copy i j idxes =
    match idxes with
    | [] ->
      (* still need the last chunk *)
      unsafe_blit str i newstr j (strlen-i)
    | i'::rest ->
      let di = i' - i in
      unsafe_blit str i newstr j di ;
      unsafe_blit by 0 newstr (j + di) bylen ;
      loop_copy (i + di + sublen) (j + di + bylen) rest in
  loop_copy 0 0 idxes ;
  newstr
(*$T nreplace
   nreplace ~str:"bar foo aaa bar" ~sub:"aa" ~by:"foo" = "bar foo afoo bar"
   nreplace ~str:"bar foo bar" ~sub:"bar" ~by:"foo" = "foo foo foo"
   nreplace ~str:"aaaaaa" ~sub:"aa" ~by:"aaa" = "aaaaaaaaa"
   nreplace ~str:"" ~sub:"aa" ~by:"bb" = ""
   nreplace ~str:"foo bar baz" ~sub:"foo bar baz" ~by:"" = ""
   nreplace ~str:"abc" ~sub:"abc" ~by:"def" = "def"
   let s1 = "foo" in let s2 = nreplace ~str:s1 ~sub:"X" ~by:"X" in set s2 0 'F' ; s1.[0] = 'f'
*)


let rev_in_place s =
  let len = String.length s in
  if len > 0 then for k = 0 to (len - 1)/2 do
      let old = s.[k] and mirror = len - 1 - k in
      s.[k] <- s.[mirror]; s.[mirror] <- old;
    done
(*$= rev_in_place as f & ~printer:identity
  (let s="" in f s; s)          ""
  (let s="1" in f s; s)         "1"
  (let s="12" in f s; s)        "21"
  (let s="Example!" in f s; s)  "!elpmaxE"
*)

let in_place_mirror = rev_in_place

let repeat s n =
  let buf = Buffer.create ( n * (String.length s) ) in
  for i = 1 to n do Buffer.add_string buf s done;
  Buffer.contents buf
(*$T repeat
   repeat "fo" 4 = "fofofofo"
   repeat "fo" 0 = ""
   repeat "" 4 = ""
*)

let rev s = 
  let len = String.length s in
  let reversed = String.create len in
  for i = 0 to len - 1 do
    String.unsafe_set reversed (len - i - 1) (String.unsafe_get s i)
  done;
  reversed

(*$T rev
   rev "" = ""
   rev "batteries" = "seirettab"
   rev "even" = "neve"
*)

let trim s =
  let len = length s in
  let rec aux_1 i = (*locate leading whitespaces*)
    if i = len then None (*The whole string is whitespace*)
    else if BatChar.is_whitespace (unsafe_get s i) then aux_1 (i + 1)
    else Some i in
  match aux_1 0 with
  | None -> ""
  | Some last_leading_whitespace ->
    let rec aux_2 i =
      assert (i >= 0);
      if BatChar.is_whitespace (unsafe_get s i) then aux_2 (i - 1)
      else i in
    let first_trailing_whitespace = aux_2 (len - 1) in
    unsafe_slice last_leading_whitespace (first_trailing_whitespace + 1) s

(*$T trim
   trim " \t foo\n  " = "foo"
   trim " foo bar " = "foo bar"
   trim "  \t " = ""
   trim "" = ""
*)

let splice s1 off len s2 =
  let len1 = length s1 and len2 = length s2 in
  let off = wrap off ~hi:len1 in
  let len = clip ~lo:0 ~hi:(len1 - off) len in
  let out_len = len1 - len + len2 in
  let s = create out_len in
  blit s1 0 s 0 off; (* s1 before splice point *)
  blit s2 0 s off len2; (* s2 at splice point *)
  blit s1 (off+len) s (off+len2) (len1 - (off+len)); (* s1 after off+len *)
  s
(*$T splice
   splice "foo bar baz" 3 5 "XXX" = "fooXXXbaz"
   splice "foo bar baz" 5 0 "XXX" = "foo bXXXar baz"
   splice "foo bar baz" 5 (-10) "XXX" = "foo bXXXar baz"
   splice "foo bar baz" 5 50 "XXX" = "foo bXXX"
   splice "foo bar baz" (-4) 2 "XXX" = "foo barXXXaz"
   splice "bar baz" (-4) 2 "XXX" = "barXXXaz"
*)


let is_empty s = length s = 0
(*$T is_empty
   is_empty ""
   not (is_empty "foo")
   is_empty (String.make 0 'a')
*)

let icompare s1 s2 = compare (String.lowercase s1) (String.lowercase s2)
(*$T icompare
   icompare "FOO" "bar" = 1
*)

type t_alias = t (* needed for IString breaks type t = t *)

module IString =
struct
  type t = t_alias
  let compare = icompare
end

let numeric_compare s1 s2 =
  let e1 = BatEnum.group BatChar.is_digit (enum s1) in
  let e2 = BatEnum.group BatChar.is_digit (enum s2) in
  BatEnum.compare (fun g1 g2 ->
    let s1 = of_enum g1 in
    let s2 = of_enum g2 in
    if BatChar.is_digit s1.[0] && BatChar.is_digit s2.[0] then
      let n1 = Big_int.big_int_of_string s1 in
      let n2 = Big_int.big_int_of_string s2 in
      Big_int.compare_big_int n1 n2
    else
      String.compare s1 s2
  ) e1 e2

(*$T numeric_compare
   numeric_compare "xx43" "xx320" = -1
   numeric_compare "xx3" "xx21" = -1
   numeric_compare "xx02" "xx2" = 0
   numeric_compare "xx20" "xx5" = 1
   numeric_compare "abc" "def" = compare "abc" "def"
   numeric_compare "x50y" "x51y" = -1
   numeric_compare "a23d" "a234" < 0
   numeric_compare "a234" "a23d" > 0
   numeric_compare "a1b" "a01c" < 0
   numeric_compare "a1b" "a01b" = 0
   numeric_compare "a1b2" "a01b01" > 0
*)

(*$Q numeric_compare
  (Q.triple Q.printable_string Q.pos_int Q.pos_int) (fun (s,m,n) -> numeric_compare (s ^ string_of_int m) (s ^ string_of_int n) = BatInt.compare m n)
*)

module NumString =
struct
  type t = t_alias
  let compare = numeric_compare
end


let print = BatInnerIO.nwrite
let println out s = BatInnerIO.nwrite out s; BatInnerIO.write out '\n'

(*$T
  BatIO.to_string print "\n" = "\n"
  BatIO.to_string println "\n" = "\n\n"
  BatIO.to_string print_quoted "\n" = "\"\\n\""
  quote "\n" = "\"\\n\""
*)

(* Beware: the documentation of print_quoted claims that its behavior
   is compatible with this "quote" function. This is currently true as
   they both use "%S", but any change in 'quote' here should be
   careful to preserve this consistency. *)
let quote s = Printf.sprintf "%S" s
(*$T quote
   quote "foo" = "\"foo\""
   quote "\"foo\"" = "\"\\\"foo\\\"\""
   quote "\n" = "\"\\n\""
*)

let print_quoted out s = BatInnerIO.nwrite out (quote s)

module Exceptionless =
struct
  let find_from str ofs sub =
    try Some (find_from str ofs sub) with Not_found -> None

  let find str sub = find_from str 0 sub
  (*$T
    Exceptionless.find "a" "b" = None
  *)

  let rfind_from str suf sub =
    try Some (rfind_from str suf sub) with Not_found -> None

  let rfind str sub = rfind_from str (String.length str - 1) sub
  (*$T
    Exceptionless.rfind "a" "b" = None
  *)

  let to_int s = try Some (to_int s) with Failure _ -> None
  (*$T
    Exceptionless.to_int "" = None
  *)

  let to_float s = try Some (to_float s) with Failure _ -> None
  (*$T
    Exceptionless.to_float "" = None
  *)

  let index s c = try Some (index s c) with Not_found -> None
  (*$T
    Exceptionless.index "a" 'b' = None
  *)

  let index_from s i c = try Some (index_from s i c) with Not_found -> None
  (*$T
    Exceptionless.index_from "a" 0 'b' = None
  *)

  let rindex_from s i c = try Some (rindex_from s i c) with Not_found -> None
  (*$T
    Exceptionless.rindex_from "a" 0 'b' = None
  *)

  let rindex s c = try Some (rindex s c) with Not_found -> None
  (*$T
    Exceptionless.rindex "a" 'b' = None
  *)

  let split str ~by = try Some (split str ~by) with Not_found -> None
  (*$T
    Exceptionless.split "a" ~by:"e" = None
  *)

  let rsplit str ~by = try Some (rsplit str ~by) with Not_found -> None
  (*$T
    Exceptionless.rsplit "a" ~by:"e" = None
  *)
end (* String.Exceptionless *)

module Cap =
struct
  type 'a t = string

  let make          = make
  let is_empty      = is_empty
  let init          = init
  let enum          = enum
  let of_enum       = of_enum
  let backwards     = backwards
  let of_backwards  = of_backwards

  let of_int        = of_int
  let of_float      = of_float
  let of_char       = of_char
  let to_int        = to_int
  let to_float      = to_float
  let map           = map
  let fold_left     = fold_left
  let fold_right    = fold_right
  let iter          = iter
  let index         = index
  let rindex        = rindex
  let index_from    = index_from
  let rindex_from   = rindex_from
  let contains      = contains
  let contains_from = contains_from
  let rcontains_from= rcontains_from
  let find          = find
  let find_from     = find_from
  let rfind         = rfind
  let rfind_from    = rfind_from
  let ends_with     = ends_with
  let starts_with   = starts_with
  let exists        = exists
  let lchop         = lchop
  let rchop         = rchop
  let strip         = strip
  let uppercase     = uppercase
  let lowercase     = lowercase
  let capitalize    = capitalize
  let uncapitalize  = uncapitalize
  let copy          = copy
  let sub           = sub
  let fill          = fill
  let blit          = blit
  let concat        = concat
  let escaped       = escaped
  let replace_chars = replace_chars
  let replace       = replace
  let nreplace      = nreplace
  let split         = split
  let repeat        = repeat
  let rsplit        = rsplit
  let nsplit        = nsplit
  let join          = join
  let slice         = slice
  let explode       = explode
  let implode       = implode
  let compare       = compare
  let icompare      = icompare
  let splice        = splice
  let trim          = trim
  let quote         = quote
  let left          = left
  let right         = right
  let head          = head
  let tail          = tail
  let filter_map    = filter_map
  let filter        = filter
  let of_list       = of_list
  let to_list       = to_list

  let quote         = quote
  let print         = print
  let println       = println
  let print_quoted  = print_quoted

  external of_string : string -> _ t                = "%identity"
  external to_string : [`Read | `Write] t -> string = "%identity"
  external read_only : [> `Read] t -> [`Read] t     = "%identity"
  external write_only: [> `Write] t -> [`Write] t   = "%identity"

  external length : _ t -> int = "%string_length"
  external get : [> `Read] t -> int -> char = "%string_safe_get"
  external set : [> `Write] t -> int -> char -> unit = "%string_safe_set"
  external create : int -> _ t = "caml_create_string"
  external unsafe_get : [> `Read] t -> int -> char = "%string_unsafe_get"
  external unsafe_set : [> `Write] t -> int -> char -> unit = "%string_unsafe_set"
  external unsafe_blit :
    [> `Read] t -> int -> [> `Write] t -> int -> int -> unit = "caml_blit_string" "noalloc"
  external unsafe_fill :
    [> `Write] t -> int -> int -> char -> unit = "caml_fill_string" "noalloc"

  module Exceptionless =
  struct
    let find_from = Exceptionless.find_from
    let find = Exceptionless.find
    let rfind_from = Exceptionless.rfind_from
    let rfind = Exceptionless.rfind
    let to_int = Exceptionless.to_int
    let to_float = Exceptionless.to_float
    let index = Exceptionless.index
    let index_from = Exceptionless.index_from
    let rindex_from = Exceptionless.rindex_from
    let rindex = Exceptionless.rindex
    let split = Exceptionless.split
    let rsplit = Exceptionless.rsplit
  end (* String.Cap.Exceptionless *)

end (* String.Cap *)
