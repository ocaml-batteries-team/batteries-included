(*
 * ExtString - Additional functions for string manipulations.
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


(*Minor optimization.*)
let int_min (x:int) (y:int) = if x < y then x else y
let int_max (x:int) (y:int) = if x < y then y else x

open String

let compare = String.compare
(**T String.compare
   compare "FOO" "bar" = -1
**)

let init len f =
	let s = create len in
	for i = 0 to len - 1 do
		unsafe_set s i (f i)
	done;
	s

(**T String.init
   init 5 (fun i -> Char.chr (i + int_of_char '0')) = "01234";
**)

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
(**T String.starts_with
   starts_with "foobarbaz" "foob"
   starts_with "foobarbaz" ""
   starts_with "" ""
   not (starts_with "bar" "foobar")
   not (starts_with "" "foo")
**)


let ends_with str p =
  let el = length p   
  and sl = length str in
  let diff = sl - el  in
    if diff < 0 then false (*string is too short*)
    else
      BatReturn.label
	(fun label ->
	   for i = 0 to el - 1 do
	     if get str (diff + i) <> get p i then
	       BatReturn.return label false
	   done;
	   true)
(**T String.ends_with
   ends_with "foobarbaz" "rbaz"
   ends_with "foobarbaz" ""
   ends_with "" ""
   not (ends_with "foo" "foobar")
   not (ends_with "" "foo")
**)

let find_from str ofs sub = 
  let sublen = length sub in
    if sublen = 0 then ofs (*If [sub] is the empty string, by convention, it may be found wherever we started searching.*)
    else
      let len = length str in
	if len = 0 then raise Not_found else
	if 0 > ofs || ofs >= len then raise (Invalid_argument "index out of bounds")
	else
	BatReturn.label (fun label ->
  	  for i = ofs to len - sublen do
	    let j = ref 0 in
	      while unsafe_get str (i + !j) = unsafe_get sub !j do
		incr j;
		if !j = sublen then BatReturn.return label i
	      done;
	  done;
	  raise Not_found
        )



let find str sub = find_from str 0 sub
(**T String.find
   find "foobarbaz" "bar" = 3
   try ignore (find "foo" "bar"); false with Not_found -> true
   find_from "foobarbaz" 4 "ba" = 6
   try ignore (find_from "foo" 2 "foo"); false with Not_found -> true
 **)

let rfind_from str suf sub = 
  let sublen = length sub 
  and len    = length str in
    if sublen = 0 then len
    else
      if len = 0 then raise Not_found else
	if 0 > suf || suf >= len then raise (Invalid_argument "index out of bounds")
	else
	BatReturn.label (fun label ->
  	  for i = suf - sublen + 1 downto 0 do
	    (*Printf.printf "i:%i/suf:%i/sublen:%i/len:%i\n" i suf sublen len;*)
	    let j = ref 0 in
	      while unsafe_get str ( i + !j ) = unsafe_get sub !j do
		incr j;
		if !j = sublen then BatReturn.return label i
	      done;
	  done;
	  raise Not_found
        )

let rfind str sub = rfind_from str (String.length str - 1) sub
(**T String.rfind
   rfind "foobarbaz" "ba" = 6
   try ignore(rfind "foo" "barr"); false with Not_found -> true
   rfind_from "foobarbaz" 5 "ba" = 3
**)

let exists str sub =
	try
		ignore(find str sub);
		true
	with
		Not_found -> false
(**T String.exists 
   exists "foobarbaz" "obar"
   exists "obar" "obar"
   exists "foobarbaz" ""
   exists "" ""
   not (exists "foobar" "obb")
   not (exists "" "foo")
**)

let strip ?(chars=" \t\r\n") s =
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
(**T String.strip
   strip ~chars:" ,()" " boo() bar()" = "boo() bar"
**)



let left s len = if len >= length s then s else sub s 0 len
let right s len = let slen = length s in
		  if len >= slen then s else sub s (slen - len) len
let head s pos = left s pos
let tail s pos = let slen = length s in
		 if pos >= slen then "" else sub s pos (slen - pos)

(**T String.{left,right,head,tail}
   left "abc" 1 = "a"
   right "abc" 1 = "c"
   left "ab" 3 = "ab"
   right "ab" 3 = "ab"
   tail "abc" 1 = "bc"
   tail "ab" 3 = ""
   left "abc" 3 = "abc"
   right "abc" 3 = "abc"
   head "abc" 3 = "abc"
   tail "abc" 3 = ""
   left "abc" 10 = "abc"
   right "abc" 10 = "abc"
   head "abc" 10 = "abc"
   tail "abc" 10 = ""
   left "abc" 0 = ""
   right "abc" 0 = ""
   head "abc" 0 = ""
   tail "abc" 0 = "abc"
 **)

let split str sep =
	let p = find str sep in
	let len = length sep in
	let slen = length str in
	sub str 0 p, sub str (p + len) (slen - p - len)

(**T String.split
   split "abcGxyzG123" "G" = ("abc","xyzG123")
   split "abcGHIzyxGHI123" "GHI" = ("abc", "zyxGHI123")
   split "abcGHIzyxGHI123" "" = ("", "abcGHIzyxGHI123")
   try split "abcxyz" "G" |> ignore; false with Not_found -> true
**)

let rsplit str sep = 
  let p = rfind str sep in
  let len = length sep in
  let slen = length str in
    sub str 0 p, sub str (p + len) (slen - p - len)
(**T String.rsplit
   rsplit "abcGxyzG123" "G" = ("abcGxyz","123")
   rsplit "abcGHIzyxGHI123" "GHI" = ("abcGHIzyx", "123")
   rsplit "abcGHIzyxGHI123" "" = ("abcGHIzyxGHI123", "")
   try rsplit "abcxyz" "G" |> ignore; false with Not_found -> true
**)

(**
   An implementation of [nsplit] in one pass.

   This implementation traverses the string backwards, hence building the list
   of substrings from the end to the beginning, so as to avoid a call to [List.rev].
*)
let nsplit str sep =
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
          | None     -> (* sep NOT found *)
            (sub str 0 (ofs + 1))::acc
      )
      else
        (* Negative ofs: the last sep started at the beginning of str *)
        ""::acc
    in
      aux [] (length str - 1 )

(**T String.nsplit
   nsplit "a;b;c" ";" = ["a"; "b"; "c"]
   nsplit "" "x" = []
   try nsplit "abc" "" = ["a"; "b"; "c"] with Invalid_argument _ -> true
**)

let join = concat

let unsafe_slice i j s =
  if i >= j || i = length s then
    create 0
  else
    sub s i (j-i)

let clip ~lo ~hi (x:int) = if x < lo then lo else if x > hi then hi else x
let wrap (x:int) ~hi = if x < 0 then hi + x else x
let slice ?(first=0) ?(last=Sys.max_string_length) s =
  let lo = 0 and hi = length s in
  let i = clip ~lo ~hi (wrap first ~hi) in
  let j = clip ~lo ~hi (wrap last ~hi) in
  unsafe_slice i j s

(**T String.slice
   slice ~first:1 ~last:(-3) " foo bar baz" = "foo bar "
   slice "foo" = "foo"
   slice ~first:0 ~last:10 "foo" = "foo"
   slice ~first:(-2) "foo" = "oo"
   slice ~first:(-3) ~last:(-1) "foob" = "oo"
   slice ~first:5 ~last:4 "foobarbaz" = ""
**)


let lchop ?(n=1) s =
  if n < 0 then
    invalid_arg "lchop: number of characters to chop is negative"
  else
    let slen = length s in
    if slen <= n then "" else sub s n (slen - n)
(**T String.lchop
   lchop "Weeble" = "eeble"
   lchop "" = ""
   lchop ~n:3 "Weeble" = "ble"
   lchop ~n:1000 "Weeble" = ""
**)

let rchop ?(n=1) s =
  if n < 0 then
    invalid_arg "rchop: number of characters to chop is negative"
  else
    let slen = length s in
    if slen <= n then "" else sub s 0 (slen - n)
(**T String.rchop
   rchop "Weeble" = "Weebl"
   rchop "" = ""
   rchop ~n:3 "Weeble" = "Wee"
   rchop ~n:1000 "Weeble" = ""
**)

let of_int = string_of_int
(**T String.of_int
   of_int 56 = "56"
   of_int (-1) = "-1"
**)

let of_float = string_of_float

let of_char = make 1
(**T String.of_char
   of_char 's' = "s"
   of_char '\000' = "\000"
**)

let to_int s = int_of_string s
(**T String.to_int
   to_int "8_480" = to_int "0x21_20"
   try ignore(to_int ""); false with Failure "int_of_string" -> true
   try ignore(to_int "2,3"); false with Failure "int_of_string" -> true
**)

let to_float s = float_of_string s
(**T String.to_float
   to_float "12.34e-1" = to_float "1.234"
   to_float "1" = 1.
   try ignore(to_float ""); false with Failure _ -> true
**)

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
(**T String.enum
   "" |> enum |> List.of_enum = []
   "foo" |> enum |> List.of_enum = ['f'; 'o'; 'o']
**)

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
(**T String.backwards
   "" |> backwards |> of_enum = ""
   "foo" |> backwards |> of_enum = "oof"
 **)

let of_enum e =
  let l = BatEnum.count e in
  let s = create l in
  let i = ref 0 in
    BatEnum.iter (fun c -> unsafe_set s (BatRef.post_incr i) c) e;
    s
(**T String.of_enum
    Enum.init 3 (fun i -> char_of_int (i + int_of_char '0')) |> of_enum = "012"
    Enum.init 0 (fun i -> ' ') |> of_enum = ""
**)

let of_backwards e =
  let l = BatEnum.count e in
  let s = create l in
  let i = ref (l - 1) in
    BatEnum.iter (fun c -> unsafe_set s (BatRef.post_decr i) c) e;
    s
(**T String.of_backwards
   "" |> enum |> of_backwards = ""
   "foo" |> enum |> of_backwards = "oof"
   "foo" |> backwards |> of_backwards = "foo"
**)

let map f s =
	let len = length s in
	let sc = create len in
	for i = 0 to len - 1 do
		unsafe_set sc i (f (unsafe_get s i))
	done;
	sc
(**T String.map
   map Char.uppercase "Five" = "FIVE"
   map Char.uppercase "" = ""
   map (String.of_char |- failwith) "" = ""
**)

let filter_map f s =
  let len = length s          in
  let sc  = Buffer.create len in
    for i = 0 to len - 1 do
      match f (unsafe_get s i) with
	| Some c -> Buffer.add_char sc c
	| None   -> ()
    done;
    Buffer.contents sc
(**T String.filter_map
   filter_map (function 'a'..'z' as c -> Some (Char.uppercase c) | _ -> None) "a b c" = "ABC"
**)

let filter f s =
  let len = length s          in
  let sc  = Buffer.create len in
    for i = 0 to len - 1 do
      let c = unsafe_get s i in
	if f c then Buffer.add_char sc c
    done;
    Buffer.contents sc
(**T String.filter
   String.filter ((<>) ' ') "a b c" = "abc"
**)

(* fold_left and fold_right by Eric C. Cooper *)
let fold_left f init str =
  let n = String.length str in
  let rec loop i result =
    if i = n then result
    else loop (i + 1) (f result str.[i])
  in
  loop 0 init
(**T String.fold_left
   fold_left (fun li c -> c::li) [] "foo" = ['o';'o';'f']
   fold_left max 'a' "apples" = 's'
**)


let fold_right f str init =
  let n = String.length str in
  let rec loop i result =
    if i = 0 then result
    else
      let i' = i - 1 in
      loop i' (f str.[i'] result)
  in
  loop n init
(**T String.fold_right
   fold_right List.cons "foo" [] = ['f';'o';'o']
   fold_right (fun c a -> if c = ' ' then a+1 else a) "a b c" 0 = 2
**)

let iteri f str =
  for i = 0 to (String.length str) - 1 do f i str.[i] done

(*** iteri
  let letter_positions word =
    let positions = Array.make 256 [] in
    let count_letter pos c =
      positions.(int_of_char c) <- pos :: positions.(int_of_char c) in
    String.iteri count_letter word;
    Array.mapi (fun c pos -> (char_of_int c, List.rev pos)) positions
    |> Array.to_list
    |> List.filter (fun (c,pos) -> pos <> [])
  in
 assert_equal ~msg:"String.iteri test"
     (letter_positions "hello")
     ['e',[1]; 'h',[0]; 'l',[2;3]; 'o',[4] ]
**)

(* explode and implode from the OCaml Expert FAQ. *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
(**T String.explode
   explode "foo" = ['f'; 'o'; 'o']
   explode "" = []
**)


let to_list = explode
(**T String.to_list
   to_list "string" |> List.interleave ';' |> of_list = "s;t;r;i;n;g"
**)

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l
(**T String.implode
   implode ['b';'a';'r'] = "bar"
   implode [] = ""
**)


let of_list = implode
(**T String.of_list
   ['c'; 'h'; 'a'; 'r'; 's'] |> of_list = "chars"
   [] |> of_list = ""
**)

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
(**T String.replace_chars
   replace_chars (function ' ' -> "(space)" | c -> of_char c) "foo bar" = "foo(space)bar"
   replace_chars (fun _ -> "") "foo" = ""
   replace_chars (fun _ -> assert false) "" = ""
**)

let replace ~str ~sub ~by =
	try
		let i = find str sub in
		(true, (slice ~last:i str) ^ by ^ 
                   (slice ~first:(i+(String.length sub)) str))
        with
		Not_found -> (false, String.copy str)
(**T String.replace
   replace "foobarbaz" "bar" "rab" = (true, "foorabbaz")
   replace "foo" "bar" "" = (false, "foo")
**)

let repeat s n =
  let buf = Buffer.create ( n * (String.length s) ) in
    for i = 1 to n do Buffer.add_string buf s done;
    Buffer.contents buf
(**T String.repeat
   repeat "fo" 4 = "fofofofo"
   repeat "fo" 0 = ""
   repeat "" 4 = ""
**)

let trim s =
  let len = length s          in
  let rec aux_1 i = (*locate leading whitespaces*)
    if   i = len then None (*The whole string is whitespace*)
    else if BatChar.is_whitespace (unsafe_get s i) then aux_1 (i + 1)
    else Some i in
  match aux_1 0 with
    | None -> ""
    | Some last_leading_whitespace ->
  let rec aux_2 i =
    if   i < 0 then None(*?*)
    else if BatChar.is_whitespace (unsafe_get s i) then aux_2 (i - 1)
    else Some i in
  match aux_2 (len - 1) with
    | None -> ""
    | Some first_trailing_whitespace ->
	unsafe_slice last_leading_whitespace (first_trailing_whitespace + 1) s

(**T String.trim
   trim " \t foo\n  " = "foo"
   trim " foo bar " = "foo bar"
   trim "  \t " = ""
   trim "" = ""
**)

let splice s1 off len s2 =
  let len1 = length s1 and len2 = length s2 in
  let off  = wrap off ~hi:len1 in
  let len  = clip ~lo:0 ~hi:(len1 - off) len in
  let out_len = len1 - len + len2 in
  let s = create out_len in
  blit s1 0 s 0 off; (* s1 before splice point *)
  blit s2 0 s off len2; (* s2 at splice point *)
  blit s1 (off+len) s (off+len2) (len1 - (off+len)); (* s1 after off+len *)
  s
(**T String.splice
   splice "foo bar baz" 3 5 "XXX" = "fooXXXbaz"
   splice "foo bar baz" 5 0 "XXX" = "foo bXXXar baz"
   splice "foo bar baz" 5 (-10) "XXX" = "foo bXXXar baz"
   splice "foo bar baz" 5 50 "XXX" = "foo bXXX"
   splice "foo bar baz" (-4) 2 "XXX" = "foo barXXXaz"
   splice "bar baz" (-4) 2 "XXX" = "barXXXaz"
**)


let is_empty s = length s = 0 
(**T String.is_empty
   String.is_empty ""
   not (String.is_empty "foo")
   String.is_empty (String.make 0 'a')
**)

let icompare s1 s2 = compare (String.lowercase s1) (String.lowercase s2)
(**T String.icompare
   icompare "FOO" "bar" = 1
**)

type t_alias = t (* needed for IString  breaks type t = t *)

module IString =
struct
  type t = t_alias
  let compare = icompare
end


(* Helper functions for numeric_compare *)
let rec pos_diff s1 s2 i =  (* finds the first position where the strings differ *)
  if i = String.length s1 then -2 else 
    if i = String.length s2 then -1 else 
      if s1.[i] = s2.[i] then pos_diff s1 s2 (i+1) 
      else i

(* scans for the end of a numeric value embedded in a string *)
let rec num_end i s = 
  if i >= String.length s then i else
    if BatChar.is_digit s.[i] then num_end (i+1) s else i

(* scans for the beginning of a numeric value embedded in a string *)
let rec num_begin i s = 
  if i < 0 then i+1 else
    if BatChar.is_digit s.[i] then num_begin (i-1) s else i+1


let rec numeric_compare_aux s1 s2 ~off =
  match pos_diff s1 s2 off with
      -2 -> -1 (* < *)
    | -1 -> 1  (* > *)
    | d (* position of first differing character *)
	when BatChar.is_digit s1.[d] && BatChar.is_digit s2.[d] -> 
	(* Scan backwards for start of number *)
      let b1 = num_begin d s1 and b2 = num_begin d s2 in
	(* Scan forwards for end of number *)
      let e1 = num_end d s1 and e2 = num_end d s2 in
(*      Printf.eprintf "Compare: %S & %S @ d:%d b1:%d b2:%d e1:%d e2:%d->" s1 s2 d b1 b2 e1 e2; *)
      let sl1 = (slice s1 ~first:b1 ~last:e1) in
      let sl2 = (slice s2 ~first:b2 ~last:e2) in
(*      Printf.eprintf " %s & %s\n" sl1 sl2;  *)
      let n1 = Big_int.big_int_of_string sl1 in
      let n2 = Big_int.big_int_of_string sl2 in
	  (* FIXME: ignores text after equal numbers -- "a1b" = "a01c" *)
      Big_int.compare_big_int n1 n2 
    | _ -> (* differing character isn't a number in both *)
      Pervasives.compare s1 s2 (* normal compare *)


let numeric_compare s1 s2 =
  (* TODO pluggable transformation functions (for lowercase) *)
  (*  let s1 = String.lowercase s1 and s2 = String.lowercase s2 in*)
  let l1 = String.length s1 and l2 = String.length s2 in
  if l1 = l2 then String.compare s1 s2
  else numeric_compare_aux s1 s2 ~off:0

(**T String.numeric_compare
   numeric_compare "xx43" "xx320" = -1
   numeric_compare "xx3" "xx21" = -1
   numeric_compare "xx02" "xx2" = 0
   numeric_compare "xx20" "xx5" = 1
   numeric_compare "abc" "def" = compare "abc" "def"
   numeric_compare "x50y" "x51y" = -1
**)

(**Q numeric_compare_qt
   (Q.triple Q.printable_string Q.pos_int Q.pos_int) (fun (s,m,n) -> numeric_compare (s^(string_of_int m)) (s^(string_of_int n)) = BatInt.compare m n)
**)

module NumString =
struct
  type t = t_alias
  let compare = numeric_compare
end


let print         = BatInnerIO.nwrite
let println out s = BatInnerIO.nwrite out s; BatInnerIO.write out '\n'
let print_quoted out s = BatPrintf.fprintf out "%S" s
let t_printer paren out x =
  BatInnerIO.write out '"';
  print out (escaped x);
  BatInnerIO.write out '"'

let unquoted_printer paren out x = print out x

(* Beware: the documentation of print_quoted claims that its behavior
   is compatible with this "quote" function. This is currently true as
   they both use "%S", but any change in 'quote' here should be
   careful to preserve this consistency. *)
let quote s = BatPrintf.sprintf2 "%S" s
(**T String.quote
   quote "foo" = "\"foo\""
   quote "\"foo\"" = "\"\\\"foo\\\"\""
   quote "\n" = "\"\\n\""
**)
  
module Exceptionless =
struct
  let find_from str ofs sub =
    try Some (find_from str ofs sub) with Not_found -> None

  let find str sub = find_from str 0 sub

  let rfind_from str suf sub =
    try Some (rfind_from str suf sub) with Not_found -> None

  let rfind str sub = rfind_from str (String.length str - 1) sub

  let to_int s = try Some (to_int s) with Not_found -> None

  let to_float s = try Some (to_float s) with Not_found -> None

  let index s c = try Some (index s c) with Not_found -> None

  let index_from s i c = try Some (index_from s i c) with Not_found -> None

  let rindex_from s i c = try Some (rindex_from s i c) with Not_found -> None

  let rindex s c = try Some (rindex s c) with Not_found -> None

  let split str sep = try Some (split str sep) with Not_found -> None

  let rsplit str sep = try Some (rsplit str sep) with Not_found -> None
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
let t_printer     = t_printer

external of_string : string -> _ t                = "%identity"
external to_string : [`Read | `Write] t -> string = "%identity"
external read_only : [> `Read] t -> [`Read] t     = "%identity"
external write_only: [> `Write] t -> [`Write] t   = "%identity"

external length : _ t  -> int = "%string_length"
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
