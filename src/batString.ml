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

let init len f =
	let s = create len in
	for i = 0 to len - 1 do
		unsafe_set s i (f i)
	done;
	s

let starts_with str p =
  let len = length p in
    if length str < len then false
    else 
      Return.label 
	(fun label ->
	   for i = 0 to len - 1 do
	     if unsafe_get str i <> unsafe_get p i then
	       Return.return label false
	   done;
	   true)


let ends_with str p =
  let el = length p   
  and sl = length str in
  let diff = sl - el  in
    if diff < 0 then false (*string is too short*)
    else
      Return.label
	(fun label ->
	   for i = 0 to el - 1 do
	     if get str (diff + i) <> get p i then
	       Return.return label false
	   done;
	   true)


let find_from str ofs sub = 
  let sublen = length sub in
    if sublen = 0 then ofs (*If [sub] is the empty string, by convention, it may be found wherever we started searching.*)
    else
      let len = length str in
	if len = 0 then invalid_arg "Empty string to search in" else
	if 0 > ofs || ofs >= len then raise (Invalid_argument "index out of bounds")
	else
	Return.label (fun label ->
  	  for i = ofs to len - sublen do
	    let j = ref 0 in
	      while unsafe_get str (i + !j) = unsafe_get sub !j do
		incr j;
		if !j = sublen then Return.return label i
	      done;
	  done;
	  raise Not_found
        )

let find str sub = find_from str 0 sub

let rfind_from str suf sub = 
  let sublen = length sub 
  and len    = length str in
    if sublen = 0 then len
    else
      if len = 0 then invalid_arg "Empty string to search in" else
	if 0 > suf || suf >= len then raise (Invalid_argument "index out of bounds")
	else
	Return.label (fun label ->
  	  for i = suf - sublen + 1 downto 0 do
	    (*Printf.printf "i:%i/suf:%i/sublen:%i/len:%i\n" i suf sublen len;*)
	    let j = ref 0 in
	      while unsafe_get str ( i + !j ) = unsafe_get sub !j do
		incr j;
		if !j = sublen then Return.return label i
	      done;
	  done;
	  raise Not_found
        )

let rfind str sub = rfind_from str (String.length str - 1) sub

let exists str sub =
	try
		ignore(find str sub);
		true
	with
		Not_found -> false

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

let left r len = sub r 0 len
let right r len = let rlen = length r in sub r (rlen - len) len
let head = left
let tail r pos = sub r pos (length r - pos)

let split str sep =
	let p = find str sep in
	let len = length sep in
	let slen = length str in
	sub str 0 p, sub str (p + len) (slen - p - len)

let rsplit str sep = 
  let p = rfind str sep in
  let len = length sep in
  let slen = length str in
    sub str 0 p, sub str (p + len) (slen - p - len)

(**
   An implementation of [nsplit] in one pass.

   This implementation traverses the string backwards, hence building the list
   of substrings from the end to the beginning, so as to avoid a call to [List.rev].
*)
let nsplit str sep =
  if str = "" then []
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

let join = concat

let slice ?(first=0) ?(last=Sys.max_string_length) s =
	let clip _min _max x = max _min (min _max x) in
	let i = clip 0 (length s)
		(if (first<0) then (length s) + first else first)
	and j = clip 0 (length s)
		(if (last<0) then (length s) + last else last)
	in
	if i>=j || i=length s then
		create 0
        else
          	sub s i (j-i)

let lchop s =
	if s = "" then "" else sub s 1 (length s - 1)

let rchop s =
	if s = "" then "" else sub s 0 (length s - 1)

let of_int = string_of_int

let of_float = string_of_float

let of_char = make 1

let to_int s = int_of_string s

let to_float s = float_of_string s

let enum s =
  let l = length s in
  let rec make i =
    Enum.make 
      ~next:(fun () ->
	       if !i = l then
		 raise Enum.No_more_elements
	       else
		 unsafe_get s (Ref.post_incr i)
	    )
      ~count:(fun () -> l - !i)
      ~clone:(fun () -> make (Ref.copy i))
  in
    make (ref 0)

let backwards s =
      let rec make i =
	Enum.make 
	  ~next:(fun () ->
		   if !i <= 0 then
		     raise Enum.No_more_elements
		   else
		     unsafe_get s (Ref.pre_decr i)
		)
	  ~count:(fun () -> !i)
	  ~clone:(fun () -> make (Ref.copy i))
      in
	make (ref (length s))

let of_enum e =
  let l = Enum.count e in
  let s = create l in
  let i = ref 0 in
    Enum.iter (fun c -> unsafe_set s (Ref.post_incr i) c) e;
    s

let of_backwards e =
  let l = Enum.count e in
  let s = create l in
  let i = ref (l - 1) in
    Enum.iter (fun c -> unsafe_set s (Ref.post_decr i) c) e;
    s


let map f s =
	let len = length s in
	let sc = create len in
	for i = 0 to len - 1 do
		unsafe_set sc i (f (unsafe_get s i))
	done;
	sc

let filter_map f s =
  let len = length s          in
  let sc  = Buffer.create len in
    for i = 0 to len - 1 do
      match f (unsafe_get s i) with
	| Some c -> Buffer.add_char sc c
	| None   -> ()
    done;
    Buffer.contents sc

let filter f s =
  let len = length s          in
  let sc  = Buffer.create len in
    for i = 0 to len - 1 do
      let c = unsafe_get s i in
	if f c then Buffer.add_char sc c
    done;
    Buffer.contents sc

(* fold_left and fold_right by Eric C. Cooper *)
let fold_left f init str =
  let n = String.length str in
  let rec loop i result =
    if i = n then result
    else loop (i + 1) (f result str.[i])
  in
  loop 0 init

let fold_right f str init =
  let n = String.length str in
  let rec loop i result =
    if i = 0 then result
    else
      let i' = i - 1 in
      loop i' (f str.[i'] result)
  in
  loop n init

let iteri f str =
  for i = 0 to (String.length str) - 1 do f i str.[i] done
  
(* explode and implode from the OCaml Expert FAQ. *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let to_list = explode

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l

let of_list = implode

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

let replace ~str ~sub ~by =
	try
		let i = find str sub in
		(true, (slice ~last:i str) ^ by ^ 
                   (slice ~first:(i+(String.length sub)) str))
        with
		Not_found -> (false, String.copy str)


let repeat s n =
  let buf = Buffer.create ( n * (String.length s) ) in
    for i = 1 to n do Buffer.add_string buf s done;
    Buffer.contents buf

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
	sub s last_leading_whitespace (first_trailing_whitespace - last_leading_whitespace + 1)

let splice s1 off len s2 =
  let len1 = length s1 and len2 = length s2           in
  let off  = if off < 0 then len1 + off - 1 else off  in
  let len  = int_min (len1 - off) len                 in
  let out_len = len1 - len + len2                     in
  let s = create out_len in
  blit s1 0 s 0 off; (* s1 before splice point *)
  blit s2 0 s off len2; (* s2 at splice point *)
  blit s1 (off+len) s (off+len2) (len1 - (off+len)); (* s1 after off+len *)
  s

let is_empty s = length s = 0 

let icompare s1 s2 = compare (String.lowercase s1) (String.lowercase s2)

type t_alias = t (* needed for IString  breaks type t = t *)

module IString =
struct
  type t = t_alias
  let compare = icompare
end



let numeric_compare s1 s2 =
(* TODO pluggable transformation functions (for lowercase) *)
(*  let s1 = String.lowercase s1 and s2 = String.lowercase s2 in*)
  let l1 = String.length s1 and l2 = String.length s2 in
  let rec pos_diff i =  (* finds the first position where the strings differ *)
    if i = l1 then -2 else if i = l2 then -1
    else if s1.[i] = s2.[i] then pos_diff (i+1) else i
  and num_end i s = (* scans for the end of a numeric value *)
    try (* TODO: bounds check here *)
      if s.[i] >= '0' && s.[i] <= '9' then num_end (i+1) s else i
    with _ -> i-1 (* let ocaml do our bounds checking for us *)
  in
  if l1 = l2 then String.compare s1 s2
  else let d = pos_diff 0 in
  if d = -2 then -1 else if d = -1 then 1 else
    let e1 = num_end d s1 and e2 = num_end d s2 in
    if e1 = d || e2 = d then Pervasives.compare s1 s2
      (*    else if e1 <> e2 then e1 - e2 else Pervasives.compare s1 s2 *)
    else begin
(*      Printf.eprintf "Compare: %s & %s @ d:%d e1:%d e2:%d->" s1 s2 d e1 e2;*)
      let n1 = Int64.of_string (String.sub s1 d (e1-d))
	(* FIXME?: trailing numbers must be less than Int64.max_int *)
      and n2 = Int64.of_string (String.sub s2 d (e2-d)) in
(*      Printf.eprintf " %Ld & %Ld\n" n1 n2;*)
      Int64.compare n1 n2 (* FIXME: ignores text after equal numbers -- "a1b" = "a01c" *)
    end

module NumString =
struct
  type t = t_alias
  let compare = numeric_compare
end


let print         = InnerIO.nwrite
let println out s = InnerIO.nwrite out s; InnerIO.write out '\n'
let print_quoted out s = BatPrintf.fprintf out "%S" s
let t_printer paren out x =
  InnerIO.write out '"';
  print out (escaped x);
  InnerIO.write out '"'

let quote = BatPrintf.sprintf2 "%S"

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
