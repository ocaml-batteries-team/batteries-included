(*
 * BatText - Unicode text library
 *
 * Copyright (C) 2012 The Batteries Included Team
 * Copyright (C) 2007 Mauricio Fernandez <mfp@acm.org>
 * Copyright (C) 2008 Edgar Friendly <thelema314@gmail.com>
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
 *
 * Rope: Rope: an implementation of the data structure described in
 *
 * Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
 * strings. Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.
 *
 * Motivated by Luca de Alfaro's extensible array implementation Vec.
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



module UTF8 = BatUTF8
module UChar = BatUChar

(**Low-level optimization*)
let int_max (x:int) (y:int) = if x < y then y else x
let int_min (x:int) (y:int) = if x < y then x else y

let splice s1 off len s2 =
  let len1 = String.length s1 and len2 = String.length s2           in
  let off  = if off < 0 then len1 + off - 1 else off  in
  let len  = int_min (len1 - off) len                 in
  let out_len = len1 - len + len2                     in
  let s = String.create out_len in
  String.blit s1 0 s 0 off; (* s1 before splice point *)
  String.blit s2 0 s off len2; (* s2 at splice point *)
  String.blit s1 (off+len) s (off+len2) (len1 - (off+len)); (* s1 after off+len *)
  s

type t =
    Empty                             (**An empty rope*)
  | Concat of t * int * t * int * int (**[Concat l ls r rs h] is the concatenation of
                                         ropes [l] and [r], where [ls] is the total
										 length of [l], [rs] is the length of [r]
										 and [h] is the height of the node in the
										 tree, used for rebalancing. *)
  | Leaf of int * UTF8.t              (**[Leaf l t] is string [t] with length [l],
										 measured in number of Unicode characters.*)

type forest_element = { mutable c : t; mutable len : int }

let str_append = (^)
let empty_str = ""
let string_of_string_list l = String.concat empty_str l



(* 48 limits max rope size to 220GB on 64 bit,
 * ~ 700MB on 32bit (length fields overflow after that) *)
let max_height = 48

(* actual size will be that plus 1 word header;
 * the code assumes it's an even num.
 * 256 gives up to a 50% overhead in the worst case (all leaf nodes near
 * half-filled *)
let leaf_size = 256 (* utf-8 characters, not bytes *)


(* MAIN CODE STARTS HERE *)

exception Out_of_bounds

let empty = Empty

(* by construction, there cannot be Empty or Leaf "" leaves *)
let is_empty = function Empty -> true | _ -> false

let height = function
  | Empty | Leaf _ -> 0
  | Concat(_,_,_,_,h) -> h

let length = function
  | Empty -> 0
  | Leaf (l,_) -> l
  | Concat(_,cl,_,cr,_) -> cl + cr

let make_concat l r =
  let hl = height l and hr = height r in
  let cl = length l and cr = length r in
  Concat(l, cl, r, cr, if hl >= hr then hl + 1 else hr + 1)

let min_len =
  let fib_tbl = Array.make max_height 0 in
  let rec fib n = match fib_tbl.(n) with
    | 0 ->
      let last = fib (n - 1) and prev = fib (n - 2) in
      let r = last + prev in
      let r = if r > last then r else last in (* check overflow *)
      fib_tbl.(n) <- r; r
    | n -> n
  in
  fib_tbl.(0) <- leaf_size + 1; fib_tbl.(1) <- 3 * leaf_size / 2 + 1;
  Array.init max_height (fun i -> if i = 0 then 1 else fib (i - 1))

let max_length = min_len.(Array.length min_len - 1)

let concat_fast l r = match l with
  | Empty -> r
  | Leaf _ | Concat(_,_,_,_,_) ->
    match r with
    | Empty -> l
    | Leaf _ | Concat(_,_,_,_,_) -> make_concat l r

(* based on Hans-J. Boehm's *)
let add_forest forest rope len =
  let i = ref 0 in
  let sum = ref empty in
  while len > min_len.(!i+1) do
    if forest.(!i).c <> Empty then begin
      sum := concat_fast forest.(!i).c !sum;
      forest.(!i).c <- Empty
    end;
    incr i
  done;
  sum := concat_fast !sum rope;
  let sum_len = ref (length !sum) in
  while !sum_len >= min_len.(!i) do
    if forest.(!i).c <> Empty then begin
      sum := concat_fast forest.(!i).c !sum;
      sum_len := !sum_len + forest.(!i).len;
      forest.(!i).c <- Empty;
    end;
    incr i
  done;
  decr i;
  forest.(!i).c <- !sum;
  forest.(!i).len <- !sum_len

let concat_forest forest =
  Array.fold_left (fun s x -> concat_fast x.c s) Empty forest

let rec balance_insert rope len forest = match rope with
  | Empty -> ()
  | Leaf _ -> add_forest forest rope len
  | Concat(l,cl,r,cr,h) when h >= max_height || len < min_len.(h) ->
    balance_insert l cl forest;
    balance_insert r cr forest
  | x -> add_forest forest x len (* function or balanced *)

let balance r =
  match r with
  | Empty | Leaf _ -> r
  | _ ->
    let forest = Array.init max_height (fun _ -> {c = Empty; len = 0}) in
    balance_insert r (length r) forest;
    concat_forest forest

let bal_if_needed l r =
  let r = make_concat l r in
  if height r < max_height then r else balance r

let concat_str l = function
  | Empty | Concat(_,_,_,_,_) -> invalid_arg "concat_str"
  | Leaf (lenr, rs) as r ->
    match l with
    | Empty -> r
    | Leaf (lenl, ls) ->
      let slen = lenr + lenl in
      if slen <= leaf_size then Leaf ((lenl+lenr),(str_append ls rs))
      else make_concat l r (* height = 1 *)
    | Concat(ll, cll, Leaf (lenlr ,lrs), clr, h) ->
      let slen = clr + lenr in
      if clr + lenr <= leaf_size then
        Concat(ll, cll, Leaf ((lenlr + lenr),(str_append lrs rs)), slen, h)
      else
        bal_if_needed l r
    | _ -> bal_if_needed l r

let append_char c r = concat_str r (Leaf (1, (UTF8.make 1 c)))

let append l = function
  | Empty -> l
  | Leaf _ as r -> concat_str l r
  | Concat(Leaf (lenrl,rls),rlc,rr,rc,h) as r ->
    (match l with
       Empty -> r
     | Concat(_,_,_,_,_) -> bal_if_needed l r
     | Leaf (lenl, ls) ->
       let slen = rlc + lenl in
       if slen <= leaf_size then
         Concat(Leaf((lenrl+lenl),(str_append ls rls)), slen, rr, rc, h)
       else
         bal_if_needed l r)
  | r -> (match l with Empty -> r | _ -> bal_if_needed l r)

let ( ^^^ ) = append

let prepend_char c r = append (Leaf (1,(UTF8.make 1 c))) r

let get r i =
  let rec aux i = function
      Empty -> raise Out_of_bounds
    | Leaf (lens, s) ->
      if i >= 0 && i < lens then UTF8.get s i
      else raise Out_of_bounds
    | Concat (l, cl, r, cr, _) ->
      if i < cl then aux i l
      else aux (i - cl) r
  in
  aux i r

let copy_set us cpos c =
  let ipos = UTF8.ByteIndex.of_char_idx us cpos in
  let jpos = UTF8.ByteIndex.next us ipos in
  let i = UTF8.ByteIndex.to_int ipos
  and j = UTF8.ByteIndex.to_int jpos in
  splice us i (j-i) (UTF8.of_char c)

let set r i v =
  let rec aux i = function
      Empty -> raise Out_of_bounds
    | Leaf (lens, s) ->
      if i >= 0 && i < lens then
        let s = copy_set s i v in
        Leaf (lens, s)
      else raise Out_of_bounds
    | Concat(l, cl, r, cr, _) ->
      if i < cl then append (aux i l) r
      else append l (aux (i - cl) r)
  in
  aux i r


module Iter = struct

  (* Iterators are used for iterating efficiently over multiple ropes
     at the same time *)

  type iterator = {
    (* Current leaf in which the iterator is *)
    mutable leaf : UTF8.t;
    (* Current byte position of the iterator *)
    mutable idx : UTF8.ByteIndex.b_idx;
    (* Ropes not yet visited *)
    mutable rest : t list;
  }

  type t = iterator option

  (* Initial iterator state: *)
  let make rope = { leaf = UTF8.empty;
                    idx = UTF8.ByteIndex.first;
                    rest = if rope = Empty then [] else [rope] }

  let rec next_leaf = function
    | Empty :: l ->
      next_leaf l
    | Leaf(len, str) :: l ->
      Some(str, l)
    | Concat(left, left_len, right, right_len, height) :: l ->
      next_leaf (left :: right :: l)
    | [] ->
      None

  (* Advance the iterator to the next position, and return current
     character: *)
  let rec next iter =
    if UTF8.ByteIndex.at_end iter.leaf iter.idx then
      (* We are at the end of the current leaf, find another one: *)
      match next_leaf iter.rest with
      | None ->
        None
      | Some(leaf, rest) ->
        iter.leaf <- leaf;
        iter.idx <- UTF8.ByteIndex.next leaf UTF8.ByteIndex.first;
        iter.rest <- rest;
        Some(UTF8.ByteIndex.look leaf UTF8.ByteIndex.first)
    else begin
      (* Just advance in the current leaf: *)
      let ch = UTF8.ByteIndex.look iter.leaf iter.idx in
      iter.idx <- UTF8.ByteIndex.next iter.leaf iter.idx;
      Some ch
    end

  (* Same thing but map leafs: *)
  let rec next_map f iter =
    if UTF8.ByteIndex.at_end iter.leaf iter.idx then
      match next_leaf iter.rest with
      | None ->
        None
      | Some(leaf, rest) ->
        let leaf = f leaf in
        iter.leaf <- leaf;
        iter.idx <- UTF8.ByteIndex.next leaf UTF8.ByteIndex.first;
        iter.rest <- rest;
        Some(UTF8.ByteIndex.look leaf UTF8.ByteIndex.first)
    else begin
      let ch = UTF8.ByteIndex.look iter.leaf iter.idx in
      iter.idx <- UTF8.ByteIndex.next iter.leaf iter.idx;
      Some ch
    end

  (* Same thing but in reverse order: *)

  let rec prev_leaf = function
    | Empty :: l ->
      prev_leaf l
    | Leaf(len, str) :: l ->
      Some(str, l)
    | Concat(left, left_len, right, right_len, height) :: l ->
      prev_leaf (right :: left :: l)
    | [] ->
      None

  let prev iter =
    if iter.idx = UTF8.ByteIndex.first then
      match prev_leaf iter.rest with
      | None ->
        None
      | Some(leaf, rest) ->
        iter.leaf <- leaf;
        iter.idx <- UTF8.ByteIndex.last leaf;
        iter.rest <- rest;
        Some(UTF8.ByteIndex.look leaf iter.idx)
    else begin
      iter.idx <- UTF8.ByteIndex.prev iter.leaf iter.idx;
      Some(UTF8.ByteIndex.look iter.leaf iter.idx)
    end
end

(* Can be improved? *)
let compare a b =
  let ia = Iter.make a and ib = Iter.make b in
  let rec loop _ =
    match Iter.next ia, Iter.next ib with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some ca, Some cb ->
      match UChar.compare ca cb with
      | 0 -> loop ()
      | n -> n
  in
  loop ()

let of_ustring ustr =
  (* We need fast access to raw bytes: *)
  let bytes =  ustr in
  let byte_length = String.length bytes in

  (* - [rope] is the accumulator
     - [start_byte_idx] is the byte position of the current slice
     - [current_byte_idx] is the current byte position
     - [slice_size] is the number of unicode characters contained
     between [start_byte_idx] and [current_byte_idx] *)
  let rec loop rope start_byte_idx current_byte_idx slice_size =
    if current_byte_idx = byte_length then begin

      if slice_size = 0 then
        rope
      else
        add_slice rope start_byte_idx current_byte_idx slice_size

    end else begin

      if slice_size = leaf_size then
        (* We have enough unicode characters for this slice, extract
           it and add a leaf to the rope: *)
        loop (add_slice rope start_byte_idx current_byte_idx slice_size)
          current_byte_idx current_byte_idx 0
      else
        let next_byte_idx = UTF8.next ustr current_byte_idx in
        loop rope start_byte_idx next_byte_idx (slice_size + 1)
    end
  and add_slice rope start_byte_idx end_byte_idx slice_size =
    append rope (Leaf(slice_size,
        (* This is correct, we are just extracting a
           sequence of well-formed UTF-8 encoded unicode
           characters: *)
        UTF8.of_string_unsafe
          (String.sub bytes start_byte_idx (end_byte_idx - start_byte_idx))))
  in
  loop Empty 0 0 0

let of_string s =
  (* Validate + unsafe to avoid an extra copy (it is OK because
     of_ustring do not reuse its argument in the resulting rope): *)
  UTF8.validate s;
  of_ustring (UTF8.of_string_unsafe s)

let append_us r us = append r (of_ustring us)

let rec make len c =
  let rec concatloop len i r =
    if i <= len then
      (*TODO: test for sharing among substrings *)
      concatloop len (i * 2) (append r r)
    else r
  in
  if len = 0 then Empty
  else if len <= leaf_size then Leaf (len, (UTF8.make len c))
  else
    let rope = concatloop len 2 (of_ustring (UTF8.make 1 c)) in
    append rope (make (len - length rope) c)

let of_uchar c = make 1 c
let of_char c = of_uchar (UChar.of_char c)

let sub r start len =
  let rec aux start len = function
      Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds else Empty
    | Leaf (lens, s) ->
      if len < 0 || start < 0 || start + len > lens then
        raise Out_of_bounds
      else if len > 0 then (* Leaf "" cannot happen *)
        (try Leaf (len, (UTF8.sub s start len)) with _ -> raise Out_of_bounds)
      else Empty
    | Concat(l,cl,r,cr,_) ->
      if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
      let left =
        if start = 0 then
          if len >= cl then
            l
          else aux 0 len l
        else if start > cl then Empty
        else if start + len >= cl then
          aux start (cl - start) l
        else aux start len l in
      let right =
        if start <= cl then
          let upto = start + len in
          if upto = cl + cr then r
          else if upto < cl then Empty
          else aux 0 (upto - cl) r
        else aux (start - cl) len r
      in
      append left right
  in aux start len r

let insert start rope r =
  append (append (sub r 0 start) rope) (sub r start (length r - start))

let remove start len r =
  append (sub r 0 start) (sub r (start + len) (length r - start - len))

let to_ustring r =
  let rec strings l = function
    | Empty -> l
    | Leaf (_,s) -> s :: l
    | Concat(left,_,right,_,_) -> strings (strings l right) left
  in
  string_of_string_list (strings [] r)

let rec bulk_iter f = function
  | Empty -> ()
  | Leaf (_,s) -> f s
  | Concat(l,_,r,_,_) -> bulk_iter f l; bulk_iter f r

let rec bulk_iteri ?(base=0) f = function
  | Empty -> ()
  | Leaf (_,s) -> f base s
  | Concat(l,cl,r,_,_) ->
    bulk_iteri ~base f l;
    bulk_iteri ~base:(base+cl) f r

let rec iter f = function
  | Empty -> ()
  | Leaf (_,s) -> UTF8.iter f s
  | Concat(l,_,r,_,_) -> iter f l; iter f r


let rec iteri ?(base=0) f = function
  | Empty -> ()
  | Leaf (_,s) ->
    UTF8.iteri (fun c j -> f (base + j) c) s
  | Concat(l,cl,r,_,_) -> iteri ~base f l; iteri ~base:(base + cl) f r


let rec bulk_iteri_backwards ~top f = function
  | Empty -> ()
  | Leaf (lens,s) -> f (top-lens) s (* gives f the base position, not the top *)
  | Concat(l,_,r,cr,_) ->
    bulk_iteri_backwards ~top f r;
    bulk_iteri_backwards ~top:(top-cr) f l

let rec range_iter f start len = function
  | Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds
  | Leaf (lens, s) ->
    let n = start + len in
    if start >= 0 && len >= 0 && n <= lens then
      for i = start to n - 1 do
        f (UTF8.look s (UTF8.nth s i)) (*TODO: use enum to iterate efficiently*)
      done
    else raise Out_of_bounds
  | Concat(l,cl,r,cr,_) ->
    if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
    if start < cl then begin
      let upto = start + len in
      if upto <= cl then
        range_iter f start len l
      else begin
        range_iter f start (cl - start) l;
        range_iter f 0 (upto - cl) r
      end
    end else begin
      range_iter f (start - cl) len r
    end

let rec range_iteri f ?(base = 0) start len = function
  | Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds
  | Leaf (lens, s) ->
    let n = start + len in
    if start >= 0 && len >= 0 && n <= lens then
      for i = start to n - 1 do
        f (base+i) (UTF8.look s (UTF8.nth s i))
        (*TODO: use enum to iterate efficiently*)
      done
    else raise Out_of_bounds
  | Concat(l,cl,r,cr,_) ->
    if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
    if start < cl then begin
      let upto = start + len in
      if upto <= cl then
        range_iteri f ~base start len l
      else begin
        range_iteri f ~base start (cl - start) l;
        range_iteri f ~base:(base + cl - start) 0 (upto - cl) r
      end
    end else begin
      range_iteri f ~base (start - cl) len r
    end

let rec fold f a = function
  | Empty -> a
  | Leaf (_,s) ->
    UTF8.fold (fun a c -> f a c) a s
  | Concat(l,_,r,_,_) -> fold f (fold f a l) r

let rec bulk_fold f a = function
  | Empty                  -> a
  | Leaf   (_, s)          -> f a s
  | Concat (l, _, r, _, _) -> bulk_fold f (bulk_fold f a l) r

let to_string t =
  (* We use unsafe version to avoid the copy of the non-reachable
     temporary string: *)
  UTF8.to_string_unsafe (to_ustring t)

let init len f = Leaf (len, UTF8.init len f)

let of_string_unsafe s = of_ustring (UTF8.of_string_unsafe s)
let of_int i = of_string_unsafe (string_of_int i)
let of_float f = of_string_unsafe (string_of_float f)

let to_int r = int_of_string (UTF8.to_string_unsafe (to_ustring r))
let to_float r = float_of_string (UTF8.to_string_unsafe (to_ustring r))

let bulk_map f r = bulk_fold (fun acc s -> append_us acc (f s)) Empty r
let map f r = bulk_map (fun s -> UTF8.map f s) r

let bulk_filter_map f r = bulk_fold (fun acc s -> match f s with None -> acc | Some r -> append_us acc r) Empty r
let filter_map f r = bulk_map (UTF8.filter_map f) r

let filter f r = bulk_map (UTF8.filter f) r

let left r len  = sub r 0 len
let right r len = let rlen = length r in sub r (rlen - len) len
let head = left
let tail r pos = sub r pos (length r - pos)

let index r u =
  let i = Iter.make r in
  let rec loop n =
    match Iter.next i with
    | None  -> raise Not_found
    | Some u' ->
      if UChar.eq u u' then n else
        loop (n + 1)
  in
  loop 0


module Return = BatReturn

let index_from r base item =
  Return.with_label (fun label ->
    let index_aux i c =
      if c = item then Return.return label i
    in
    range_iteri index_aux base (length r - base) r;
    raise Not_found)
(*$T index_from
  index_from (of_string "batteries") 0 (BatUChar.of_char 't') = 2
  index_from (of_string "batteries") 3 (BatUChar.of_char 't') = 3
  Result.(catch (index_from (of_string "batteries") 4) (BatUChar.of_char 't') \
            |> is_exn Not_found)
  Result.(catch (index_from (of_string "batteries") 20) (BatUChar.of_char 't') \
            |> is_exn Out_of_bounds)
*)

let rindex r char =
  Return.with_label (fun label ->
    let index_aux i us =
      try
        let p = UTF8.rindex us char in
        Return.return label (p+i)
      with Not_found -> ()
    in
    bulk_iteri_backwards ~top:(length r) index_aux r;
    raise Not_found)

let rindex_from r start char =
  let rsub = left r start in
  (rindex rsub char)

let contains r char =
  Return.with_label (fun label ->
    let contains_aux us =
      if UTF8.contains us char then Return.return label true
    in
    bulk_iter contains_aux r;
    false)

let contains_from r start char =
  Return.with_label (fun label ->
    let contains_aux c = if c = char then Return.return label true in
    range_iter contains_aux start (length r - start) r;
    false)

let rcontains_from = contains_from

let equals r1 r2 = compare r1 r2 = 0

let starts_with r prefix =
  let ir = Iter.make r and iprefix = Iter.make prefix in
  let rec loop _ =
    match Iter.next iprefix with
    | None -> true
    | Some ch1 ->
      match Iter.next ir with
      | None -> false
      | Some ch2 -> UChar.compare ch1 ch2 = 0 && loop ()
  in
  loop ()

let ends_with r suffix =
  let ir = Iter.make r and isuffix = Iter.make suffix in
  let rec loop _ =
    match Iter.prev isuffix with
    | None -> true
    | Some ch1 ->
      match Iter.prev ir with
      | None -> false
      | Some ch2 -> UChar.compare ch1 ch2 = 0 && loop ()
  in
  loop ()

(** find [sub] within [rop] or raises Not_found *)
let find_from rop ofs sub_rop =
  let len = length rop in
  if ofs < 0 || ofs > len then raise Out_of_bounds;
  let matchlen = length sub_rop in
  let sub_rop = to_ustring sub_rop in
  let check_at pos = sub_rop = (to_ustring (sub rop pos matchlen)) in
  (* TODO: inefficient *)
  Return.with_label (fun label ->
    for i = ofs to len - matchlen do
      if check_at i then Return.return label i
    done;
    raise Not_found)
(*$T find_from
  find_from (of_string "foobarbaz") 4 (of_string "ba") = 6
  find_from (of_string "foobarbaz") 7 (of_string "") = 7
  Result.(catch (find_from (of_string "") 0) (of_string "a") |> is_exn Not_found)
  let foo = of_string "foo" in Result.(catch (find_from foo 2) foo |> is_exn Not_found)
  let foo = of_string "foo" in Result.(catch (find_from foo 3) foo |> is_exn Not_found)
  let foo = of_string "foo" in Result.(catch (find_from foo 4) foo |> is_exn Out_of_bounds)
  let foo = of_string "foo" in Result.(catch (find_from foo (-1)) foo |> is_exn Out_of_bounds)
*)

let find rop sub = find_from rop 0 sub

let rfind_from rop suf sub_rop =
  if suf + 1 < 0 || suf + 1 > length rop then raise Out_of_bounds;
  let matchlen = length sub_rop in
  let sub_rop = to_ustring sub_rop in
  let check_at pos = sub_rop = (to_ustring (sub rop pos matchlen)) in
  (* TODO: inefficient *)
  Return.with_label (fun label ->
    for i = suf - matchlen + 1 downto 0 do
      if check_at i then Return.return label i
    done;
    raise Not_found)
(*$T rfind_from
  rfind_from (of_string "foobarbaz") 5 (of_string "ba") = 3
  rfind_from (of_string "foobarbaz") 7 (of_string "ba") = 6
  rfind_from (of_string "foobarbaz") 6 (of_string "ba") = 3
  rfind_from (of_string "foobarbaz") 7 (of_string "") = 8
  Result.(catch (rfind_from (of_string "") 3) empty |> is_exn Out_of_bounds)
  Result.(catch (rfind_from (of_string "") (-1)) (of_string "a") |> is_exn Not_found)
  Result.(catch (rfind_from (of_string "foobarbaz") 2) (of_string "ba") |> is_exn Not_found)
  Result.(catch (rfind_from (of_string "foo") 3) (of_string "foo") |> is_exn Out_of_bounds)
  Result.(catch (rfind_from (of_string "foo") (-2)) (of_string "foo") |> is_exn Out_of_bounds)
*)

let rfind rop sub = rfind_from rop (length rop - 1) sub

let exists r_str r_sub = try ignore(find r_str r_sub); true with Not_found -> false

let strip_default_chars = List.map UChar.of_char [' ';'\t';'\r';'\n']
let strip ?(chars=strip_default_chars) rope =
  let rec strip_left n iter =
    match Iter.next iter with
    | None ->
      Empty
    | Some ch when List.mem ch chars ->
      strip_left (n + 1) iter
    | _ ->
      sub rope n (strip_right (length rope - n) (Iter.make rope))
  and strip_right n iter =
    match Iter.prev iter with
    | None ->
      assert false
    | Some ch when List.mem ch chars ->
      strip_right (n - 1) iter
    | _ ->
      n
  in
  strip_left 0 (Iter.make rope)

let lchop = function
  | Empty -> Empty
  | str -> sub str 1 (length str - 1)
let rchop = function
  | Empty -> Empty
  | str -> sub str 0 (length str - 1)


let of_list l =
  let e = ref l in
  let get_leaf () =
    Return.label
      (fun label ->
        let b = Buffer.create 256 in
        for i = 1 to 256 do
          match !e with
            []   -> Return.return label (false, UTF8.of_string_unsafe (Buffer.contents b))
          | c :: rest  -> Buffer.add_string b (UTF8.to_string_unsafe (UTF8.of_char c)); e := rest
        done;
        (true, UTF8.of_string_unsafe (Buffer.contents b) ))
  in
  let rec loop r = (* concat 256 characters at a time *)
    match get_leaf () with
      (true,  us) -> loop     (append r (of_ustring us))
    | (false, us) -> append r (of_ustring us)
  in
  loop Empty

let splice r start len new_sub =
  let start = if start >= 0 then start else (length r) + start in
  append (left r start)
    (append new_sub (tail r (start+len)))

let fill r start len char =
  splice r start len (make len char)

let blit rsrc offsrc rdst offdst len =
  splice rdst offdst len (sub rsrc offsrc len)


let list_reduce f = function [] -> invalid_arg "Empty List"
                           | h::t -> List.fold_left f h t

let concat sep r_list =
  if r_list = [] then empty else
    list_reduce (fun r1 r2 -> append r1 (append sep r2)) r_list

(**T concat
   Text.concat (Text.of_string "xyz") [] = Text.empty
 **)

let escaped r = bulk_map UTF8.escaped r

let replace_chars f r = fold (fun acc s -> append_us acc (f s)) Empty r

let split r sep =
  let i = find r sep in
  head r i, tail r (i+length sep)
(*$T split
  split (of_string "OCaml, the coolest FP language.") (of_char ' ') = \
    (of_string "OCaml,", of_string "the coolest FP language.")
  split (of_string "OCaml, the coolest FP language.") (of_char '.') = \
    (of_string "OCaml, the coolest FP language", empty)
  Result.(catch (split (of_string "OCaml, the coolest FP language.")) \
		(of_char '!') |> is_exn Not_found)
*)

let rsplit (r:t) sep =
  let i = rfind r sep in
  head r i, tail r (i+length sep)
(*$T rsplit
  rsplit (of_string "OCaml, the coolest FP language.") (of_char ' ') = \
    (of_string "OCaml, the coolest FP", of_string "language.")
  rsplit (of_string "OCaml, the coolest FP language.") (of_char 'O') = \
    (empty, of_string "Caml, the coolest FP language.")
  Result.(catch (rsplit (of_string "OCaml, the coolest FP language.")) \
		(of_char '!') |> is_exn Not_found)
*)

(** An implementation of [nsplit] in one pass.

    This implementation traverses the string backwards, hence building
    the list of substrings from the end to the beginning, so as to
    avoid a call to [List.rev].  *)
let nsplit str sep =
  if is_empty str then []
  else let seplen = length sep in
    let rec aux acc ofs = match
      try Some(rfind_from str ofs sep)
      with Not_found -> None
    with
    | Some idx ->
      (* at this point, [idx] to [idx + seplen - 1] contains the
			separator, which is useless to us on the other hand,
			[idx + seplen] to [ofs] contains what's just after the
			separator, which is what we want*)
      let end_of_occurrence = idx + seplen in
      if end_of_occurrence >= ofs then
        aux acc (idx - 1) (*We may be at the end of the string*)
      else
        aux ( sub str end_of_occurrence ( ofs - end_of_occurrence + 1) :: acc ) (idx - 1)
    | None -> (sub str 0 (ofs + 1))::acc
    in
    aux [] (length str - 1 )
(*$T nsplit
  nsplit (of_string "OCaml, the coolest FP language.") (of_char ' ') = \
    List.map of_string ["OCaml,"; "the"; "coolest"; "FP"; "language."]
  nsplit (of_string "OCaml, the coolest FP language.") (of_char 'o') = \
    List.map of_string ["OCaml, the c"; "lest FP language."]
  nsplit (of_string "OCaml, the coolest FP language.") (of_char '!') = \
    List.map of_string ["OCaml, the coolest FP language."]
*)

let join = concat

let slice ?(first=0) ?(last=max_int) s =
  let clip _min _max x = int_max _min (int_min _max x) in
  let i = clip 0 (length s)
      (if (first<0) then (length s) + first else first)
  and j = clip 0 (length s)
      (if (last<0) then (length s) + last else last)
  in
  if i>=j || i=length s then
    Empty
  else
    sub s i (j-i)


let replace ~str ~sub ~by =
  try
    let i = find str sub in
    (true, append (slice ~last:i str)
       (append by (slice ~first:(i+(length sub)) str)))
  with Not_found -> (false, str)


let explode r = List.rev (fold (fun a u -> u :: a) [] r)
(*$T explode
   explode (of_string "foo") = List.map UChar.of_char ['f'; 'o'; 'o']
   explode (of_string "ếẶ") = List.map UChar.chr [0x1ebf; 0x1eb6]
   explode (of_string "") = []
*)

let implode l = of_list l
(*$T implode
   implode (List.map UChar.of_char ['f'; 'o'; 'o']) = of_string "foo"
   implode (List.map UChar.chr [0x1ebf; 0x1eb6]) = of_string "ếẶ"
   implode [] = of_string ""
*)

let of_latin1 s = of_ustring (UTF8.of_latin1 s)


let print out t = bulk_iter (BatIO.nwrite out) t

open BatIO
(** {6 Unicode}*)

(** {7 Reading unicode}

    All these functions assume that the input is UTF-8 encoded.
*)

(*val read_uchar: input -> UChar.t*)
(** read one UChar from a UTF-8 encoded input*)
let read_char i =
  let n0  = read i in
  let len = UTF8.length0 (Char.code n0) in
  if len = 1 then UChar.of_char n0
  else
    let s = String.create len in
    String.set s 0 n0;
    ignore(really_input i s 1 ( len - 1));
    UTF8.get s 0


(*val uchars_of : input -> UChar.t BatEnum.t*)
(** offer the characters of an UTF-8 encoded input as an enumeration*)

let chars_of i = make_enum read_char i

(*val read_rope: input -> int -> Rope.t*)
(** read up to n uchars from a UTF-8 encoded input*)
let read_text i n =
  let rec loop r j =
    if j = 0 then r
    else loop (append_char (read_char i) r) (j-1) (* TODO: make more efficient by appending a string of Rope.leaf_size (256) chars at a time *)
  in
  if n <= 0 then empty
  else loop empty n

(** read the whole contents of a UTF-8 encoded input*)
let read_all i = of_string (BatIO.read_all i)
(* TODO: make efficient - possibly similar to above - buffering leaf_size chars at a time *)

(** read a line of UTF-8*)
let read_line i =
  let line = read_line i in
  UTF8.validate line;
  of_string line

(** offer the lines of a UTF-8 encoded input as an enumeration*)
let lines_of i = BatIO.make_enum read_line i

(** {7 Writing unicode}

    All these functions assume that the output is UTF-8 encoded.*)

let write_string o c = write_string o c

(*val write_uchar: _ output -> UChar.t -> unit*)
let write_char o c = write_string o (UTF8.init 1 (fun _ -> c))

(*val write_rope : _ output -> Rope.t -> unit*)
let write_text = print

(*val write_uline: _ output -> Rope.t -> unit*)
let write_line o r = write_text o r; write o '\n'

(*val write_ulines : _ output -> Rope.t BatEnum.t -> unit*)
let write_lines o re = BatEnum.iter (write_line o) re

(*val write_ropes : _ output -> Rope.t BatEnum.t -> unit*)
let write_texts o re = BatEnum.iter (write_text o) re

(*val write_uchars : _ output -> UChar.t BatEnum.t -> unit*)
let write_chars o uce = BatEnum.iter (write_char o) uce

let sprintf fmt =
  BatPrintf.ksprintf of_string fmt

let ksprintf k fmt =
  BatPrintf.ksprintf (fun s -> k (of_string s)) fmt

let output_text       = print
