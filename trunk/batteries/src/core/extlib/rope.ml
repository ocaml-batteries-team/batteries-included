(* 
 * Rope: Rope: an implementation of the data structure described in
 *   
 * Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
 * strings. Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.
 * 
 * Motivated by Luca de Alfaro's extensible array implementation Vec.
 * 
 * Copyright (C) 2007 Mauricio Fernandez <mfp@acm.org>
 * Copyright (C) 2008 Edgar Friendly <thelema314@gmail.com>
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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
TYPE_CONV_PATH "Batteries.Data.Text.Rope" (*For Sexplib, Bin-prot...*)
open ExtUTF8
 
(* =begin ignore *)
type t =
    Empty
  (* left, left size, right, right size, height *)
  | Concat of t * int * t * int * int
  (* length in unicode characters, string data *)
  | Leaf of int * UTF8.t
 
type forest_element = { mutable c : t; mutable len : int }
 
let str_append = UTF8.append
let empty_str = UTF8.empty
let string_of_string_list l = UTF8.join UTF8.empty l
 
 
(* 48 limits max rope size to 220GB on 64 bit,
* ~ 700MB on 32bit (length fields overflow after that) *)
let max_height = 48
 
(* actual size will be that plus 1 word header;
* the code assumes it's an even num.
* 256 gives up to a 50% overhead in the worst case (all leaf nodes near
* half-filled *)
let leaf_size = 256 (* utf-8 characters, not bytes *)
(* =end *)
 
(* =begin code *)
 
exception Out_of_bounds
 
let empty = Empty
 


(* by construction, there cannot be Empty or Leaf "" leaves *)
let is_empty = function Empty -> true | _ -> false
 
let height = function
    Empty | Leaf _ -> 0
  | Concat(_,_,_,_,h) -> h
 
let rec length = function
    Empty -> 0
  | Leaf (l,_) -> l
  | Concat(_,cl,_,cr,_) -> cl + cr
 
let make_concat l r =
  let hl = height l and hr = height r in
  let cl = length l and cr = length r in
    Concat(l, cl, r, cr, if hl >= hr then hl + 1 else hr + 1)
 
let min_len =
  let fib_tbl = Array.make max_height 0 in
  let rec fib n = match fib_tbl.(n) with
      0 ->
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
    Empty -> r
  | Leaf _ | Concat(_,_,_,_,_) ->
      match r with
          Empty -> l
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
    Empty -> ()
  | Leaf _ -> add_forest forest rope len
  | Concat(l,cl,r,cr,h) when h >= max_height || len < min_len.(h) ->
      balance_insert l cl forest;
      balance_insert r cr forest
  | x -> add_forest forest x len (* function or balanced *)
 
let balance r =
  match r with
      Empty | Leaf _ -> r
    | _ ->
        let forest = Array.init max_height (fun _ -> {c = Empty; len = 0}) in
          balance_insert r (length r) forest;
          concat_forest forest
 
let bal_if_needed l r =
  let r = make_concat l r in
    if height r < max_height then r else balance r
 
let concat_str l = function
    Empty | Concat(_,_,_,_,_) -> invalid_arg "concat_str"
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
 
let concat l = function
    Empty -> l
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
 
let prepend_char c r = concat (Leaf (1,(UTF8.make 1 c))) r
 
let rec get i = function
    Empty -> raise Out_of_bounds
  | Leaf (lens, s) ->
      if i >= 0 && i < lens then UTF8.unsafe_get s i
      else raise Out_of_bounds
  | Concat (l, cl, r, cr, _) ->
      if i < cl then get i l
      else get (i - cl) r
 
let rec set i (v:ExtUChar.UChar.t) = function
    Empty -> raise Out_of_bounds
  | Leaf (lens, s) ->
      if i >= 0 && i < lens then
	let s = UTF8.copy_set s i v in
          Leaf (lens, s)
      else raise Out_of_bounds
  | Concat(l, cl, r, cr, _) ->
      if i < cl then concat (set i v l) r
      else concat l (set (i - cl) v r)
 
let of_ustring s =
  let lens = UTF8.length s in
  if lens = 0 then Empty
  else
    let min (x:int) (y:int) = if x <= y then x else y in
    let rec loop r i =
      if i < lens then (* lens - i > 0, thus Leaf "" can't happen *)
  let slice_size = min (lens-i) leaf_size in
  let new_r = concat r (Leaf (slice_size, (UTF8.sub s i slice_size))) in
    loop new_r (i + leaf_size)
      else
        r
    in loop Empty 0
 
let rec make len c =
  let rec concatloop len i r =
    if i <= len then
      concatloop len (i * 2) (concat r r)
    else r
  in
    if len = 0 then Empty
    else if len <= leaf_size then Leaf (len, (UTF8.make len c))
    else
      let rope = concatloop len 2 (of_ustring (UTF8.make 1 c)) in
        concat rope (make (len - length rope) c)
 
let of_char c = make 1 c

let rec sub start len = function
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
          else sub 0 len l
        else if start > cl then Empty
        else if start + len >= cl then
          sub start (cl - start) l
        else sub start len l in
      let right =
        if start <= cl then
          let upto = start + len in
            if upto = cl + cr then r
            else if upto < cl then Empty
            else sub 0 (upto - cl) r
        else sub (start - cl) len r
      in
        concat left right
 
let insert start rope r =
  concat (concat (sub 0 start r) rope) (sub start (length r - start) r)
 
let remove start len r =
  concat (sub 0 start r) (sub (start + len) (length r - start - len) r)
 
let to_ustring r =
  let rec strings l = function
      Empty -> l
    | Leaf (_,s) -> s :: l
    | Concat(left,_,right,_,_) -> strings (strings l right) left
  in
    string_of_string_list (strings [] r)
 
let rec iter f = function
    Empty -> ()
  | Leaf (_,s) -> UTF8.iter f s
  | Concat(l,_,r,_,_) -> iter f l; iter f r
 
let iteri f r =
  let rec aux f i = function
    Empty -> ()
  | Leaf (_,s) ->
      let e = UTF8.enum s in
      Enum.iteri (fun j c -> f (i+j) c) e
  | Concat(l,cl,r,_,_) -> aux f i l; aux f (i + cl) r
  in
    aux f 0 r
 
let rec bulk_iter f = function
    Empty -> ()
  | Leaf (_,s) -> f s
  | Concat(l,_,r,_,_) -> bulk_iter f l; bulk_iter f r
 
let rec range_iter f start len = function
    Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds
  | Leaf (lens, s) ->
      let n = start + len in
      if start >= 0 && len >= 0 && n <= lens then
  for i = start to n - 1 do
          f (UTF8.unsafe_get s i) (*TODO: use enum to iterate efficiently*)
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
 
let rec fold f a = function
    Empty -> a
  | Leaf (_,s) ->
      Enum.fold (fun a c -> f c a) a (UTF8.enum s)
  | Concat(l,_,r,_,_) -> fold f (fold f a l) r
 
let rec bulk_fold f a = function
  | Empty                  -> a
  | Leaf   (_, s)          -> f a s
  | Concat (l, _, r, _, _) -> bulk_fold f (bulk_fold f a l) r

(*let rec enum = function (* return an enumeration of UChars --*)
    Empty                  -> Enum.empty ()
  | Leaf (_,s)             -> UTF8.enum s
  | Concat (l, _, r, _, _) -> Enum.append (enum l) (enum r)*)

let enum s =
  let rec aux = function
    Empty                  -> Enum.empty ()
  | Leaf (_,s)             -> UTF8.enum s
  | Concat (l, _, r, _, _) -> Enum.append (Enum.delay (fun () -> aux l)) 
                                          (Enum.delay (fun () -> aux r))
  in aux s

let backwards s = 
  let rec aux = function
    Empty                  -> Enum.empty ()
  | Leaf (_,s)             -> UTF8.backwards s
  | Concat (l, _, r, _, _) -> Enum.append (Enum.delay (fun () -> aux r)) 
                                          (Enum.delay (fun () -> aux l))
  in aux s

let bulk_enum s = 
  let rec aux = function
    | Empty      -> Enum.empty ()
    | Leaf(_, s) -> Enum.singleton s
    | Concat(l, _, r, _, _) -> Enum.append (Enum.delay (fun () -> aux l)) 
                                           (Enum.delay (fun () -> aux r))
  in aux s

(*Probably useless
let bulk_backwards s = 
  let rec aux = function
    | Empty      -> Enum.empty ()
    | Leaf(_, s) -> Enum.singleton s
    | Concat(l, _, r, _, _) -> Enum.append (Enum.delay (fun () -> aux r)) 
                                           (Enum.delay (fun () -> aux l))
  in aux s
*)

let of_enum e =
  let get_leaf () =
    Labels.label
      (fun return ->
	 let b = Buffer.create 256 in
	   for i = 1 to 256 do
	     match Enum.get e with
		 None   -> Labels.recall return (false, UTF8.of_string_unsafe (Buffer.contents b))
	       | Some c -> Buffer.add_string b (UTF8.to_string_unsafe (UTF8.of_char c))
	   done;
	   (true, UTF8.of_string_unsafe (Buffer.contents b) ))
  in
  let rec loop r = (* concat 256 characters at a time *)
    match get_leaf () with
	(true,  us) -> loop     (concat r (of_ustring us))
      | (false, us) -> concat r (of_ustring us)
  in
    loop Empty

let of_backwards e =(*(Yoric) I'll keep the implementation simple at least until I understand [of_enum]*)
  Enum.fold (fun c acc -> concat acc (of_char c)) Empty e
  
let of_bulk_enum e =
  Enum.fold (fun s acc -> concat acc (of_ustring s)) Empty e
(*Probably useless 
let of_bulk_backwards e =
  Enum.fold (fun s acc -> concat (of_ustring s) acc) Empty e
*)
module CE = CamomileLibrary.CharEncoding.Configure(CamomileLibrary.CamomileDefaultConfig)
 
let of_latin1 s =
  of_ustring (UTF8.of_string (CE.recode_string CE.latin1 CE.utf8 s))
 
let sexp_of_t t =
  UTF8.sexp_of_t (to_ustring t)
let t_of_sexp s =
  of_ustring (UTF8.t_of_sexp s)

let print out t =
  bulk_iter (fun us -> InnerIO.nwrite out (UTF8.to_string us)) t

let lowercase s =
  bulk_fold (fun acc c -> concat acc (of_ustring (UTF8.lowercase c)))  Empty s

let uppercase s =
  bulk_fold (fun acc c -> concat acc (of_ustring (UTF8.uppercase c)))  Empty s
(* =end *)
