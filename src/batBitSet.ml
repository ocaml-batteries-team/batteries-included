(*
 * Bitset - Efficient bit sets
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA	02111-1307	USA
 *)

type t = string ref

let capacity t = (String.length !t) * 8

let empty () = ref ""

let create_ sfun c n = (* n is in bits *)
  if n < 0 then invalid_arg ("BitSet."^sfun^": negative size");
  let size = n / 8 + (if n mod 8 = 0 then 0 else 1) in
  ref (String.make size c)

let create =
  create_ "create" '\000'

let copy t = ref (String.copy !t)

let extend t n = (* len in bits *)
  if n > capacity t then
    let t' = create n in
    String.blit !t 0 !t' 0 (String.length !t);
    t := !t'

type bit_op = 
  | Set 
  | Unset
  | Toggle
  | Mem 

let rec apply_bit_op op t x =
  if x < 0 then 
    begin
      let sfun = 
        match op with 
          | Set -> "set" 
          | Unset -> "unset" 
          | Toggle -> "toggle"
          | Mem -> "mem"
      in
        invalid_arg ("BitSet."^sfun^": negative index")
    end
  else if x < capacity t then
    begin
      let pos = x / 8 and delta = x mod 8 in
      let str = !t in
      let c = Char.code (String.unsafe_get str pos) in
      let v = (c lsr delta) land 1 == 1 in (* TODO: mask *)
      let bset c = String.unsafe_set str pos (Char.unsafe_chr c) in
        match op with 
          | Set ->
              if not v then
                bset (c lor (1 lsl delta)); (* TODO: mask *)
              false
          | Unset ->
              if v then
                bset (c lxor (1 lsl delta)); (* TODO: mask *)
              false
          | Toggle ->
              bset (c lxor (1 lsl delta)); (* TODO: mask *)
              false
          | Mem ->
              v
    end
  else
    begin
      match op with
        | Set | Toggle -> 
            extend t x;
            apply_bit_op op t x
        | Unset | Mem ->
            false
    end


let ignore_bool (_b: bool) = ()

let set t x = ignore_bool (apply_bit_op Set t x)

let unset t x = ignore_bool (apply_bit_op Unset t x)

let toggle t x = ignore_bool (apply_bit_op Toggle t x)

let mem t x = 
  if x < 0 then invalid_arg ("BitSet.mem: negative index")
  else if x < capacity t then
    begin
      let pos = x / 8 and delta = x mod 8 in
      let c = Char.code (String.unsafe_get !t pos) in
        (c lsr delta) land 1 == 1 (* TODO: mask *)
    end
  else
    false

let add x t = let dup = copy t in set dup x; dup

let remove x t = let dup = copy t in unset dup x; dup

let put t = 
  function
    | true -> set t
    | false -> unset t

let create_full n =
  let t = create_ "create_full" '\255' n in
  (* Fix the tail *) 
  for i = n to (capacity t) - 1 do 
    unset t i
  done;
  t

let compare t1 t2 =
  failwith "Not implemented"
(*
  let some_msb b = try Some (find_msb b) with Not_found -> None in
  match (some_msb t1, some_msb t2) with
    (None, Some _) -> -1 (* 0-y -> -1 *)
  | (Some _, None) -> 1  (* x-0 ->  1 *)
  | (None, None) -> 0    (* 0-0 ->  0 *)
  | (Some a, Some b) ->  (* x-y *)
      if a < b then -1
      else if a > b then 1
      else
        begin
          (* MSBs differ, we need to scan arrays until we find a
             difference *)
          let ndx = a lsr log_int_size in
          assert (ndx < t1.len && ndx < t2.len);
          try
            for i = ndx downto 0 do
              let b1 = bget t1.data i
              and b2 = bget t2.data i in
              if b1 <> b2 then raise (Break_int (compare b1 b2))
            done;
            0
          with
            Break_int res -> res
        end
 *)

let equals t1 t2 =
	compare t1 t2 = 0

let count t =
  failwith "Not implemented"

(* Computed Constant: array of 256 positions of least set bit *)
let lsb_table = 
  let find_lsb b = (* find the least set bit in a byte *)
    let rec loop n = if b land (1 lsl n) <> 0 then n else loop (n+1) in
    if b = 0 then (-1) else loop 0
  in
    Array.init (1 lsl (1 lsl 8)) (fun i -> find_lsb i)

(* Find the first set bit in the bit array *)
let next_set_bit b n =
  failwith "Not implemented"
(*
  if n < 0 then invalid_arg "BitSet.next_set_bit";
  let buf = b.data in
  let rec find_set_bit byte_ndx =
    if byte_ndx >= b.len then None (* Not found *)
    else
      let byte = bget buf byte_ndx in
      if byte = 0 then (* keep looking next byte *)
	find_set_bit (byte_ndx + 1)
      else (* Done *)
	Some (lsb_table.(byte) + byte_ndx lsl log_int_size)
  in
  let byte_ndx = n lsr log_int_size in
  let bit_offs = n land int_size in
  (* get the current byte, but shift by bit_offs; dropping ignored bits *)
  let byte = (bget buf byte_ndx) lsr bit_offs in
  if byte = 0 then (* use aux function to search *)
    find_set_bit (byte_ndx + 1)
  else (* in current byte *)
    Some (lsb_table.(byte) + n)
 *)

let enum t =
  failwith "Not implemented"
(*
  let rec make n =
    let cur = ref n in
    let rec next () =
      match next_set_bit t !cur with
        Some elem ->
          cur := (elem+1);
          elem
      | None ->
          raise BatEnum.No_more_elements in
    BatEnum.make
      ~next
      ~count:(fun () -> partial_count t !cur)
      ~clone:(fun () -> make !cur)
  in
  make 0
 *)

let of_enum ?(cap=128) e = let bs = create cap in BatEnum.iter (set bs) e; bs

let of_list ?(cap=128) lst = let bs = create cap in List.iter (set bs) lst; bs

let inter a b =
  failwith "Not implemented"
(*
  let max_size = max a.len b.len in
  let d = raw_create max_size in
  let sl = min a.len b.len in
  let abuf = a.data
  and bbuf = b.data in
  (* Note: rest of the array is set to zero automatically *)
  for i = 0 to sl-1 do
    bset d.data i ((bget abuf i) land (bget bbuf i))
  done;
  d
 *)

(* Note: rest of the array is handled automatically correct, since we
   took a copy of the bigger set. *)
let union a b =
  failwith "Not implemented"
(*
  let d = if a.len > b.len then copy a else copy b in
  let sl = min a.len b.len in
  let abuf = a.data
  and bbuf = b.data in
  for i = 0 to sl-1 do
    bset d.data i ((bget abuf i) lor (bget bbuf i))
  done;
  d
 *)

let diff a b =
  failwith "Not implemented"
(*
  let maxlen = max a.len b.len in
  let buf = bcreate maxlen in
  bblit a.data 0 buf 0 a.len;
  let sl = min a.len b.len in
  let abuf = a.data
  and bbuf = b.data in
  for i = 0 to sl-1 do
    bset buf i ((bget abuf i) land (lnot (bget bbuf i)))
  done;
  for i = sl to a.len - 1 do (*If [b] is shorter than [a], assume that remaining bits of [b] are [0],
			       append the bits of [a] which haven't been copied yet*)
    bset buf i (bget abuf i)
  done;
  for i = sl to b.len - 1 do (*If [a] is shorter than [b], assume that remaining bits of [a] are [0],
			       append the negation of bits of [b] which haven't been copied yet*)
    bset buf i ((lnot (bget bbuf i)))
  done;
  { data = buf; len = maxlen }
 *)

let sym_diff a b =
  failwith "Not implemented"
(*
  let maxlen = max a.len b.len in
  let buf = bcreate maxlen in
  (* Copy larger (assumes missing bits are zero) *)
  bblit (if a.len > b.len then a.data else b.data) 0 buf 0 maxlen;
  let sl = min a.len b.len in
  let abuf = a.data
  and bbuf = b.data in
  for i = 0 to sl-1 do
    bset buf i ((bget abuf i) lxor (bget bbuf i))
  done;
  { data = buf; len = maxlen }
 *)

(* TODO the following set operations can be made faster if you do the
   set operation in-place instead of taking a copy.  But be careful
   when the sizes of the bitvector strings differ. *)
let intersect t t' =
  failwith "Not implemented"
(*
  let d = inter t t' in
  t.data <- d.data;
  t.len <- d.len
 *)

let differentiate t t' =
  failwith "Not implemented"
(*
  let d = diff t t' in
  t.data <- d.data;
  t.len <- d.len
 *)

let unite t t' =
  failwith "Not implemented"
(*
  let d = union t t' in
  t.data <- d.data;
  t.len <- d.len
 *)

let differentiate_sym t t' =
  failwith "Not implemented"
(*
  let d = sym_diff t t' in
  t.data <- d.data;
  t.len <- d.len
 *)

(*print a BitSet between [| and |]*)
(*let print ?(first="[|") ?(last="|]") ?(sep="") out t =
  let print_bit i =
    if is_set t i then BatInnerIO.write out '1'
    else               BatInnerIO.write out '0'
  in
  BatInnerIO.nwrite out first;
    if t.len = 0 then ()
    else begin
      print_bit 0;
      for i = 1 to t.len - 1 do
	BatInnerIO.nwrite out sep;
	print_bit i
      done
    end*)
let print out t =
  failwith "Not implemented"
(*
  let print_bit i =
    BatInnerIO.write out (if mem t i then '1' else '0')
  in
  for i = 0 to 8*t.len - 1 do
    print_bit i
  done
 *)
