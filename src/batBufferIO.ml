(*
 * BatBufferIO - Circular byte buffer
 * Copyright (C) 2014 Simon Cruanes
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

(** Circular Byte Buffer for IO *)

type t = {
  mutable start : int;
  mutable stop : int; (* excluded *)
  mutable buf : string;
}

exception Empty

let create size =
  { start=0;
    stop=0;
    buf =String.make size ' ';
  }

let copy b =
  { b with buf=String.copy b.buf; }

let of_string s =
  { start=0;
    stop=String.length s;
    buf=String.copy s;
  }

let capacity b = String.length b.buf

let length b =
  if b.stop >= b.start
  then b.stop - b.start
  else (String.length b.buf - b.start) + b.stop

(* resize [b] so that inner capacity is [cap] *)
let resize b cap =
  assert (cap >= String.length b.buf);
  let buf' = String.make cap ' ' in
  (* copy into buf' *)
  let len =
    if b.stop >= b.start
    then begin
      String.blit b.buf b.start buf' 0 (b.stop - b.start);
      b.stop - b.start
    end else begin
      let len_end = String.length b.buf - b.start in
      String.blit b.buf b.start buf' 0 len_end;
      String.blit b.buf 0 buf' len_end b.stop;
      len_end + b.stop
    end
  in
  b.buf <- buf';
  b.start <- 0;
  b.stop <- len;
  ()

let blit_from b s o len =
  let cap = capacity b - length b in
  (* resize if needed, with a constant to amortize *)
  if cap < len then resize b (String.length b.buf + len + 24);
  assert (capacity b - length b >= len);
  if b.stop >= b.start
  then (*  [_______ start xxxxxxxxx stop ______] *)
    let len_end = String.length b.buf - b.stop in
    if len_end >= len
    then (String.blit s o b.buf b.stop len;
          b.stop <- b.stop + len)
    else (String.blit s o b.buf b.stop len_end;
          String.blit s (o+len_end) b.buf 0 (len-len_end);
          b.stop <- len-len_end)
  else begin (* [xxxxx stop ____________ start xxxxxx] *)
    let len_middle = b.start - b.stop in
    assert (len_middle >= len);
    String.blit s o b.buf b.stop len;
    b.stop <- b.stop + len
  end;
  ()

let blit_into b s o len =
  if o+len > String.length s
    then raise (Invalid_argument "BufferIO.blit_into");
  if b.stop >= b.start
  then
    let n = min (b.stop - b.start) len in
    let _ = String.blit b.buf b.start s o n in
    n
  else begin
    let len_end = String.length b.buf - b.start in
    String.blit b.buf b.start s o (min len_end len);
    if len_end >= len
    then len  (* done *)
    else begin
      let n = min b.stop (len - len_end) in
      String.blit b.buf 0 s (o+len_end) n;
      n + len_end
    end
  end

let add_string b s = blit_from b s 0 (String.length s)

(*$Q
  (Q.pair Q.printable_string Q.printable_string) (fun (s,s') -> \
    let b = create 24 in add_string b s; add_string b s'; \
    String.length s + String.length s' = length b)
*)

let to_string b =
  let s = String.make (length b) ' ' in
  let n = blit_into b s 0 (String.length s) in
  assert (n = String.length s);
  s

(*$Q
  (Q.pair Q.printable_string Q.printable_string) (fun (s,s') -> \
    let b = create 24 in add_string b s; add_string b s'; \
    to_string b = s ^ s')
*)

let clear b =
  b.stop <- 0;
  b.start <- 0;
  ()

let reset b =
  clear b;
  if capacity b > 64
    then b.buf <- String.make 64 ' ';  (* reset *)
  ()

let is_empty b = b.start = b.stop

let next b =
  if b.start = b.stop then raise Empty;
  b.buf.[b.start]

let pop b =
  if b.start = b.stop then raise Empty;
  let c = b.buf.[b.start] in
  if b.start + 1 = String.length b.buf
  then b.start <- 0
  else b.start <- b.start + 1;
  c

let junk b =
  if b.start = b.stop then raise Empty;
  if b.start + 1 = String.length b.buf
  then b.start <- 0
  else b.start <- b.start + 1

let skip b len =
  if len > length b then raise (Invalid_argument "BufferIO.skip");
  if b.stop >= b.start
  then b.start <- b.start + len
  else
    let len_end = String.length b.buf - b.start in
    if len > len_end
    then b.start <- len-len_end  (* wrap to the beginning *)
    else b.start <- b.start + len

(*$Q
  (Q.pair Q.printable_string Q.printable_string) (fun (s,s') -> \
    let b = create 24 in add_string b s; add_string b s'; \
    add_string b "hello world"; (* big enough *) \
    let l = length b in let l' = l/2 in skip b l'; \
    length b + l' = l)
*)

let iteri b f =
  if b.stop >= b.start
  then for i = b.start to b.stop - 1 do f i b.buf.[i] done
  else (
    for i = b.start to String.length b.buf -1 do f i b.buf.[i] done;
    for i = 0 to b.stop - 1 do f i b.buf.[i] done;
  )

(*$T
  let s = "hello world" in \
  let b = of_string s in \
  try iteri b (fun i c -> if s.[i] <> c then raise Exit); true with Exit -> false
*)

let get b i =
  if b.stop >= b.start
  then
    if i >= b.stop - b.start
    then raise (Invalid_argument "BufferIO.get")
    else b.buf.[b.start + i]
  else
    let len_end = String.length b.buf - b.start in
    if i < len_end
      then b.buf.[b.start + i]
    else if i - len_end > b.stop
      then raise (Invalid_argument "BufferIO.get")
      else b.buf.[i - len_end]


