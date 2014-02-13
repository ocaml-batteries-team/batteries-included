(*
 * Hashcons -- a hashconsing library
 * Copyright (C) 2011  Batteries Included Development Team
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

(* Most of this code is lifted from J.-C. Fillâtre and S. Conchon's
   implementation:

   http://www.lri.fr/~filliatr/ftp/ocaml/ds/hashcons.ml
*)

module Pervasives = struct include Pervasives include BatPervasives end
module Int = BatInt
module Sys = BatSys
module Hashtbl = BatHashtbl
module Array = struct include Array include BatArray end

type 'a hobj = {
  obj   : 'a ;
  tag   : int ;
  hcode : int ;
}

type 'a t = 'a hobj

let compare ho1 ho2 = Int.compare ho1.tag ho2.tag

let gentag =
  let tags = ref 0 in
  fun () -> incr tags ; !tags

module type Table =
sig
  type key
  type t
  val create   : int -> t
  val clear    : t -> unit
  val hashcons : t -> key -> key hobj
  val iter     : (key hobj -> unit) -> t -> unit
  val fold     : (key hobj -> 'a -> 'a) -> t -> 'a -> 'a
  val count    : t -> int
end

module MakeTable (HT : Hashtbl.HashedType) : Table with type key = HT.t =
struct
  type key = HT.t

  type data = HT.t hobj

  type t = {
    mutable table : data Weak.t array ;
    mutable totsize : int ;             (* sum of the bucket sizes *)
    mutable limit : int ;               (* max ratio totsize/table length *)
  }

  let emptybucket = Weak.create 0

  let create sz =
    let sz = Pervasives.min (Pervasives.max sz 7) (Sys.max_array_length - 1) in
    { table = Array.make sz emptybucket
    ; totsize = 0 ; limit = 3 }

  let clear t =
    Array.modify (fun _ -> emptybucket) t.table ;
    t.totsize <- 0 ;
    t.limit <- 3

  let fold f t init =
    let rec fold_bucket i b accu =
      if i >= Weak.length b then accu else
        match Weak.get b i with
        | Some v -> fold_bucket (i + 1) b (f v accu)
        | None -> fold_bucket (i + 1) b accu
    in
    Array.fold_right (fold_bucket 0) t.table init

  let iter f t =
    let rec iter_bucket i b =
      if i >= Weak.length b then () else
        match Weak.get b i with
        | Some v -> f v ; iter_bucket (i + 1) b
        | None -> iter_bucket (i + 1) b
    in
    Array.iter (iter_bucket 0) t.table

  let count t =
    let rec count_bucket i b accu =
      if i >= Weak.length b then accu else
        count_bucket (i + 1) b (accu + (if Weak.check b i then 1 else 0))
    in
    Array.fold_right (count_bucket 0) t.table 0

  let next_sz n = Pervasives.min (3 * n / 2 + 3) (Sys.max_array_length - 1)

  let rec resize t =
    let oldlen = Array.length t.table in
    let newlen = next_sz oldlen in
    if newlen > oldlen then begin
      let newt = create newlen in
      newt.limit <- t.limit + 100 ;          (* prevent resizing of newt *)
      iter (add newt) t ;
      t.table <- newt.table ;
      t.limit <- t.limit + 2 ;
    end

  and add t d =
    let index = d.hcode mod (Array.length t.table) in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then begin
        let newsz = Pervasives.min (sz + 3) (Sys.max_array_length - 1) in
        if newsz <= sz then
          failwith "Hashcons.Make: hash bucket cannot grow more" ;
        let newbucket = Weak.create newsz in
        Weak.blit bucket 0 newbucket 0 sz ;
        Weak.set newbucket i (Some d) ;
        t.table.(index) <- newbucket ;
        t.totsize <- t.totsize + (newsz - sz) ;
        if t.totsize > t.limit * Array.length t.table then resize t ;
      end else begin
        if Weak.check bucket i
        then loop (i + 1)
        else Weak.set bucket i (Some d)
      end
    in
    loop 0

  let hashcons t d =
    let hcode = (HT.hash d) land Pervasives.max_int in
    let index = hcode mod (Array.length t.table) in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then begin
        let hdata = { hcode = hcode ; tag = gentag () ; obj = d } in
        add t hdata ;
        hdata
      end else begin
        match Weak.get_copy bucket i with
        | Some v when HT.equal v.obj d ->
          begin match Weak.get bucket i with
            | Some v -> v
            | None -> loop (i + 1)
          end
        | _ -> loop (i + 1)
      end
    in
    loop 0
end

module H = struct
  let hc0_ h = h
  let hc0 x = x.hcode
  let hc1_ x h = x + 19 * h
  let hc1  x = hc1_ x.hcode
end
