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

type 'a hobj = {
  obj : 'a ;
  tag  : int ;
  key : int ;
}

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
end

module MakeTable (HT : BatHashtbl.HashedType) : Table with type key = HT.t =
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
    let sz = if sz < 7 then 7 else sz in
    let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz
    in {
      table = Array.make sz emptybucket ;
      totsize = 0 ;
      limit = 3 ;
    }

  let clear t =
    for i = 0 to Array.length t.table - 1 do
      t.table.(i) <- emptybucket
    done ;
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

  let next_sz n = min (3*n/2 + 3) (Sys.max_array_length - 1)

  let rec resize t =
    let oldlen = Array.length t.table in
    let newlen = next_sz oldlen in
    if newlen > oldlen then begin
      let newt = create newlen in
      newt.limit <- t.limit + 100 ;          (* prevent resizing of newt *)
      fold (fun d () -> add newt d) t () ;
      t.table <- newt.table ;
      t.limit <- t.limit + 2 ;
    end

  and add t d =
    let index = d.key mod (Array.length t.table) in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then begin
        let newsz = min (sz + 3) (Sys.max_array_length - 1) in
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
    let key = HT.hash d in
    let index = key mod (Array.length t.table) in
    let bucket = t.table.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then begin
	let hdata = { key = key ; tag = gentag () ; obj = d } in
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
  let hc0 x = x.key
  let hc1_ x h = x + 19 * h
  let hc1  x = hc1_ x.key
end
