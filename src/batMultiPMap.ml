(*
 * MultiPMap - Polymorphic maps with multiple associations
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 * Copyright (C) 2008      David Teller, LIFO, Universite d'Orleans
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


type ('a, 'b) t = {
  content : ('a, 'b BatPSet.t) BatPMap.t;
  keys    : 'a -> 'a -> int     ;
  data    : 'b -> 'b -> int
}


let empty =
  {content = BatPMap.empty;
   keys    = compare;
   data    = compare}

let is_empty t = BatPMap.is_empty t.content

let create keys data = 
  {
    content = BatPMap.create keys;
    data    = data;
    keys    = keys
  }

let find k t =
  try BatPMap.find k t.content
  with Not_found -> BatPSet.create t.data

let add k d t =
  {(t) with content = BatPMap.add k (BatPSet.add d (find k t)) t.content}
  
let remove_all k t =
  {(t) with content = BatPMap.remove k t.content}

let remove k d t =
  try 
    let set = BatPSet.remove d (BatPMap.find k t.content) in
      {(t)
       with content =
	  if   BatPSet.is_empty set then BatPMap.remove k t.content
	  else BatPMap.add k set t.content;
      }
  with Not_found -> t

let mem k d =
  BatPMap.mem k d.content

let exists = mem

let iter f d = BatPMap.iter f d.content

let map  (f:('b BatPSet.t -> 'c BatPSet.t)) (cmp:('b -> 'b -> int) -> ('c -> 'c -> int)) (t:('a, 'b) t) =
  { content = BatPMap.map f t.content;
    keys    = t.keys;
    data    = cmp t.data}

let mapi  (f:('a -> 'b BatPSet.t -> 'c BatPSet.t)) (cmp:('b -> 'b -> int) -> ('c -> 'c -> int)) (t:('a, 'b) t) =
  { content = BatPMap.mapi f t.content;
    keys    = t.keys;
    data    = cmp t.data}


let fold f d i  = BatPMap.fold f d.content i

let foldi f d i = BatPMap.foldi f d.content i

let enum t      = BatEnum.concat (BatEnum.map (fun (k,e) -> BatEnum.map (fun x -> (k,x)) (BatPSet.enum e)) (BatPMap.enum t.content))

let of_enum ?(keys=compare) ?(data=compare) e = 
  let base = create keys data in
    BatEnum.fold (fun acc (k,d) -> add k d acc) base e

let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_k print_v out t =
  BatEnum.print ~first ~last ~sep (fun out (k, v) -> BatPrintf.fprintf out "%a: %a" print_k k print_v v) out (enum t)

module Infix =
struct
  let (-->) map key = find key map
  let (<--) map (key, value) = add key value map
end

