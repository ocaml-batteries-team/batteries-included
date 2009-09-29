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
  content : ('a, 'b PSet.t) PMap.t;
  keys    : 'a -> 'a -> int     ;
  data    : 'b -> 'b -> int
}


let empty =
  {content = PMap.empty;
   keys    = compare;
   data    = compare}

let is_empty t = PMap.is_empty t.content

let create keys data = 
  {
    content = PMap.create keys;
    data    = data;
    keys    = keys
  }

let find k t =
  try PMap.find k t.content
  with Not_found -> PSet.create t.data

let add k d t =
  {(t) with content = PMap.add k (PSet.add d (find k t)) t.content}
  
let remove_all k t =
  {(t) with content = PMap.remove k t.content}

let remove k d t =
  try 
    let set = PSet.remove d (PMap.find k t.content) in
      {(t)
       with content =
	  if   PSet.is_empty set then PMap.remove k t.content
	  else PMap.add k set t.content;
      }
  with Not_found -> t

let mem k d =
  PMap.mem k d.content

let exists = mem

let iter f d = PMap.iter f d.content

let map  (f:('b PSet.t -> 'c PSet.t)) (cmp:('b -> 'b -> int) -> ('c -> 'c -> int)) (t:('a, 'b) t) =
  { content = PMap.map f t.content;
    keys    = t.keys;
    data    = cmp t.data}

let mapi  (f:('a -> 'b PSet.t -> 'c PSet.t)) (cmp:('b -> 'b -> int) -> ('c -> 'c -> int)) (t:('a, 'b) t) =
  { content = PMap.mapi f t.content;
    keys    = t.keys;
    data    = cmp t.data}


let fold f d i  = PMap.fold f d.content i

let foldi f d i = PMap.foldi f d.content i

let enum t      = Enum.concat (Enum.map (fun (k,e) -> Enum.map (fun x -> (k,x)) (PSet.enum e)) (PMap.enum t.content))

let of_enum ?(keys=compare) ?(data=compare) e = 
  let base = create keys data in
    Enum.fold (fun acc (k,d) -> add k d acc) base e

let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_k print_v out t =
  Enum.print ~first ~last ~sep (fun out (k, v) -> ExtPrintf.Printf.fprintf out "%a: %a" print_k k print_v v) out (enum t)
