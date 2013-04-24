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

module S = BatSet.PSet
module M = BatMap.PMap

type ('a, 'b) t = {
  content : ('a, 'b S.t) M.t;
  keys    : 'a -> 'a -> int;
  data    : 'b -> 'b -> int
}


let empty =
  {content = M.empty;
   keys    = compare;
   data    = compare}

let is_empty t = M.is_empty t.content

let create keys data =
  {
    content = M.create keys;
    data    = data;
    keys    = keys
  }

let find k t =
  try M.find k t.content
  with Not_found -> S.create t.data

let add k d t =
  {(t) with content = M.add k (S.add d (find k t)) t.content}

let remove_all k t =
  {(t) with content = M.remove k t.content}

let remove k d t =
  try
    let set = S.remove d (M.find k t.content) in
    {(t)
     with content =
            if   S.is_empty set then M.remove k t.content
            else M.add k set t.content;
    }
  with Not_found -> t

let mem k d = M.mem k d.content

(* let exists = mem *)

let iter f d = M.iter f d.content

let map  (f:('b S.t -> 'c S.t)) (cmp:('b -> 'b -> int) -> ('c -> 'c -> int)) (t:('a, 'b) t) =
  { content = M.map f t.content;
    keys    = t.keys;
    data    = cmp t.data}

let mapi  (f:('a -> 'b S.t -> 'c S.t)) (cmp:('b -> 'b -> int) -> ('c -> 'c -> int)) (t:('a, 'b) t) =
  { content = M.mapi f t.content;
    keys    = t.keys;
    data    = cmp t.data}


let fold f d i  = M.fold f d.content i

let foldi f d i = M.foldi f d.content i

let modify k f t =
  {t with content = M.modify k f t.content}

let modify_def dft k f t =
  {t with content = M.modify_def dft k f t.content}

let modify_opt k f t =
  {t with content = M.modify_opt k f t.content}

let enum t =
  BatEnum.concat (BatEnum.map (fun (k,e) -> BatEnum.map (fun x -> (k,x)) (S.enum e)) (M.enum t.content))

let of_enum ?(keys=compare) ?(data=compare) e =
  let base = create keys data in
  BatEnum.fold (fun acc (k,d) -> add k d acc) base e

let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") ?(kvsep=": ") print_k print_v out t =
  BatEnum.print ~first ~last ~sep (fun out (k, v) -> BatPrintf.fprintf out "%a%s%a" print_k k kvsep print_v v) out (enum t)

module Infix =
struct
  let (-->) map key = find key map
  let (<--) map (key, value) = add key value map
end
