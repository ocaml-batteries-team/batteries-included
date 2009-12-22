(*
 * PMap - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 * Copyright (C)      2008 David Teller
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


type 'a t = ('a, unit) BatPMap.t

type 'a enumerable = 'a t
type 'a mappable = 'a t

let empty    = BatPMap.empty

let create   = BatPMap.create

let is_empty = BatPMap.is_empty

let mem      = BatPMap.mem

let add e t  = BatPMap.add e () t

let remove   = BatPMap.remove

let for_map f = fun x _ -> f x

let iter f   = BatPMap.iter (for_map f)

let fold f   = BatPMap.foldi (for_map f)

let map f e  = BatPMap.foldi (fun k _ acc -> add (f k) acc) e empty

let filter f = BatPMap.filteri (for_map f)

let filter_map f e = BatPMap.foldi (fun k _ acc -> match f k with None -> acc | Some v -> add v acc) e empty

let exists f t = BatReturn.label (fun label ->
			       iter (fun k -> if f k then BatReturn.return label true) t;
			       false)

let cardinal t =
  fold (fun _ acc -> acc + 1) t 0 

let choose t = fst (BatPMap.choose t)

let min_elt t = fst (BatPMap.min_binding t)

let max_elt t = fst (BatPMap.max_binding t)

let enum t = 
  BatEnum.map (fun (k, _) -> k) (BatPMap.enum t)

let of_enum t =
  BatEnum.fold (fun acc elem -> add elem acc) empty t

let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_elt out t =
  BatEnum.print ~first ~last ~sep print_elt out (enum t)

let for_all f t = BatPMap.for_all (fun k _ -> f k) t

let partition f t = BatPMap.partition (fun k _ -> f k) t

let filter f t = BatPMap.filteri (fun k _ -> f k) t

let pop t = let (k, _), m = BatPMap.pop t in k, m
