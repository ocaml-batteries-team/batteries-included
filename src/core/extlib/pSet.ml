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

open Sexplib
open Conv
TYPE_CONV_PATH "PSet" (*For Sexplib, Bin-prot...*)

type 'a t = ('a, unit) PMap.t with sexp

type 'a enumerable = 'a t
type 'a mappable = 'a t

let empty    = PMap.empty

let create   = PMap.create

let is_empty = PMap.is_empty

let mem      = PMap.mem

let add e t  = PMap.add e () t

let remove   = PMap.remove

let for_map f = fun x _ -> f x

let iter f   = PMap.iter (for_map f)

let fold f   = PMap.foldi (for_map f)

let map f e  = PMap.foldi (fun k _ acc -> add (f k) acc) e empty

let filter f = PMap.filteri (for_map f)

let filter_map f e = PMap.foldi (fun k _ acc -> match f k with None -> acc | Some v -> add v acc) e empty

let exists f t = Return.label (fun label ->
			       iter (fun k -> if f k then Return.return label true) t;
			       false)

let cardinal t =
  fold (fun _ acc -> acc + 1) t 0 

let enum t = 
  Enum.map (fun (k, _) -> k) (PMap.enum t)

let of_enum t =
  Enum.fold add empty t

let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_elt out t =
  Enum.print ~first ~last ~sep print_elt out (enum t)
