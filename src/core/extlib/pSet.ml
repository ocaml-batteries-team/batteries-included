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
TYPE_CONV_PATH "Batteries.Data.Persistent.PSet" (*For Sexplib, Bin-prot...*)

type 'a t = ('a, bool) PMap.t with sexp

let empty    = PMap.empty

let create   = PMap.create

let is_empty = PMap.is_empty

let mem      = PMap.mem

let add e t  = PMap.add e true t

let remove   = PMap.remove

let iter f   = PMap.iter (fun x _ -> f x)

let fold f   = PMap.foldi (fun k _ -> f k)

let exists f t = Labels.label (fun return ->
			       iter (fun k -> if f k then Labels.recall return true) t;
			       false)

let cardinal t =
  fold (fun _ acc -> acc + 1) t 0 

let enum t = 
  Enum.map (fun (k, _) -> k) (PMap.enum t)

let of_enum t =
  Enum.fold add empty t

let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_elt out t =
  Enum.print ~first ~last ~sep print_elt out (enum t)
