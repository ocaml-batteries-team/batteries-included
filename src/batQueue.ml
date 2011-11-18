(* 
 * ExtQueue - Extended operations on queues
 * Copyright (C) 1996 Xavier Leroy
 *               2008 David Teller, LIFO, Universite d'Orleans
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


open Queue

  type 'a enumerable = 'a t

  let of_enum e =
    let q = create () in
      BatEnum.iter (fun x -> push x q) e;
      q

  let enum q = BatEnum.from (fun () -> try pop q with Empty -> raise BatEnum.No_more_elements)


  let print ?(first="") ?(last="") ?(sep="") print_a out t =
      BatEnum.print ~first ~last ~sep print_a out (enum (copy t))

  module Exceptionless = struct
    let peek q = try Some (peek q) with Empty -> None
    let take q = try Some (take q) with Empty -> None
  end
