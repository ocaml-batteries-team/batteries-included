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


open Stack

  type 'a enumerable = 'a Stack.t

  let of_enum e =
    let s = create () in
      BatEnum.iter (fun x -> push x s) e;
      s

  let enum s = BatEnum.from
    (fun () -> try pop s with Stack.Empty -> raise BatEnum.No_more_elements)

  let print ?(first="") ?(last="") ?(sep="") print_a out t =
      BatEnum.print ~first ~last ~sep print_a out (enum (copy t))

  module Exceptionless = struct
    let top s = try Some (top s) with Stack.Empty -> None
    let pop s = try Some (pop s) with Stack.Empty -> None
  end
