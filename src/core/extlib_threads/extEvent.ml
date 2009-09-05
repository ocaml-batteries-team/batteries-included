(*
 * ExtEvent - Additional functions for Events
 * Copyright (C) 1996 Xavier Leroy
 *               1996 David Nowak
 *               2009 David Rajchenbach-Teller
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

module Event = struct
  open Extlib

include Event

let enum chan =
  Enum.from(fun x -> sync(receive chan))

let of_enum e =
  let chan = new_channel () in
  let rec aux () =
    match Enum.get e with
      | Some x -> sync(send chan x); aux ()
      | None   -> ()
  in 
    ignore (Thread.create aux ());
    chan



end
