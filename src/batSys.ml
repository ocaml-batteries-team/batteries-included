(*
 * BatSys - additional and modified functions for System
 * Copyright (C) 1996 Xavier Leroy
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
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

let big_endian = false  (* overridden by real big_endian value in 4.00 and above *)

include Sys

let files_of d = BatArray.enum (readdir d)

##V<4.3##let sigbus = -22
##V<4.3##let sigpoll = -23
##V<4.3##let sigsys = -24
##V<4.3##let sigtrap = -25
##V<4.3##let sigurg = -26
##V<4.3##let sigxcpu = -27
##V<4.3##let sigxfsz = -28

##V>=4.3##external opaque_identity : 'a -> 'a = "%opaque"
##V<4.3##let opaque_identity = BatOpaqueInnerSys.opaque_identity

##V<4.5##let getenv_opt v = try Some (getenv v) with Not_found -> None
