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

let files_of d = BatArray.gen (readdir d)

let invisible_args = ref 1
(* the number or arguments to ignore at the beginning of Sys.argv,
   usually because program-name is put in argv.(0) *)

let args () =
  let g = BatArray.gen Sys.argv in
  BatGen.drop !invisible_args g
