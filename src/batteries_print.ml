(* 
 * Batteries_print - Pretty-printers for the toplevel
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

let print_uchar fmt t =
  Format.fprintf fmt "UChar.of_char '%s'" (BatUTF8.to_string (BatUTF8.of_char t))

let print_rope fmt t =
  Format.fprintf fmt "r%S" (BatRope.to_string t)

let print_ustring fmt t =
  Format.fprintf fmt "u%S" (BatUTF8.to_string t)

let string_of_cap t = BatString.Cap.to_string (BatString.Cap.copy t)

let print_string_cap_rw fmt t =
  Format.fprintf fmt "rw%S" (string_of_cap t)

let print_string_cap_ro fmt t =
  Format.fprintf fmt "ro%S" (string_of_cap t)

