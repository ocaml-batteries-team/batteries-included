(* 
 * ExtScanf - Extended Scanf module
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


  module Scanning =
  struct
    include Scanf.Scanning

    let from_input inp =
      from_function (fun () -> BatInnerIO.read inp)

    let from_channel = from_input

    let stdib = from_input (BatInnerIO.stdin)
  end

  type ('a, 'b, 'c, 'd) scanner =
      ('a, Scanning.scanbuf, 'b, 'c, 'a -> 'd, 'd) format6 -> 'c;;


  exception Scan_failure of string;;

  open Scanf
  let fscanf        = fscanf
  let sscanf        = sscanf
  let scanf         = scanf
  let kscanf        = kscanf
  let bscanf        = bscanf
  let bscanf_format = bscanf_format
  let sscanf_format = sscanf_format
  let format_from_string = format_from_string
