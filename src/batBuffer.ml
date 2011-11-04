(* 
 * ExtBuffer - Additional buffer operations
 * Copyright (C) 1999 Pierre Weis, Xavier Leroy
 *               2009 David Teller, LIFO, Universite d'Orleans
 *               2009 Dawid Toton
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


open BatString
open Buffer

  (** The underlying buffer type. *)
  type buffer = 
      {mutable buffer : string;(** Contents of the buffer *)
       mutable position : int; (** The end of the buffer  *)
       mutable length : int;   (** The size of the buffer *)
       initial_buffer : string (** For resetting to the original size **)}

  external buffer_of_t : t -> buffer = "%identity"
  external t_of_buffer : buffer -> t = "%identity"

  let print out t =
    BatString.print out (contents t)

  let enum t =
    let buf = buffer_of_t t in
      BatEnum.take buf.position (BatString.enum buf.buffer)

  let of_enum e =
    let length = BatEnum.count e  in
    let buf    = create length in
      add_string buf (BatString.of_enum e);
      buf

  let add_input t inp n =
    add_string t (BatInnerIO.really_nread inp n)

  let output_buffer = BatInnerIO.write_buf
    
  let add_channel = add_input

  let blit t srcoff dst dstoff len =
    let buf = buffer_of_t t in
      if srcoff < 0 || len < 0 || srcoff > buf.position - len
      then invalid_arg "Buffer.blit"
      else String.blit buf.buffer srcoff dst dstoff len

