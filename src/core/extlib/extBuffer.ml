(* 
 * ExtBuffer - Additional buffer operations
 * Copyright (C) 1999 Pierre Weis, Xavier Leroy
 *               2008 David Teller
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
TYPE_CONV_PATH "Batteries.Data.Text" (*For Sexplib, Bin-prot...*)

open ExtString

module Buffer =
struct

  include Buffer

  (**The underlying buffer type.*)
  type buffer = 
      {mutable buffer : string;(**Contents of the buffer*)
       mutable position : int; (**The end of the buffer *)
       mutable length : int;   (**The size of the buffer*)
       initial_buffer : string (**?**)}

  external buffer_of_t : t -> buffer = "%identity"
  external t_of_buffer : buffer -> t = "%identity"

  let t_of_sexp sexp = 
    let s   = Conv.string_of_sexp sexp in 
    let buf = create (String.length s) in
      add_string buf s;
      buf

  let sexp_of_t t = Conv.sexp_of_string (Buffer.contents t)

  let print out t =
    ExtString.String.print out (contents t)

    
  let enum t =
    let buf = buffer_of_t t in
      Enum.take buf.position (String.enum buf.buffer)

  let of_enum e =
    let length = Enum.count e  in
    let buf    = create length in
      add_string buf (ExtString.String.of_enum e);
      buf

  let add_input t inp n =
    add_string t (InnerIO.really_nread inp n)

  let output_buffer = InnerIO.write_buf
    
  let add_channel = add_input
end
