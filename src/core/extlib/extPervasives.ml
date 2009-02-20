(*
 * ExtPervasives - Additional functions
 * Copyright (C) 1996 Xavier Leroy
 *               2003 Nicolas Cannasse
 *               2007 Zheng Li
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
TYPE_CONV_PATH "Batteries" (*For Sexplib, Bin-prot...*)

module Pervasives = struct
  include Pervasives
  include Std
  open Enum

  (** {6 I/O}*)
  let print_guess   = Std.print
  let prerr_guess v = prerr_endline (dump v)

  let stdin             = IO.stdin
  let stdout            = IO.stdout
  let stderr            = IO.stderr
  let stdnull           = IO.stdnull

  let open_out          = File.open_out
  let open_out_bin name = 
    IO.output_channel (open_out_bin name)
  let open_out_gen mode perm name = 
    IO.output_channel (open_out_gen mode perm name)

  let flush             = IO.flush
  let flush_all         = IO.flush_all
  let close_all         = IO.close_all
  
  let output_char       = ExtChar.Char.print
  let output_string     = ExtString.String.print
  let output_rope       = Rope.print
  let output oc buf pos len = 
    ignore (IO.output oc buf pos len)
  let output_byte       = IO.write_byte
  let output_binary_int = IO.write_i32
  let output_value out v= ExtMarshal.Marshal.output out v
  let close_out         = IO.close_out
  let close_out_noerr out = 
    try IO.close_out out
    with _ -> ()

  let open_in           = File.open_in
  let open_in_bin name  = IO.input_channel (open_in_bin name)
  let open_in_gen mode perm filename = 
    IO.input_channel (open_in_gen mode perm filename)

  let input_char        = IO.read
  let input_line        = IO.read_line
  let input             = IO.input
  let really_input inp buf pos len = 
    ignore (IO.really_input inp buf pos len)
  let input_byte        = IO.read_byte
  let input_binary_int  = IO.read_i32
  let close_in          = IO.close_in
  let close_in_noerr inp=
    try IO.close_in inp
    with _ -> ()
  let input_value       = ExtMarshal.Marshal.input

  let print_all inp     = IO.copy inp IO.stdout
  let prerr_all inp     = IO.copy inp IO.stderr

  (**{6 Importing Enum}*)

  let foreach e f       = iter f e
  let exists            = exists
  let for_all           = for_all
  let fold              = fold
  let reduce            = reduce
  let find              = find
  let peek              = peek
  let push              = push
  let junk              = junk
  let map               = map
  let filter            = filter
  let concat            = concat
  let ( -- )            = ( -- )
  let ( --- )           = ( --- )
  let ( --~ )           = ( --~ )
  let ( // )            = ( // )
  let ( /@ ) e f        = map f e
  let ( @/ )            = map
  let print             = print
  let get               = get
  let iter              = iter
  let scanl             = scanl

  (** {6 Concurrency}*)

  let unique_value  = ref 0
  let lock          = ref Concurrent.nolock
  let unique ()     =
    Concurrent.sync !lock Ref.post_incr unique_value

  (** {6 Operators}*)

  let first f (x, y) = (f x, y)
  let second f (x, y)= (x, f y)
  let ( **> )        = ( <| )
  let undefined ?(message="Undefined") = failwith message

  (** {6 String operations}*)

  let lowercase = String.lowercase
  let uppercase = String.uppercase

  (** {6 Clean-up}*)

  let _ = at_exit close_all; (*Called second*)
          at_exit flush_all  (*Called first*)
end
