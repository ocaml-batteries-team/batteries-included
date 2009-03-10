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
    IO.output_channel ~cleanup:true (open_out_bin name)
  let open_out_gen mode perm name = 
    IO.output_channel ~cleanup:true (open_out_gen mode perm name)

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
  let open_in_bin name  = IO.input_channel ~cleanup:true (open_in_bin name)
  let open_in_gen mode perm filename = 
    IO.input_channel ~cleanup:true (open_in_gen mode perm filename)

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

  (** {6 Directives} *)

(*
   type ('a, 'b) unum_directive = ?left_justify:bool -> ?pad_with_zeros:bool -> ?width:int -> ('a, 'b) Print.directive
   type ('a, 'b) snum_directive = ?prefix_with_plus:bool -> ?prefix_with_space:bool -> ('a, 'b) unum_directive
*)

  let pdir_a k f x = k (fun oc -> f oc x)
  let pdir_t k f = k (fun oc -> f oc)
  let pdir_B k x = k (fun oc -> IO.nwrite oc (string_of_bool x))
  let pdir_c k x = k (fun oc -> IO.write oc x)
  let pdir_C k x = k (fun oc ->
                        IO.write oc '\'';
                        IO.write oc x;
                        IO.write oc '\'')

  let pdir_s ?(left_justify=false) ?width k x =
    match width with
      | None ->
          k (fun oc -> IO.nwrite oc x)
      | Some n ->
          let len = String.length x in
          if len >= n then
            k (fun oc -> IO.nwrite oc x)
          else if left_justify then
            k (fun oc ->
                 IO.nwrite oc x;
                 for i = len to n do
                   IO.write oc ' '
                 done)
          else
            k (fun oc ->
                 IO.nwrite oc x;
                 for i = len to n do
                   IO.write oc ' '
                 done)

  let pdir_S ?left_justify ?width k x = pdir_s ?left_justify ?width k (Printf.sprintf "%S" x)

  (* TODO: make that more efficient: *)

  let pdir_d k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%d" x))
  let pdir_i k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%i" x))
  let pdir_u k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%u" x))
  let pdir_x k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%x" x))
  let pdir_X k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%X" x))
  let pdir_o k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%o" x))

  let pdir_ld k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%ld" x))
  let pdir_li k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%li" x))
  let pdir_lu k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%lu" x))
  let pdir_lx k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%lx" x))
  let pdir_lX k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%lX" x))
  let pdir_lo k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%lo" x))

  let pdir_Ld k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%Ld" x))
  let pdir_Li k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%Li" x))
  let pdir_Lu k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%Lu" x))
  let pdir_Lx k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%Lx" x))
  let pdir_LX k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%LX" x))
  let pdir_Lo k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%Lo" x))

  let pdir_nd k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%nd" x))
  let pdir_ni k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%ni" x))
  let pdir_nu k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%nu" x))
  let pdir_nx k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%nx" x))
  let pdir_nX k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%nX" x))
  let pdir_no k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%no" x))

  let pdir_f k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%f" x))
  let pdir_F k x = k (fun oc -> IO.nwrite oc (Printf.sprintf "%F" x))

  let pdir_format k fmt = fmt.Print.printer fmt.Print.pattern k

  let pdir_rope k x = k (fun oc -> Rope.print oc x)
  let pdir_utf8 k x = k (fun oc -> ExtUTF8.UTF8.print oc x)

  let pdir_obj k x = k x#print

  (** {6 Clean-up}*)

  let _ = at_exit close_all; (*Called second*)
          at_exit flush_all  (*Called first*)
end
