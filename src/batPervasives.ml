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


open Pervasives
  include BatStd
  open BatEnum

  (** {6 I/O}*)
  let print_guess oc v = BatIO.nwrite oc (dump v)
  let prerr_guess v = prerr_endline (dump v)

  let stdin             = BatIO.stdin
  let stdout            = BatIO.stdout
  let stderr            = BatIO.stderr
  let stdnull           = BatIO.stdnull

  let open_out          = BatFile.open_out
  let open_out_bin name = 
    BatIO.output_channel ~cleanup:true (open_out_bin name)
  let open_out_gen mode perm name = 
    BatIO.output_channel ~cleanup:true (open_out_gen mode perm name)

  let flush             = BatIO.flush
  let flush_all         = BatIO.flush_all
  let close_all         = BatIO.close_all
  
  let output_char       = BatChar.print
  let output_string     = BatString.print
  let output_rope       = BatRope.print
  let output oc buf pos len = 
    ignore (BatIO.output oc buf pos len)
  let output_byte       = BatIO.write_byte
  let output_binary_int = BatIO.write_i32
  let output_binary_float out v= BatIO.write_i64 out (BatInt64.bits_of_float v)
  let output_value out v= BatMarshal.output out v
  let close_out         = BatIO.close_out
  let close_out_noerr out = 
    try BatIO.close_out out
    with _ -> ()

  let open_in           = BatFile.open_in
  let open_in_bin name  = BatIO.input_channel ~cleanup:true (open_in_bin name)
  let open_in_gen mode perm filename = 
    BatIO.input_channel ~cleanup:true (open_in_gen mode perm filename)

  let input_char        = BatIO.read
  let input_line ic     = try BatIO.read_line ic with BatIO.No_more_input -> raise End_of_file
  let input             = BatIO.input
  let really_input inp buf pos len = 
    ignore (BatIO.really_input inp buf pos len)
  let input_byte        = BatIO.read_byte
  let input_binary_int  = BatIO.read_i32
  let input_binary_float inp= BatInt64.float_of_bits (BatIO.read_i64 inp)
  let close_in          = BatIO.close_in
  let close_in_noerr inp=
    try BatIO.close_in inp
    with _ -> ()
  let input_value       = BatMarshal.input

  let print_all inp     = BatIO.copy inp BatIO.stdout
  let prerr_all inp     = BatIO.copy inp BatIO.stderr

  include BatList.Infix

  (**{6 Importing BatEnum}*)

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
  let filter_map        = filter_map
  let concat            = concat
  let print             = print
  let get               = get
  let iter              = iter
  let scanl             = scanl
  include Infix

  (** {6 Operators}*)

  let first f (x, y) = (f x, y)
  let second f (x, y)= (x, f y)
  let ( **> )        = ( <| )
  let undefined ?(message="Undefined") = failwith message

  (** {6 String operations}*)

  let lowercase = String.lowercase
  let uppercase = String.uppercase

  (** {6 Directives} *)

  type printer_flags = {
    pf_width : int option;
    pf_frac_digits : int option;
    pf_padding_char : char;
    pf_justify : [ `right | `left ];
    pf_positive_prefix : char option;
  }

  let default_printer_flags = {
    pf_justify = `right;
    pf_width = None;
    pf_frac_digits = None;
    pf_padding_char = ' ';
    pf_positive_prefix = None;
  }

  let printer_a k f x = k (fun oc -> f oc x)
  let printer_t k f = k (fun oc -> f oc)
  let printer_B k x = k (fun oc -> BatIO.nwrite oc (string_of_bool x))
  let printer_c k x = k (fun oc -> BatIO.write oc x)
  let printer_C k x = k (fun oc ->
                           BatIO.write oc '\'';
                           BatIO.nwrite oc (Char.escaped x);
                           BatIO.write oc '\'')

  let printer_s ?(flags=default_printer_flags) k x =
    match flags.pf_width with
      | None ->
          k (fun oc -> BatIO.nwrite oc x)
      | Some n ->
          let len = String.length x in
          if len >= n then
            k (fun oc -> BatIO.nwrite oc x)
          else
            match flags.pf_justify with
              | `right ->
                  k (fun oc ->
                       for i = len + 1 to n do
                         BatIO.write oc flags.pf_padding_char
                       done;
                       BatIO.nwrite oc x)
              | `left ->
                  k (fun oc ->
                       BatIO.nwrite oc x;
                       for i = len + 1 to n do
                         BatIO.write oc flags.pf_padding_char
                       done)

  let printer_sc ?(flags=default_printer_flags) k x =
    match flags.pf_width with
      | None ->
          k (fun oc -> BatString.Cap.print oc x)
      | Some n ->
          let len = BatString.Cap.length x in
          if len >= n then
            k (fun oc -> BatString.Cap.print oc x)
          else
            match flags.pf_justify with
              | `right ->
                  k (fun oc ->
                       for i = len + 1 to n do
                         BatIO.write oc flags.pf_padding_char
                       done;
                       BatString.Cap.print oc x)
              | `left ->
                  k (fun oc ->
                       BatString.Cap.print oc x;
                       for i = len + 1 to n do
                         BatIO.write oc flags.pf_padding_char
                       done)

  let printer_S ?flags k x =
    printer_s ?flags k (BatString.quote x)

  let printer_Sc ?flags k x =
    printer_s ?flags k (BatString.Cap.quote x)


    

  open BatNumber

  let digits mk_digit base op n =
    let rec aux acc n =
      if op.compare n op.zero = 0 then
        acc
      else
        aux (mk_digit (op.to_int (op.modulo n base)) :: acc) (op.div n base)
    in
    if op.compare n op.zero = 0 then
      ['0']
    else
      aux [] n

  let printer_unum mk_digit base op ?flags k x =
    printer_s ?flags k (BatString.implode (digits mk_digit base op x))

  let printer_snum mk_digit base op ?(flags=default_printer_flags) k x =
    let l = digits mk_digit base op x in
    let l =
      if op.compare x op.zero < 0 then
        '-' :: l
      else
        match flags.pf_positive_prefix with
          | None ->
              l
          | Some c ->
              c :: l
    in
    printer_s ~flags k (BatString.implode l)

  let dec_digit x =
    char_of_int (int_of_char '0' + x)

  let oct_digit = dec_digit

  let lhex_digit x =
    if x < 10 then
      dec_digit x
    else
      char_of_int (int_of_char 'a' + x - 10)

  let uhex_digit x =
    if x < 10 then
      dec_digit x
    else
      char_of_int (int_of_char 'A' + x - 10)

  let printer_d ?flags k x = printer_snum dec_digit 10 BatInt.operations ?flags k x
  let printer_i ?flags k x = printer_snum dec_digit 10 BatInt.operations ?flags k x
  let printer_u ?flags k x = printer_unum dec_digit 10 BatInt.operations ?flags k x
  let printer_x ?flags k x = printer_unum lhex_digit 16 BatInt.operations ?flags k x
  let printer_X ?flags k x = printer_unum uhex_digit 16 BatInt.operations ?flags k x
  let printer_o ?flags k x = printer_unum oct_digit 8 BatInt.operations ?flags k x

  let printer_ld ?flags k x = printer_snum dec_digit 10l BatInt32.operations ?flags k x
  let printer_li ?flags k x = printer_snum dec_digit 10l BatInt32.operations ?flags k x
  let printer_lu ?flags k x = printer_unum dec_digit 10l BatInt32.operations ?flags k x
  let printer_lx ?flags k x = printer_unum lhex_digit 16l BatInt32.operations ?flags k x
  let printer_lX ?flags k x = printer_unum uhex_digit 16l BatInt32.operations ?flags k x
  let printer_lo ?flags k x = printer_unum oct_digit 8l BatInt32.operations ?flags k x

  let printer_Ld ?flags k x = printer_snum dec_digit 10L BatInt64.operations ?flags k x
  let printer_Li ?flags k x = printer_snum dec_digit 10L BatInt64.operations ?flags k x
  let printer_Lu ?flags k x = printer_unum dec_digit 10L BatInt64.operations ?flags k x
  let printer_Lx ?flags k x = printer_unum lhex_digit 16L BatInt64.operations ?flags k x
  let printer_LX ?flags k x = printer_unum uhex_digit 16L BatInt64.operations ?flags k x
  let printer_Lo ?flags k x = printer_unum oct_digit 8L BatInt64.operations ?flags k x

  let printer_nd ?flags k x = printer_snum dec_digit 10n BatNativeint.operations ?flags k x
  let printer_ni ?flags k x = printer_snum dec_digit 10n BatNativeint.operations ?flags k x
  let printer_nu ?flags k x = printer_unum dec_digit 10n BatNativeint.operations ?flags k x
  let printer_nx ?flags k x = printer_unum lhex_digit 16n BatNativeint.operations ?flags k x
  let printer_nX ?flags k x = printer_unum uhex_digit 16n BatNativeint.operations ?flags k x
  let printer_no ?flags k x = printer_unum oct_digit 8n BatNativeint.operations ?flags k x

  let printer_f ?flags k x =
    k (fun oc -> BatIO.nwrite oc
	 (match flags with
	   | None | Some {pf_width=None; pf_frac_digits=None} ->
	       Printf.sprintf "%f" x
	   | Some {pf_width=Some w; pf_frac_digits=None} ->
	       Printf.sprintf "%*f" w x
	   | Some {pf_width=None; pf_frac_digits=Some f} ->
	       Printf.sprintf "%.*f" f x
	   | Some {pf_width=Some w; pf_frac_digits=Some f} ->
	       Printf.sprintf "%*.*f" w f x
	 )
      )
  let printer_F ?flags k x =
    k (fun oc -> BatIO.nwrite oc
	 (match flags with
	   | None | Some {pf_width=None; pf_frac_digits=None} ->
	       Printf.sprintf "%F" x
	   | Some {pf_width=Some w; pf_frac_digits=None} ->
	       Printf.sprintf "%*F" w x
	   | Some {pf_width=None; pf_frac_digits=Some f} ->
	       Printf.sprintf "%.*F" f x
	   | Some {pf_width=Some w; pf_frac_digits=Some f} ->
	       Printf.sprintf "%*.*F" w f x
	 )
      )

  let printer_format k fmt = fmt.BatPrint.printer fmt.BatPrint.pattern k

  let printer_rope k x = k (fun oc -> BatRope.print oc x)
  let printer_utf8 k x = k (fun oc -> BatUTF8.print oc x)
  let printer_obj k x = k x#print
  let printer_exn k x = k (fun oc -> BatPrintexc.print oc x)

  let printer_int  = printer_i
  let printer_uint = printer_u
  let printer_hex  = printer_x
  let printer_HEX  = printer_X
  let printer_oct  = printer_o

  (** {6 Value printers} *)

  let bool_printer = BatBool.t_printer
  let int_printer = BatInt.t_printer
  let int32_printer = BatInt32.t_printer
  let int64_printer = BatInt64.t_printer
  let char_printer = BatChar.t_printer
  let nativeint_printer = BatNativeint.t_printer
  let float_printer = BatFloat.t_printer
  let string_printer = BatString.t_printer
  let list_printer = BatList.t_printer
  let array_printer = BatArray.t_printer
  let option_printer = BatOption.t_printer
  let maybe_printer = BatOption.maybe_printer
  let exn_printer paren out x =
    if paren then BatIO.write out '(';
    BatPrintexc.print out x;
    if paren then BatIO.write out ')'

  (** {6 Clean-up}*)

  let _ = at_exit close_all; (*Called second*)
          at_exit flush_all  (*Called first*)
