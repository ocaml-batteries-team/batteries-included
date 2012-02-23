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
open BatEnum

let input_lines ch =
  BatEnum.from (fun () ->
    try input_line ch with End_of_file -> raise BatEnum.No_more_elements)

let input_chars ch =
  BatEnum.from (fun () ->
    try input_char ch with End_of_file -> raise BatEnum.No_more_elements)

type 'a _mut_list = {
  hd : 'a;
  mutable tl : 'a _mut_list;
}

let input_list ch =
  let _empty = Obj.magic [] in
  let rec loop dst =
    let r = { hd = input_line ch; tl = _empty } in
    dst.tl <- r;
    loop r in
  let r = { hd = Obj.magic(); tl = _empty } in
  try loop r
  with
    End_of_file ->
      Obj.magic r.tl

let buf_len = 8192

let input_all ic =
  let rec loop acc total buf ofs =
    let n = input ic buf ofs (buf_len - ofs) in
    if n = 0 then
      let res = String.create total in
      let pos = total - ofs in
      let _ = String.blit buf 0 res pos ofs in
      let coll pos buf =
        let new_pos = pos - buf_len in
        String.blit buf 0 res new_pos buf_len;
        new_pos in
      let _ = List.fold_left coll pos acc in
      res
    else
      let new_ofs = ofs + n in
      let new_total = total + n in
      if new_ofs = buf_len then
        loop (buf :: acc) new_total (String.create buf_len) 0
      else loop acc new_total buf new_ofs in
  loop [] 0 (String.create buf_len) 0

let input_file ?(bin=false) fname =
  let ch = (if bin then open_in_bin else open_in) fname in
  let str = input_all ch in
  close_in ch;
  str

let output_file ~filename ~text =
  let ch = open_out filename in
  output_string ch text;
  close_out ch

let print_bool = function
  | true -> print_string "true"
  | false -> print_string "false"

let prerr_bool = function
  | true -> prerr_string "true"
  | false -> prerr_string "false"

let string_of_char c = String.make 1 c

external identity : 'a -> 'a = "%identity"

let rec dump r =
  if Obj.is_int r then
    string_of_int (Obj.magic r : int)
  else (* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (Obj.field r n :: acc) n
    in
    let rec is_list r =
      if Obj.is_int r then
	r = Obj.repr 0 (* [] *)
      else
	let s = Obj.size r and t = Obj.tag r in
	t = 0 && s = 2 && is_list (Obj.field r 1) (* h :: t *)
    in
    let rec get_list r =
      if Obj.is_int r then
	[]
      else
	let h = Obj.field r 0 and t = get_list (Obj.field r 1) in
	h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible
       * in pure OCaml at the moment.  *)
      "<" ^ name ^ ">"
    in
    let s = Obj.size r and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
    match t with
      | _ when is_list r ->
	let fields = get_list r in
	"[" ^ String.concat "; " (List.map dump fields) ^ "]"
      | 0 ->
	let fields = get_fields [] s in
	"(" ^ String.concat ", " (List.map dump fields) ^ ")"
      | x when x = Obj.lazy_tag ->
	(* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
	 * clear if very large constructed values could have the same
	 * tag. XXX *)
	opaque "lazy"
      | x when x = Obj.closure_tag ->
	opaque "closure"
      | x when x = Obj.object_tag ->
	let fields = get_fields [] s in
	let _clasz, id, slots =
	  match fields with
	    | h::h'::t -> h, h', t
	    | _ -> assert false
	in
	(* No information on decoding the class (first field).  So just print
	 * out the ID and the slots. *)
	"Object #" ^ dump id ^ " (" ^ String.concat ", " (List.map dump slots) ^ ")"
      | x when x = Obj.infix_tag ->
	opaque "infix"
      | x when x = Obj.forward_tag ->
	opaque "forward"
      | x when x < Obj.no_scan_tag ->
	let fields = get_fields [] s in
	"Tag" ^ string_of_int t ^
	  " (" ^ String.concat ", " (List.map dump fields) ^ ")"
      | x when x = Obj.string_tag ->
	"\"" ^ String.escaped (Obj.magic r : string) ^ "\""
      | x when x = Obj.double_tag ->
	string_of_float (Obj.magic r : float)
      | x when x = Obj.abstract_tag ->
	opaque "abstract"
      | x when x = Obj.custom_tag ->
	opaque "custom"
      | x when x = Obj.final_tag ->
	opaque "final"
      | x when x = Obj.double_array_tag ->
	BatIO.to_string (BatArray.print BatFloat.print) (Obj.magic r : float array)
      | _ ->
	opaque (Printf.sprintf "unknown: tag %d size %d" t s)

let dump v = dump (Obj.repr v)

let print_any oc v = BatIO.nwrite oc (dump v)

let finally = BatFile.finally

let with_dispose ~dispose f x =
  finally (fun () -> dispose x) f x

let forever f x = ignore (while true do f x done)

let ignore_exceptions f x = try ignore (f x) with _ -> ()

(* unique int generation from batPervasives *)
let unique_value  = ref 0
let lock          = ref BatConcurrent.nolock
let unique ()     =
  BatConcurrent.sync !lock BatRef.post_incr unique_value

(*$Q unique
   Q.unit (fun () -> unique () <> unique ())
 *)

type ('a, 'b) result =
  | Ok  of 'a
  | Bad of 'b

(* Ideas taken from Nicholas Pouillard's my_std.ml in ocamlbuild/ *)
let ignore_ok = function
    Ok _ -> ()
  | Bad ex -> raise ex

let ok = function
    Ok v -> v
  | Bad ex -> raise ex

let wrap f x = try Ok (f x) with ex -> Bad ex

(** {6 Operators}*)

let ( |> ) x f = f x

let ( <| ) f x = f x

let ( |- ) f g x = g (f x)

let ( -| ) f g x = f (g x)

let flip f x y = f y x

let ( *** ) f g = fun (x,y) -> (f x, g y)

let ( &&& ) f g = fun x -> (f x, g x)

let curry f x y = f (x,y)

let uncurry f (x,y) = f x y

let const x _ = x

let tap f x = f x; x

let invisible_args = ref 1
(* the number or arguments to ignore at the beginning of Sys.argv,
usually because program-name is put in argv.(0) *)

let args () =
  let e = BatArray.enum Sys.argv in
    BatEnum.drop !invisible_args e;
    e

let exe =
  Array.get Sys.argv 0

let argv = Sys.argv


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
  let output_text       = Ulib.Text.print
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

  let ( **> )        = ( <| )
  let undefined ?(message="Undefined") = failwith message

  let verify x ex = if x then () else raise ex
  let verify_arg x s = if x then () else invalid_arg s

  let ( |? ) = BatOption.Infix.( |? )

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

  let printer_text k x = k (fun oc -> Ulib.Text.print oc x)
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
