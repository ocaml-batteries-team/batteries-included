(*
 * BatPervasives - Additional functions
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

include BatInnerPervasives

let invisible_args = ref 1
(* the number or arguments to ignore at the beginning of Sys.argv,
   usually because program-name is put in argv.(0) *)

let args () =
  let e = BatArray.enum Sys.argv in
  BatEnum.drop !invisible_args e;
  e

let exe = Array.get Sys.argv 0

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

let undefined ?(message="Undefined") _ = failwith message
(*$T undefined
   ignore (Obj.magic (undefined ~message:"")); true
   try ignore (undefined ~message:"FooBar" ()); false with Failure "FooBar" -> true
*)

let verify x ex = if x then () else raise ex
let verify_arg x s = if x then () else invalid_arg s

(** {6 Clean-up}*)

let _ = at_exit close_all; (*Called second*)
  at_exit flush_all  (*Called first*)
