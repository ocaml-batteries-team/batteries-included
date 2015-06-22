# 1 "src/batFormat.mlv"
(*
 * BatFormat - Extended Format module
 * Copyright (C) 1996 Pierre Weis
 *               2009 David Teller, LIFO, Universite d'Orleans
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


open BatIO
include Format

(* internal functions *)

let output_of out = fun s i o -> ignore (really_output out s i o)
let flush_of out = BatInnerIO.get_flush out
let newline_of out = fun () -> BatInnerIO.write out '\n'
let spaces_of out =
  (* Default function to output spaces.
     Copied from base format.ml*)
  let blank_line = String.make 80 ' ' in
  let rec display_blanks n =
    if n > 0 then
      if n <= 80 then ignore (really_output out blank_line 0 n) else
        begin
          ignore (really_output out blank_line 0 80);
          display_blanks (n - 80)
        end
  in display_blanks

(**{6 New functions}*)

let formatter_of_output out =
  let output = output_of out
  and flush  = flush_of  out
  in
  let f = make_formatter output flush in
  BatInnerIO.on_close_out out (fun _ -> pp_print_flush f ()); (*Note: we can't just use [flush] as [f] contains a cache.*)
  pp_set_all_formatter_output_functions f
    ~out:output
    ~flush
    ~newline:(newline_of out)
    ~spaces:(spaces_of out);
  f

let set_formatter_output out =
  BatInnerIO.on_close_out out (fun _ -> pp_print_flush Format.std_formatter ());
  set_all_formatter_output_functions
    ~out:(output_of out)
    ~flush:(flush_of out)
    ~newline:(newline_of out)
    ~spaces:(spaces_of out)

let pp_set_formatter_output f out =
  BatInnerIO.on_close_out out (fun _ -> pp_print_flush f ());
  pp_set_all_formatter_output_functions f
    ~out:(output_of out)
    ~flush:(flush_of out)
    ~newline:(newline_of out)
    ~spaces:(spaces_of out)

(**{6 Old values, new semantics}*)

let formatter_of_out_channel = formatter_of_output
let set_formatter_out_channel = set_formatter_output
let pp_set_formatter_out_channel = pp_set_formatter_output
let std_formatter = formatter_of_output BatIO.stdout
let err_formatter = formatter_of_output BatIO.stderr

(* Backward compatibility *)

(* To format a list *)
let rec pp_print_list ?(pp_sep = pp_print_cut) pp_v ppf = function
  | [] -> ()
  | [v] -> pp_v ppf v
  | v :: vs ->
    pp_v ppf v;
    pp_sep ppf ();
    pp_print_list ~pp_sep pp_v ppf vs

(* To format free-flowing text *)
let pp_print_text ppf s =
  let len = String.length s in
  let left = ref 0 in
  let right = ref 0 in
  let flush () =
    pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    match s.[!right] with
      | '\n' ->
        flush ();
        pp_force_newline ppf ()
      | ' ' ->
        flush (); pp_print_space ppf ()
      (* there is no specific support for '\t'
         as it is unclear what a right semantics would be *)
      | _ -> incr right
  done;
  if !left <> len then flush ()
# 115 "src/batFormat.mlv"


(**{6 Initialization}*)

let () =
  set_formatter_output BatIO.stdout;
  pp_set_formatter_output Format.std_formatter stdout;
  pp_set_formatter_output Format.err_formatter stderr
