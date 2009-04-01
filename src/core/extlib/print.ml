(*
 * Print - Functional unparsing
 * Copyright (C) 2009 Jeremie Dimino
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

type ('a, 'b) directive = ((unit InnerIO.output -> unit) -> 'b) -> 'a
type pattern = string
type ('a, 'b) format = {
  pattern : pattern;
  printer : pattern -> ('a, 'b) directive;
}

(* Parse an integer followed by a [')'] in [pattern] *)
let parse_key pattern i =
  let rec aux acc i =
    if i = String.length pattern then
      invalid_arg "Batteries.Print.format"
    else
      match pattern.[i] with
        | ')' ->
            (acc, i + 1)
        | '0' .. '9' as ch ->
            aux (acc * 10 + (Char.code ch - Char.code '0')) (i + 1)
        | _ ->
            invalid_arg "Batteries.Print.format"
  in
  if i + 1 >= String.length pattern then
    invalid_arg "Batteries.Print.format"
  else
    match pattern.[i] with
      | '0' .. '9' as ch ->
          aux (Char.code ch - Char.code '0') (i + 1)
      | _ ->
          invalid_arg "Batteries.Print.format"

let format oc pattern directives =
  let rec aux i =
    if i = String.length pattern then
      ()
    else
      match pattern.[i] with
        | '%' ->
            if i + 1 >= String.length pattern then
              invalid_arg "Batteries.Print.format"
            else begin
              match pattern.[i + 1] with
                | '(' ->
                    let key, i = parse_key pattern (i + 2) in
                    if key < 0 || key > Array.length directives then
                      invalid_arg "Batteries.Print.format"
                    else begin
                      directives.(key) oc;
                      aux i
                    end

                | '%' ->
                    InnerIO.write oc '%';
                    aux (i + 2)

                | '!' ->
                    InnerIO.flush oc;
                    aux (i + 2)

                | _ ->
                    invalid_arg "Batteries.Print.format"
            end

        | ch ->
            InnerIO.write oc ch;
            aux (i + 1)
  in
  aux 0

let literal str k = k (fun oc -> InnerIO.nwrite oc str)

let kfprintf k oc fmt = fmt.printer fmt.pattern (fun f -> f (IO.cast_output oc); k oc)
let fprintf oc fmt = fmt.printer fmt.pattern (fun f -> f (IO.cast_output oc))

let ifprintf _ fmt = fprintf InnerIO.stdnull fmt

let printf fmt = fprintf InnerIO.stdout fmt
let eprintf fmt = fprintf InnerIO.stderr fmt

let bprintf buf fmt = fprintf (InnerIO.output_buffer buf) fmt
let kbprintf k buf fmt = kfprintf (fun _ -> k buf) (InnerIO.output_buffer buf) fmt

let sprintf fmt =
  let oc = InnerIO.output_buffer (Buffer.create 42) in
  kfprintf InnerIO.close_out oc fmt

let ksprintf k fmt =
  let oc = InnerIO.output_buffer (Buffer.create 42) in
  kfprintf (fun oc -> k (InnerIO.close_out oc)) oc fmt

let rprintf fmt =
  ksprintf Rope.of_string fmt

let krprintf k fmt =
  ksprintf (fun s -> k (Rope.of_string s)) fmt
