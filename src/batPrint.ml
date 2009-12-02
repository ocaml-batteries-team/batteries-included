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

type ('a, 'b) directive = ((unit BatInnerIO.output -> unit) -> 'b) -> 'a
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
                    BatInnerIO.write oc '%';
                    aux (i + 2)

                | '!' ->
                    BatInnerIO.flush oc;
                    aux (i + 2)

                | _ ->
                    invalid_arg "Batteries.Print.format"
            end

        | ch ->
            BatInnerIO.write oc ch;
            aux (i + 1)
  in
  aux 0

let literal str k = k (fun oc -> BatInnerIO.nwrite oc str)

let kfprintf k oc fmt = fmt.printer fmt.pattern (fun f -> f (BatIO.cast_output oc); k oc)
let fprintf oc fmt = fmt.printer fmt.pattern (fun f -> f (BatIO.cast_output oc))

let ifprintf _ fmt = fprintf BatInnerIO.stdnull fmt

let printf fmt = fprintf BatInnerIO.stdout fmt
let eprintf fmt = fprintf BatInnerIO.stderr fmt

let bprintf buf fmt = fprintf (BatInnerIO.output_buffer buf) fmt
let kbprintf k buf fmt = kfprintf (fun _ -> k buf) (BatInnerIO.output_buffer buf) fmt

let sprintf fmt =
  let oc = BatInnerIO.output_buffer (Buffer.create 42) in
  kfprintf BatInnerIO.close_out oc fmt

let ksprintf k fmt =
  let oc = BatInnerIO.output_buffer (Buffer.create 42) in
  kfprintf (fun oc -> k (BatInnerIO.close_out oc)) oc fmt

let rprintf fmt =
  ksprintf BatRope.of_string fmt

let krprintf k fmt =
  ksprintf (fun s -> k (BatRope.of_string s)) fmt
