(*
 * Print - Functionnal unparsing
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

type ('a, 'b, 'acc) directive = (('acc IO.output -> unit) -> 'b) -> 'a
type pattern = string
type ('a, 'b, 'acc) format = {
  pattern : pattern;
  printer : pattern -> ('a, 'b, 'acc) directive;
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
                    IO.write oc '%';
                    aux (i + 2)

                | '!' ->
                    IO.flush oc;
                    aux (i + 2)

                | _ ->
                    invalid_arg "Batteries.Print.format"
            end

        | ch ->
            IO.write oc ch;
            aux (i + 1)
  in
  aux 0

let literal str k = k (fun oc -> IO.nwrite oc str)

let kfprintf k oc fmt = fmt.printer fmt.pattern (fun f -> f oc; k oc)
let fprintf oc fmt = fmt.printer fmt.pattern (fun f -> f oc)

let printf fmt = fprintf IO.stdout fmt
let eprintf fmt = fprintf IO.stderr fmt

let bprintf buf fmt = fprintf (IO.output_buffer buf) fmt
let kbprintf k buf fmt = kfprintf (fun _ -> k buf) (IO.output_buffer buf) fmt

let sprintf fmt =
  let oc = IO.output_buffer (Buffer.create 42) in
  kfprintf IO.close_out oc fmt
let ksprintf k fmt =
  let oc = IO.output_buffer (Buffer.create 42) in
  kfprintf (fun oc -> k (IO.close_out oc)) oc fmt
