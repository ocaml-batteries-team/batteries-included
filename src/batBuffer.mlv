(*
 * BatBuffer - Additional buffer operations
 * Copyright (C) 1999 Pierre Weis, Xavier Leroy
 *               2009 David Teller, LIFO, Universite d'Orleans
 *               2009 Dawid Toton
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


open BatString
include Buffer

(** The underlying buffer type. *)
type buffer =
  {mutable buffer : string;(** Contents of the buffer *)
   mutable position : int; (** The end of the buffer  *)
   mutable length : int;   (** The size of the buffer *)
   initial_buffer : string (** For resetting to the original size **)}

external buffer_of_t : t -> buffer = "%identity"
external t_of_buffer : buffer -> t = "%identity"

let print out t =
  BatString.print out (contents t)

(*$Q print
  (Q.string) (fun s -> let b = create 5 in add_string b "foo"; add_string b s; add_string b "bar"; BatIO.to_string print b = "foo" ^ s ^ "bar")
*)

let enum t =
  let buf = buffer_of_t t in
  BatEnum.take buf.position (BatString.enum buf.buffer)

(*$Q enum
  (Q.string) (fun s -> let b = create 10 in add_string b s; BatEnum.equal Char.equal (enum b) (BatString.enum s))
*)

let of_enum e =
  let buf =
    if BatEnum.fast_count e
    then create (BatEnum.count e)
    else create 128
  in
  add_string buf (BatString.of_enum e);
  buf

(*$Q of_enum
  (Q.string) (fun s -> let b = of_enum (BatString.enum s) in contents b = s)
  (Q.string) (fun s -> let e = BatString.enum s in \
                       let e = BatEnum.from (fun () -> BatEnum.get_exn e) in \
                       contents (of_enum e) = s)
*)

let add_input t inp n =
  add_string t (BatInnerIO.really_nread inp n)

(*$Q add_input
  (Q.string) (fun s -> let b = create 10 in add_input b (BatIO.input_string s) (String.length s); contents b = s)
*)

let output_buffer buf =
  BatInnerIO.create_out
    ~write: (add_char buf)
    ~output:(fun s p l -> add_substring buf s p l; l)
    ~close: (fun () -> contents buf)
    ~flush: BatInnerIO.noop

(*$Q output_buffer
  (Q.string) (fun s -> let b = create 10 in let oc = output_buffer b in IO.nwrite oc s; IO.close_out oc = s)
*)

let add_channel = add_input
