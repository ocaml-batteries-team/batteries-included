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
  (Q.string) (fun s -> let b = create 5 in \
    add_string b "foo"; add_string b s; add_string b "bar"; \
    BatIO.to_string print b = "foo" ^ s ^ "bar")
*)

let gen t =
  let buf = buffer_of_t t in
  let i = ref 0 in
  fun () ->
    if !i = buf.position then None
    else Some buf.buffer.[BatRef.post_incr i]

(*$Q gen
  (Q.string) (fun s -> let b = create 10 in add_string b s; \
    BatGen.eq ~eq:Char.equal (gen b) (BatString.gen s))
*)

let of_gen g =
  let s = BatString.of_gen g in
  let buf = create (String.length s) in
  add_string buf s;
  buf

(*$Q of_gen
  (Q.string) (fun s -> let b = of_gen (BatString.gen s) in contents b = s)
  (Q.string) (fun s -> let e = BatString.gen s in \
                       contents (of_gen e) = s)
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
