(*
 * BatInnerIO - Abstract input/output (inner module)
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 Philippe Strauss
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

module UID : sig
  type t = private int

  val placeholder : t
  val uid : unit -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end = struct
  type t = int
  let placeholder = (-1)
  let counter = ref (-1)
  let uid () =
    incr counter; !counter

  let equal (a : int) b = (a = b)
  let compare (a : int) b = Pervasives.compare a b
  let hash (a : int) = Hashtbl.hash a
end

type 'upstream _input = {
  mutable in_read  : unit -> char;
  mutable in_input : string -> int -> int -> int;
  mutable in_close : unit -> unit;
  in_id: UID.t;(**A unique identifier.*)
  in_upstream: 'upstream;
}

type ('a, 'upstream) _output = {
  mutable out_write : char -> unit;
  mutable out_output: string -> int -> int -> int;
  mutable out_close : unit -> 'a;
  mutable out_flush : unit -> unit;
  out_id: UID.t;(**A unique identifier.*)
  out_upstream: 'upstream;
  (** The set of outputs which have been created to write to this output.*)
}

module rec Input
  : sig
    type t = InputWeakSet.t _input
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end
  = struct
    type t = InputWeakSet.t _input
    let equal inp1 inp2 = Pervasives.(=) inp1.in_id inp2.in_id
    let compare inp1 inp2 = Pervasives.compare inp1.in_id inp2.in_id
    let hash inp = Hashtbl.hash inp.in_id
  end
and InputWeakSet : (Weak.S with type data = Input.t) = Weak.Make(Input)

module rec Output
  : sig
    type 'a output = ('a, OutputWeakSet.t) _output
    type t = unit output
    val equal : 'a output -> 'b output -> bool
    val compare : 'a output -> 'b output -> int
    val hash : 'a output -> int
    end
  = struct
    type 'a output = ('a, OutputWeakSet.t) _output
    type t = unit output
    let equal out1 out2 = Pervasives.(=) out1.out_id out2.out_id
    let compare out1 out2 = Pervasives.compare out1.out_id out2.out_id
    let hash out = Hashtbl.hash out.out_id
  end

and OutputWeakSet : (Weak.S with type data = Output.t) = Weak.Make(Output)

(**All the currently opened outputs -- used to permit [flush_all] and [close_all].*)

type input = InputWeakSet.t _input
type 'a output = ('a, OutputWeakSet.t) _output

let nwrite o s =
  let p = ref 0 in
  let l = ref (String.length s) in
  while !l > 0 do
    let w = o.out_output s !p !l in
    (* FIXME: unknown how many characters were already written *)
    if w = 0 then raise Sys_blocked_io;
    p := !p + w;
    l := !l - w;
  done


let write o x = o.out_write x
