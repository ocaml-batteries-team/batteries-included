(*
 * ExtDigest - Additional functions for MD5 message digests
 * Copyright (C) 1996 Xavier Leroy, INRIA Rocquencourt
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
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


(** MD5 message digest.

   This module provides functions to compute 128-bit ``digests'' of
   arbitrary-length strings or files. The digests are of cryptographic
   quality: it is very hard, given a digest, to forge a string having
   that digest. The algorithm used is MD5.

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Digest.html}Digest}
    module, go there for documentation on the rest of the functions
    and types.

    @author Xavier Leroy (Base module)
    @author David Rajchenbach-Teller
*)
open BatIO
open Digest

val from_hex : string -> t
(** Convert a hexadecimal representation back into the corresponding digest.
   Raise [Invalid_argument] if the argument is not exactly 32 hexadecimal
   characters.
   @since 4.00.0 *)

val channel : input -> int -> t
(** If [len] is nonnegative, [Digest.channel ic len] reads [len]
   characters from channel [ic] and returns their digest, or raises
   [End_of_file] if end-of-file is reached before [len] characters
   are read.  If [len] is negative, [Digest.channel ic len] reads
   all characters from [ic] until end-of-file is reached and return
   their digest.

    {b Note} This version of [channel] is currently very inefficient
    if [len] < 0 and requires copying the whole input to a temporary
    file.
*)


val output : 'a output -> t -> unit
(** Write a digest on the given output. *)

val input : input -> t
(** Read a digest from the given input. *)

(** {6 Boilerplate code}*)

val compare : t -> t -> int
(** The comparison function for 16-character digest, with the same
    specification as {!Pervasives.compare} and the implementation
    shared with {!String.compare}. Along with the type [t], this
    function [compare] allows the module [Digest] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}.
    @since 4.00.0 *)
