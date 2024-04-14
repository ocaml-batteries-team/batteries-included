(*
 * BatDigest - Additional functions for MD5 message digests
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

    @author Xavier Leroy (Base module)
    @author David Rajchenbach-Teller
*)

open BatIO

type t = string
(** The type of digests: 16-character strings. *)

val string : string -> t
(** Return the digest of the given string. *)

val bytes : Bytes.t -> t
(** Return the digest of the given byte sequence.

    @since 2.3.0
*)

val substring : string -> int -> int -> t
(** [Digest.substring s ofs len] returns the digest of the substring
    of [s] starting at character number [ofs] and containing [len]
    characters. *)

val subbytes : Bytes.t -> int -> int -> t
(** [Digest.subbytes s ofs len] returns the digest of the subsequence
    of [s] starting at index [ofs] and containing [len] bytes.

    @since 2.3.0
*)

val file : string -> t
(** Return the digest of the file whose name is given. *)

val to_hex : t -> string
(** Return the printable hexadecimal representation of the given digest. *)

val from_hex : string -> t
(** Convert a hexadecimal representation back into the corresponding digest.
    @raise Invalid_argument if the argument is not exactly 32 hexadecimal
    characters.
    @since 4.00.0 *)

val channel : input -> int -> Digest.t
(** If [len] is nonnegative, [Digest.channel ic len] reads [len]
    characters from channel [ic] and returns their digest, or
    @raise End_of_file if end-of-file is reached before [len] characters
    are read.  If [len] is negative, [Digest.channel ic len] reads
    all characters from [ic] until end-of-file is reached and return
    their digest.

    {b Note} This version of [channel] is currently very inefficient
    if [len] < 0 and requires copying the whole input to a temporary
    file.
*)

val output : 'a output -> t -> unit
(** Write a digest on the given output. *)

val print : 'a output -> Digest.t -> unit
(** Write a digest on the given output in hexadecimal. *)

val input : input -> Digest.t
(** Read a digest from the given input. *)

val compare : t -> t -> int
  (** The comparison function for 16-character digest, with the same
      specification as {!Pervasives.compare} and the implementation
      shared with {!String.compare}. Along with the type [t], this
      function [compare] allows the module [Digest] to be passed as
      argument to the functors {!Set.Make} and {!Map.Make}.
      @since Batteries 2.0 *)

val equal : t -> t -> bool
(** The equal function for digests.
    @since 2.5.0 *)

##V>=5.2##val of_hex : string -> t

##V>=5.2##module type S = sig
##V>=5.2##  type t = string
##V>=5.2##  val hash_length : int
##V>=5.2##  val compare : t -> t -> int
##V>=5.2##  val equal : t -> t -> bool
##V>=5.2##  val string : string -> t
##V>=5.2##  val bytes : bytes -> t
##V>=5.2##  val substring : string -> int -> int -> t
##V>=5.2##  val subbytes : bytes -> int -> int -> t
##V>=5.2##  val channel : Stdlib.in_channel -> int -> t
##V>=5.2##  val file : string -> t
##V>=5.2##  val output : Stdlib.out_channel -> t -> unit
##V>=5.2##  val input : Stdlib.in_channel -> t
##V>=5.2##  val to_hex : t -> string
##V>=5.2##  val of_hex : string -> t
##V>=5.2##end
##V>=5.2##module BLAKE128 : S
##V>=5.2##module BLAKE256 : S
##V>=5.2##module BLAKE512 : S
##V>=5.2##module MD5 : S
