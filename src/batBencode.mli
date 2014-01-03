(*
 * BatList - additional and modified functions for lists.
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 Red Hat Inc.
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

(** {1 B-encode Serialization Format}

    B-encode is a textual data serialization format that is easy to parse
    and print, relatively efficient, and can store binary data and arbitrary
    strings without escaping.

    See {{: http://en.wikipedia.org/wiki/Bencode} wikipedia} for more details

    @documents Bencode

    @author Simon Cruanes
*)

module SMap : Map.S with type key = string

type t =
  | I of int
  | S of string
  | L of t list
  | D of t SMap.t

val equal : t -> t -> bool

val hash : t -> int

val dict_of_list : (string * t) list -> t

(** {6 Serialization (encoding)} *)

module Encoder : sig
  type t

  val of_output : 'a BatIO.output -> t

  val of_buffer : Buffer.t -> t
end

val byte_size : t -> int
(** Size needed for serialization of this value. Linear complexity. *)

val encode : Encoder.t -> t -> unit
(** Encode the value to the given encoder *)

val to_string : t -> string
(** Convert the B-encode value to a string *)

val print : 'a BatIO.output -> t -> unit
(** Print the encoded value on the output *)

val pretty_print : 'a BatIO.output -> t -> unit
  (** Print a human-readable representation of the B-encode value,
      not its string encoding *)

val pretty_to_string : t -> string
  (** Print the tree into a string *)

(** {6 Deserialization (decoding)} *)

(** Deserialization is based on the {! Decoder.t} type. Parsing can be
    incremental, in which case the input is provided chunk by chunk and
    the decoder contains the parsing state. Once a B-encoded value
    has been parsed, other values can still be read. *)

type parse_result =
  | ParseOk of t
  | ParseError of string
  | ParsePartial

module Decoder : sig
  type t
  (** Holds the decoding state *)

  val create : unit -> t
  (** Create a new decoder *)

  val reset : t -> unit
  (** Reset the decoder to its pristine state, ready to parse something
      different. Before that, {! rest} and {! rest_size} can be used
      to recover the part of the input that has not been consumed yet. *)

  val state : t -> parse_result
    (** Current state of the decoder *)

  val rest : t -> string
    (** What remains after parsing (the additional, unused input) *)

  val rest_size : t -> int
    (** Length of [rest d]. 0 indicates that the whole input has been consumed. *)
end

val decode : Decoder.t -> string -> int -> int -> parse_result
(** [parse dec s i len] uses the partial state stored in [dec] and
    the substring of [s] starting at index [i] with length [len].
    It can return an error, a value or just [ParsePartial] if
    more input is needed *)

val resume_parsing : Decoder.t -> parse_result
(** Resume where the previous call to {!decode} stopped (may have
    returned a value while some input is not processed) *)

(** {6 Utils} *)

val of_string : string -> (t, string) BatResult.t
(** Read a single value from the string *)

val decode_enum : ?dec:Decoder.t -> string BatEnum.t -> (t,string) BatResult.t BatEnum.t
(** Parse as many values as possible from the stream. The resulting enumeration
    is not clonable in itself, and stops as soon as EOF is met or
    a [BatResult.Bad] value is returned.
  
    @param dec optional decoder to use (a fresh one is created if none
    is provided). *)

val decode_string : ?dec:Decoder.t -> string -> (t, string) BatResult.t BatEnum.t
(** Parse values from the string *)

val decode_input : ?dec:Decoder.t -> BatIO.input -> (t, string) BatResult.t BatEnum.t
(** Parse values from the input using the given decoder. *)

(** {6 Type serialization} *)

type bencode = t

module Convert : sig
  exception Error of string
  (** To be raised by conversion function that try to transform a B-encode value
      into some other type *)

  type 'a t = {
    to_bencode : 'a -> bencode;
    of_bencode : bencode -> 'a; (** Raise [Error] if it fails *)
  } (** Conversion between ['a] and B-encode values *)

  val make : to_bencode:('a -> bencode) -> of_bencode:(bencode -> 'a) -> 'a t
  (** create a conversion structure *)

  (** {7 Utils}
  
  The reading functions can raise {!Error}. *)

  val to_bencode : 'a t -> 'a -> bencode

  val of_bencode : 'a t -> bencode -> 'a

  val to_string : 'a t -> 'a -> string

  val print : 'a t -> 'a BatInnerIO.output -> 'a -> unit

  val of_string : 'a t -> string -> 'a
  (** read exactly one B-encode value from the string, and convert it *)

  val get : 'a t -> bencode -> ('a, string) BatResult.t
  (** exceptionless version of {!of_bencode} *)

  val read : 'a t -> BatIO.input -> ('a,string) BatResult.t BatEnum.t
  (** [read convert i] reads B-encode values from the input, and
      converts them using [convert] *)

  (** {7 Basic instances} *)

  val string : string t

  val int : int t

  val list : 'a t -> 'a list t

  val dict : 'a t -> 'a SMap.t t
end
