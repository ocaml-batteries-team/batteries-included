(*
 * BatConv - additional and modified functions for lists.
 * Copyright (C) 2014 Simon Cruanes
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

(** {1 Serialization and Deserialization} *)

exception ConversionFailure of string

val report_error : ('a, Buffer.t, unit, 'b) format4 -> 'a
(** Helper to report conversion errors.
    @raise ConversionFailure *)

(** {6 Universal sink}

Some type any valye can be traducted into, such as a serialization format
like JSON or B-encode. *)
module UniversalSink : sig
  type 'a t = {
    unit_ : 'a;
    bool_ : bool -> 'a;
    float_ : float -> 'a;
    int_ : int -> 'a;
    string_ : string -> 'a;
    list_ : 'a list -> 'a;
    record : (string*'a) list -> 'a;
    tuple : 'a list -> 'a;
    sum : string -> 'a list -> 'a;
  }
end

(** {6 Sources}
A 'a source is used to build values of some type 'b, given a 'b sink
description of how to build values of type 'b. *)
module Source : sig
  type 'a t = {
    convert : 'b. 'b UniversalSink.t -> 'a -> 'b;
  }

  type 'r record_src

  type hlist =
    | HNil : hlist
    | HCons : 'a t * 'a * hlist -> hlist

  val hnil : hlist
  val hcons : 'a t -> 'a -> hlist -> hlist

  val unit_ : unit t
  val bool_ : bool t
  val float_ : float t
  val int_ : int t
  val string_ : string t
  val list_ : 'a t -> 'a list t

  val map : ('a -> 'b) -> 'b t -> 'a t
  val array_ : 'a t -> 'a array t
  val gen : 'a t -> 'a BatGen.t t

  val field : string -> ('r -> 'a) -> 'a t -> 'r record_src -> 'r record_src
  val record_stop : 'r record_src
  val record : 'r record_src -> 'r t
  val record_fix : ('r t -> 'r record_src) -> 'r t

  val tuple : ('a -> hlist) -> 'a t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val sum : ('a -> string * hlist) -> 'a t
  val sum0 : ('a -> string) -> 'a t
  val sum_fix : ('a t -> 'a -> string * hlist) -> 'a t

  val opt : 'a t -> 'a option t
end

(** {6 Sinks}
A sink is used to produce values of type 'a from a universal source. *)
module Sink : sig
  type 'a t  (** How to produce values of type 'a *)

  and 'r record_sink =
    | RecordField : string * 'a t * ('a -> 'r record_sink) -> 'r record_sink
    | RecordStop : 'r -> 'r record_sink

  and 't hlist =
    | HCons : 'a t * ('a -> 't hlist) -> 't hlist
    | HNil : 't -> 't hlist

  val unit_ : unit t
  val bool_ : bool t
  val float_ : float t
  val int_ : int t
  val string_ : string t
  val list_ : 'a t -> 'a list t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val array_ : 'a t -> 'a array t
  val gen : 'a t -> 'a BatGen.Restart.t t

  val field : string -> 'a t -> ('a -> 'r record_sink) -> 'r record_sink
  val yield_record : 'r -> 'r record_sink
  val record : 'r record_sink -> 'r t
  val record_fix : ('r t -> 'r record_sink) -> 'r t

  val (|+|) : 'a t -> ('a -> 't hlist) -> 't hlist
  val yield : 'a -> 'a hlist

  val tuple : 't hlist -> 't t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val sum : (string -> 'a hlist) -> 'a t
  val sum_fix : ('a t -> string -> 'a hlist) -> 'a t

  val opt : 'a t -> 'a option t

  (** What is expected by the sink? *)
  type expected =
    | ExpectInt
    | ExpectBool
    | ExpectUnit
    | ExpectFloat
    | ExpectString
    | ExpectRecord
    | ExpectTuple
    | ExpectList
    | ExpectSum

  val expected : _ t -> expected
    (** To be used by sources that have ambiguities to know what is expected.
        maps and fixpoints are unrolled. *)
end

(** {6 Universal source}

source from type 'a, where 'a is typically a serialization
format. This is used to translate from 'a to some other type. 
A universal format should use the provided combinators to
interface with {!Sink.t} values *)
module UniversalSource : sig
  type 'a t = {
    visit : 'b. 'b Sink.t -> 'a -> 'b;
  }

  val unit_ : 'b Sink.t -> 'b
  val bool_ : 'b Sink.t -> bool -> 'b
  val float_ : 'b Sink.t -> float -> 'b
  val int_ : 'b Sink.t -> int -> 'b
  val string_ : 'b Sink.t -> string -> 'b
  val list_ : src:'a t -> 'b Sink.t -> 'a list -> 'b
  val record : src:'a t -> 'b Sink.t -> (string*'a) list -> 'b
  val tuple : src:'a t -> 'b Sink.t -> 'a list -> 'b
  val sum : src:'a t -> 'b Sink.t -> string -> 'a list -> 'b
end

(** {6 Conversion Functions} *)

val into : 'a Source.t -> 'b UniversalSink.t -> 'a -> 'b
  (** Conversion to universal sink *)

val from : 'a UniversalSource.t -> 'b Sink.t -> 'a -> 'b
  (** Conversion from universal source *)

