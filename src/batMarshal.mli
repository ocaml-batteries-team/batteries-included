(* 
 * ExtMarshal - Extended marshaling operations 
 * Copyright (C) 1997 Xavier Leroy
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


(** Marshaling of data structures.

    This module provides functions to encode arbitrary data structures
    as sequences of bytes, which can then be written on a file or
    sent over a pipe or network connection.  The bytes can then
    be read back later, possibly in another process, and decoded back
    into a data structure. The format for the byte sequences
    is compatible across all machines for a given version of Objective Caml.

    Warning: marshaling is currently not type-safe. The type
    of marshaled data is not transmitted along the value of the data,
    making it impossible to check that the data read back possesses the
    type expected by the context. In particular, the result type of
    the [Marshal.from_*] functions is given as ['a], but this is
    misleading: the returned Caml value does not possess type ['a]
    for all ['a]; it has one, unique type which cannot be determined
    at compile-type.  The programmer should explicitly give the expected
    type of the returned value, using the following syntax:
    - [(Marshal.from_channel chan : type)].
    Anything can happen at run-time if the object in the file does not
    belong to the given type.

    The representation of marshaled values is not human-readable, and
    uses bytes that are not printable characters. Therefore, input and
    output channels used in conjunction with {!Marshal.output} and
    {!Marshal.input} must be opened in binary mode, using e.g.
    {!BatPervasives.open_out_bin} or
    {!BatPervasives.open_in_bin}; channels opened in text
    mode will cause unmarshaling errors on platforms where text
    channels behave differently than binary channels, e.g. Windows.

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Marshal.html}Marshal}
    module, go there for documentation on the rest of the functions
    and types.
    
    @author Xavier Leroy (base module)
    @author David Teller
*)

open Marshal

val output: _ BatInnerIO.output -> ?sharing:bool -> ?closures:bool -> 'a -> unit
  (** [output out v] writes the representation of [v] on [chan]. 

      @param sharing If [true] (default value), circularities
      and sharing inside the value [v] are detected and preserved
      in the sequence of bytes produced. In particular, this
      guarantees that marshaling always terminates. Sharing
      between values marshaled by successive calls to
      [output] is not detected, though. If [false], sharing is ignored.
      This results in faster marshaling if [v] contains no shared
      substructures, but may cause slower marshaling and larger
      byte representations if [v] actually contains sharing,
      or even non-termination if [v] contains cycles.

      @param closures If [false] (default value) marshaling fails when
      it encounters a functional value inside [v]: only ``pure'' data
      structures, containing neither functions nor objects, can safely
      be transmitted between different programs. If [true], functional
      values will be marshaled as a position in the code of the
      program. In this case, the output of marshaling can only be read
      back in processes that run exactly the same program, with
      exactly the same compiled code. (This is checked at
      un-marshaling time, using an MD5 digest of the code transmitted
      along with the code position.) *)

val input : BatInnerIO.input -> 'a
  (** [input inp] reads from [inp] the
      byte representation of a structured value, as produced by
      one of the [Marshal.to_*] functions, and reconstructs and
      returns the corresponding value.*)


val to_channel : _ BatInnerIO.output -> 'a -> extern_flags list -> unit
  (** @deprecated Use {!output} instead *)

val from_channel : BatInnerIO.input -> 'a
  (** @deprecated Use {!input} instead *)

