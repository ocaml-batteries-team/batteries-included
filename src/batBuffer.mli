(* 
 * ExtBuffer - Additional buffer operations
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


(** Extensible string buffers.

   This module implements string buffers that automatically expand
   as necessary.  It provides accumulative concatenation of strings
   in quasi-linear time (instead of quadratic time when strings are
   concatenated pairwise).

    
    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Buffer.html}Buffer}
    module, go there for documentation on the rest of the functions
    and types.

    @author Pierre Weis (Base module)
    @author Xavier Leroy (Base module)
    @author David Teller
    @author Dawid Toton
*)
open Buffer


val enum : t -> char BatEnum.t
  (** Returns an enumeration of the characters of a buffer. 

      Contents of the enumeration is unspecified if the buffer is modified after
      the enumeration is returned.*)

val of_enum : char BatEnum.t -> t
  (** Creates a buffer from a character enumeration. *)


val blit : t -> int -> string -> int -> int -> unit
(** [Buffer.blit b srcoff dst dstoff len] copies [len] characters from
   the current contents of the buffer [b] starting at offset [off],
   starting at character number [srcoff], to string [dst], starting at
   character number [dstoff].  

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid substring of the buffer, or if [dstoff] and [len] do not
    designate a valid substring of [dst]. *)

val add_input : t -> BatInnerIO.input -> int -> unit
  (** [add_input b ic n] reads exactly [n] character from the input [ic]
      and stores them at the end of buffer [b].  Raise [End_of_file] if
      the channel contains fewer than [n] characters. *)

val add_channel : t -> BatInnerIO.input -> int -> unit
  (** @obsolete replaced by {!add_input}*)

val output_buffer : _ BatInnerIO.output -> t -> unit
  (** [output_buffer oc b] writes the current contents of buffer [b]
      on the output channel [oc]. *)

(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print: 'a BatInnerIO.output -> t -> unit

