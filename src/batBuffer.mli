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


(** Extensible string buffers.

    This module implements string buffers that automatically expand
    as necessary.  It provides accumulative concatenation of strings
    in quasi-linear time (instead of quadratic time when strings are
    concatenated pairwise).

    @author Pierre Weis (Base module)
    @author Xavier Leroy (Base module)
    @author David Teller
    @author Dawid Toton
*)

type t = Buffer.t
(** The abstract type of buffers. *)

val create : int -> t
(** [create n] returns a fresh buffer, initially empty.
    The [n] parameter is the initial size of the internal string
    that holds the buffer contents. That string is automatically
    reallocated when more than [n] characters are stored in the buffer,
    but shrinks back to [n] characters when [reset] is called.
    For best performance, [n] should be of the same order of magnitude
    as the number of characters that are expected to be stored in
    the buffer (for instance, 80 for a buffer that holds one output
    line).  Nothing bad will happen if the buffer grows beyond that
    limit, however. In doubt, take [n = 16] for instance.
    If [n] is not between 1 and {!Sys.max_string_length}, it will
    be clipped to that interval. *)

val contents : t -> string
(** Return a copy of the current contents of the buffer.
    The buffer itself is unchanged. *)

val sub : t -> int -> int -> string
(** [Buffer.sub b off len] returns (a copy of) the substring of the
    current contents of the buffer [b] starting at offset [off] of length
    [len] bytes. May raise [Invalid_argument] if out of bounds request. The
    buffer itself is unaffected. *)

val blit : t -> int -> string -> int -> int -> unit
(** [Buffer.blit src srcoff dst dstoff len] copies [len] characters from
    the current contents of the buffer [src], starting at offset [srcoff]
    to string [dst], starting at character [dstoff].

    @raise Invalid_argument if [srcoff] and [len] do not designate a valid
    substring of [src], or if [dstoff] and [len] do not designate a valid
    substring of [dst].
    @since 3.11.2
*)

val nth : t -> int -> char
(** get the n-th character of the buffer. @raise Invalid_argument if
    index out of bounds *)

val length : t -> int
(** Return the number of characters currently contained in the buffer. *)

val clear : t -> unit
(** Empty the buffer. *)

val reset : t -> unit
(** Empty the buffer and deallocate the internal string holding the
    buffer contents, replacing it with the initial internal string
    of length [n] that was allocated by {!Buffer.create} [n].
    For long-lived buffers that may have grown a lot, [reset] allows
    faster reclamation of the space used by the buffer. *)

val add_char : t -> char -> unit
(** [add_char b c] appends the character [c] at the end of the buffer [b]. *)

val add_string : t -> string -> unit
(** [add_string b s] appends the string [s] at the end of the buffer [b]. *)

val add_substring : t -> string -> int -> int -> unit
(** [add_substring b s ofs len] takes [len] characters from offset
    [ofs] in string [s] and appends them at the end of the buffer [b]. *)

val add_substitute : t -> (string -> string) -> string -> unit
(** [add_substitute b f s] appends the string pattern [s] at the end
    of the buffer [b] with substitution.
    The substitution process looks for variables into
    the pattern and substitutes each variable name by its value, as
    obtained by applying the mapping [f] to the variable name. Inside the
    string pattern, a variable name immediately follows a non-escaped
    [$] character and is one of the following:
    - a non empty sequence of alphanumeric or [_] characters,
    - an arbitrary sequence of characters enclosed by a pair of
    matching parentheses or curly brackets.
    An escaped [$] character is a [$] that immediately follows a backslash
    character; it then stands for a plain [$].
    @raise Not_found if the closing character of a parenthesized variable
    cannot be found. *)

val add_buffer : t -> t -> unit
(** [add_buffer b1 b2] appends the current contents of buffer [b2]
    at the end of buffer [b1].  [b2] is not modified. *)

val add_input : t -> BatInnerIO.input -> int -> unit
(** [add_input b ic n] reads exactly [n] character from the input [ic]
    and stores them at the end of buffer [b].  @raise End_of_file if
    the channel contains fewer than [n] characters. *)

val add_channel : t -> BatInnerIO.input -> int -> unit
(** @obsolete replaced by {!add_input}*)

val output_buffer : t -> string BatInnerIO.output
(** [output_buffer b] creates an output channel that writes to that
    buffer, and when closed, returns the contents of the buffer. *)


(** {6 Boilerplate code}*)

val enum : t -> char BatEnum.t
(** Returns an enumeration of the characters of a buffer.

    Contents of the enumeration is unspecified if the buffer is modified after
    the enumeration is returned.*)

val of_enum : char BatEnum.t -> t
(** Creates a buffer from a character enumeration. *)

val print: 'a BatInnerIO.output -> t -> unit
