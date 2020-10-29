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

val to_bytes : t -> Bytes.t
(** Return a copy of the current contents of the buffer.
    The buffer itself is unchanged.

    @since 2.3.0
*)

val sub : t -> int -> int -> string
(** [Buffer.sub b off len] returns a copy of [len] bytes from the
    current contents of the buffer [b], starting at offset [off].

    Raise [Invalid_argument] if [srcoff] and [len] do not designate a valid
    range of [b]. *)

val blit : t -> int -> Bytes.t -> int -> int -> unit
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

val add_bytes : t -> Bytes.t -> unit
(** [add_bytes b s] appends the string [s] at the end of the buffer [b].

    @since 2.3.0
*)

val add_substring : t -> string -> int -> int -> unit
(** [add_substring b s ofs len] takes [len] characters from offset
    [ofs] in string [s] and appends them at the end of the buffer [b]. *)

val add_subbytes : t -> Bytes.t -> int -> int -> unit
(** [add_subbytes b s ofs len] takes [len] characters from offset
    [ofs] in byte sequence [s] and appends them at the end of the buffer [b].

    @since 2.3.0
*)

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

##V>=4.5##val truncate : t -> int -> unit
##V>=4.5##(** [truncate b len] truncates the length of [b] to [len]
##V>=4.5##    Note: the internal byte sequence is not shortened.
##V>=4.5##    Raises [Invalid_argument] if [len < 0] or [len > length b].
##V>=4.5## @since 2.7.0 and OCaml 4.05.0 *)

##V>=4.6##val add_utf_8_uchar : t -> Uchar.t -> unit
##V>=4.6##(** [add_utf_8_uchar b u] appends the {{:https://tools.ietf.org/html/rfc3629}
##V>=4.6##    UTF-8} encoding of [u] at the end of buffer [b].
##V>=4.6##
##V>=4.6##    @since 2.8.0 and OCaml 4.06.0 *)

##V>=4.6##val add_utf_16le_uchar : t -> Uchar.t -> unit
##V>=4.6##(** [add_utf_16le_uchar b u] appends the
##V>=4.6##    {{:https://tools.ietf.org/html/rfc2781}UTF-16LE} encoding of [u]
##V>=4.6##    at the end of buffer [b].
##V>=4.6##
##V>=4.6##    @since 2.8.0 and OCaml 4.06.0 *)

##V>=4.6##val add_utf_16be_uchar : t -> Uchar.t -> unit
##V>=4.6##(** [add_utf_16be_uchar b u] appends the
##V>=4.6##    {{:https://tools.ietf.org/html/rfc2781}UTF-16BE} encoding of [u]
##V>=4.6##    at the end of buffer [b].
##V>=4.6##
##V>=4.6##    @since 2.8.0 and OCaml 4.06.0 *)

(** {6 Boilerplate code}*)

val enum : t -> char BatEnum.t
(** Returns an enumeration of the characters of a buffer.

    Contents of the enumeration is unspecified if the buffer is modified after
    the enumeration is returned.*)

val of_enum : char BatEnum.t -> t
(** Creates a buffer from a character enumeration. *)

val print: 'a BatInnerIO.output -> t -> unit

##V>=4.07##(** {1 Iterators} *)
##V>=4.07##
##V>=4.07##val to_seq : t -> char Seq.t
##V>=4.07##(** Iterate on the buffer, in increasing order.
##V>=4.07##    Modification of the buffer during iteration is undefined behavior.
##V>=4.07##    @since 2.10.0 and OCaml 4.07 *)

##V>=4.07##val to_seqi : t -> (int * char) Seq.t
##V>=4.07##(** Iterate on the buffer, in increasing order, yielding indices along chars.
##V>=4.07##    Modification of the buffer during iteration is undefined behavior.
##V>=4.07##    @since 2.10.0 and OCaml 4.07 *)

##V>=4.07##val add_seq : t -> char Seq.t -> unit
##V>=4.07##(** Add chars to the buffer
##V>=4.07##    @since 2.10.0 and OCaml 4.07 *)

##V>=4.07##val of_seq : char Seq.t -> t
##V>=4.07##(** Create a buffer from the generator
##V>=4.07##    @since 2.10.0 and OCaml 4.07 *)

##V>=4.08##(** {1 Binary encoding of integers} *)
##V>=4.08##
##V>=4.08##(** The functions in this section append binary encodings of integers
##V>=4.08##    to buffers.
##V>=4.08##
##V>=4.08##    Little-endian (resp. big-endian) encoding means that least
##V>=4.08##    (resp. most) significant bytes are stored first.  Big-endian is
##V>=4.08##    also known as network byte order.  Native-endian encoding is
##V>=4.08##    either little-endian or big-endian depending on {!Sys.big_endian}.
##V>=4.08##
##V>=4.08##    32-bit and 64-bit integers are represented by the [int32] and
##V>=4.08##    [int64] types, which can be interpreted either as signed or
##V>=4.08##    unsigned numbers.
##V>=4.08##
##V>=4.08##    8-bit and 16-bit integers are represented by the [int] type,
##V>=4.08##    which has more bits than the binary encoding.  Functions that
##V>=4.08##    encode these values truncate their inputs to their least
##V>=4.08##    significant bytes.
##V>=4.08##*)

##V>=4.08##val add_uint8 : t -> int -> unit
##V>=4.08##(** [add_uint8 b i] appends a binary unsigned 8-bit integer [i] to
##V>=4.08##    [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int8 : t -> int -> unit
##V>=4.08##(** [add_int8 b i] appends a binary signed 8-bit integer [i] to
##V>=4.08##    [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_uint16_ne : t -> int -> unit
##V>=4.08##(** [add_uint16_ne b i] appends a binary native-endian unsigned 16-bit
##V>=4.08##    integer [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_uint16_be : t -> int -> unit
##V>=4.08##(** [add_uint16_be b i] appends a binary big-endian unsigned 16-bit
##V>=4.08##    integer [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_uint16_le : t -> int -> unit
##V>=4.08##(** [add_uint16_le b i] appends a binary little-endian unsigned 16-bit
##V>=4.08##    integer [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int16_ne : t -> int -> unit
##V>=4.08##(** [add_int16_ne b i] appends a binary native-endian signed 16-bit
##V>=4.08##    integer [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int16_be : t -> int -> unit
##V>=4.08##(** [add_int16_be b i] appends a binary big-endian signed 16-bit
##V>=4.08##    integer [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int16_le : t -> int -> unit
##V>=4.08##(** [add_int16_le b i] appends a binary little-endian signed 16-bit
##V>=4.08##    integer [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int32_ne : t -> int32 -> unit
##V>=4.08##(** [add_int32_ne b i] appends a binary native-endian 32-bit integer
##V>=4.08##    [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int32_be : t -> int32 -> unit
##V>=4.08##(** [add_int32_be b i] appends a binary big-endian 32-bit integer
##V>=4.08##    [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int32_le : t -> int32 -> unit
##V>=4.08##(** [add_int32_le b i] appends a binary little-endian 32-bit integer
##V>=4.08##    [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int64_ne  : t -> int64 -> unit
##V>=4.08##(** [add_int64_ne b i] appends a binary native-endian 64-bit integer
##V>=4.08##    [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int64_be : t -> int64 -> unit
##V>=4.08##(** [add_int64_be b i] appends a binary big-endian 64-bit integer
##V>=4.08##    [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)

##V>=4.08##val add_int64_le : t -> int64 -> unit
##V>=4.08##(** [add_int64_ne b i] appends a binary little-endian 64-bit integer
##V>=4.08##    [i] to [b].
##V>=4.08##    @since 2.10.0 and OCaml 4.08
##V>=4.08##*)
