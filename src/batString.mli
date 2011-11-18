(*
 * ExtString - Additional functions for string manipulations.
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 1996 Xavier Leroy, INRIA Rocquencourt
 * Copyright (C) 2008 Edgar Friendly
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



(** String operations. 

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html}String}
    module, go there for documentation on the rest of the functions
    and types.

    If you're going to do a lot of string slicing, BatSubstring might be
    a useful module to represent slices of strings, as it doesn't
    allocate new strings on every operation.

    @author Xavier Leroy (base library)
    @author Nicolas Cannasse
    @author David Teller
    @author Edgar Friendly
*)

open String

val is_empty : string -> bool
(** [is_empty s] returns [true] if [s] is the empty string, [false]
    otherwise.

    Usually a tad faster than comparing [s] with [""]. 
    
    Example (for some string [s]):
    [ if String.is_empty s then "(Empty)" else s ]
*)


val init : int -> (int -> char) -> string
  (** [init l f] returns the string of length [l] with the chars
      f 0 , f 1 , f 2 ... f (l-1). 

      Example: [String.init 256 char_of_int]
*)

(** {6 Conversions}*)
val enum : string -> char BatEnum.t
  (** Returns an enumeration of the characters of a string. 

      Examples:
        ["foo" |> String.enum |> List.of_enum = ['f'; 'o'; 'o']]
        [String.enum "a b c" // ((<>) ' ') |> String.of_enum = "abc"]
*)

val of_enum : char BatEnum.t -> string
  (** Creates a string from a character enumeration. 
      Example: [['f'; 'o'; 'o'] |> List.enum |> String.of_enum = "foo"]
*)

val backwards : string -> char BatEnum.t
  (** Returns an enumeration of the characters of a string, from last to first. 

      Examples:
      [ "foo" |> String.backwards |> String.of_enum = "oof" ]
      [ let rev s = String.backwards s |> String.of_enum ]
*)
  
val of_backwards : char BatEnum.t -> string
  (** Build a string from an enumeration, starting with last character, ending with first. 

      Examples:
      [ "foo" |> String.enum |> String.of_backwards = "oof" ]
      [ "foo" |> String.backwards |> String.of_backwards = "foo" ]
      [ let rev s = String.enum s |> String.of_backwards ]
*)


val of_list : char list -> string
   (** Converts a list of characters to a string.

       Example: [ ['c'; 'h'; 'a'; 'r'; 's'] |> String.of_list = "chars" ]
*) 

val to_list : string -> char list
  (** Converts a string to the list of its characters.

      Example: [ String.to_list "string" |> List.interleave ';' |> String.of_list = "s;t;r;i;n;g" ]
*)

val of_int : int -> string
  (** Returns the string representation of an int. 

      Example: [ String.of_int 56 = "56" && String.of_int (-1) = "-1" ]
*)

val of_float : float -> string
  (** Returns the string representation of an float. 

      Example: [ String.of_float 1.246 = "1.246" ]
*)

val of_char : char -> string
  (** Returns a string containing one given character. 

      Example: [ String.of_char 's' = "s" ]
*)

val to_int : string -> int
  (** Returns the integer represented by the given string or raises
      [Failure "int_of_string"] if the string does not represent an
      integer. This follows OCaml's int literal rules, so "0x"
      prefixes hexadecimal integers, "0o" for octal and "0b" for
      binary.  Underscores within the number are allowed for
      readability but ignored.

      Examples: [ String.to_int "8_480" = String.to_int "0x21_20" ]
      [ try ignore(String.to_int "2,3"); false with Failure _ -> true ]
  *)

val to_float : string -> float
  (** Returns the float represented by the given string or raises
      [Failure "float_of_string"] if the string does not represent a float.
      Decimal points aren't required in the given string, as they are
      for float literals in OCaml, but otherwise the rules for float
      literals apply.

      Examples: [String.to_float "12.34e-1" = String.to_float "1.234"]
      [String.to_float "1" = 1.]
      [try ignore(String.to_float ""); false with Failure _ -> true]
  *)

(** {6 String traversals}*)

val map : (char -> char) -> string -> string
  (** [map f s] returns a string where all characters [c] in [s] have been
      replaced by [f c]. 

      Example: [String.map Char.uppercase "Five" = "FIVE"]
**)
  
val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
  (** [fold_left f a s] is
      [f (... (f (f a s.[0]) s.[1]) ...) s.[n-1]] 

      Examples: [String.fold_left (fun li c -> c::li) [] "foo" = ['o';'o';'f']]
      [String.fold_left max 'a' "apples" = 's']
*)

val fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a
  (** [fold_right f s b] is
      [f s.[0] (f s.[1] (... (f s.[n-1] b) ...))] 

      Examples: [String.fold_right List.cons "foo" [] = ['f';'o';'o']]
      [String.fold_right (fun c a -> if c = ' ' then a+1 else a) "a b c" 0 = 2]
*)

val filter : (char -> bool) -> string -> string
  (** [filter f s] returns a copy of string [s] in which only
      characters [c] such that [f c = true] remain.

      Example: [ String.filter ((<>) ' ') "a b c" = "abc" ]
  *)

val filter_map : (char -> char option) -> string -> string
  (** [filter_map f s] calls [(f a0) (f a1).... (f an)] where [a0..an] are
      the characters of [s]. It returns the string of characters [ci] such as
      [f ai = Some ci] (when [f] returns [None], the corresponding element of
      [s] is discarded).

      Example: [ String.filter_map (function 'a'..'z' as c -> Some (Char.uppercase c) | _ -> None) "a b c" = "ABC" ]
 *)


val iteri : (int -> char -> unit) -> string -> unit
(** [String.iteri f s] is equivalent to
   [f 0 s.[0]; f 1 s.[1]; ...; f len s.[len]] where [len] is length of string [s]. 
    Example:
    {[ let letter_positions word =
         let positions = Array.make 256 [] in
         let count_letter pos c =
           positions.(int_of_char c) <- pos :: positions.(int_of_char c) in
         String.iteri count_letter word;
         Array.mapi (fun c pos -> (char_of_int c, List.rev pos)) positions
         |> Array.to_list
         |> List.filter (fun (c,pos) -> pos <> [])
       in
       letter_positions "hello" = ['e',[1]; 'h',[0]; 'l',[2;3]; 'o',[4] ]
    ]}
*)

(** {6 Finding}*)



val find : string -> string -> int
  (** [find s x] returns the starting index of the first occurrence of
      string [x] within string [s].

      {b Note} This implementation is optimized for short strings.

      @raise Not_found if [x] is not a substring of [s]. 

      Example: [String.find "foobarbaz" "bar" = 3]
*)

val find_from: string -> int -> string -> int
  (** [find_from s ofs x] behaves as [find s x] but starts searching
      at offset [ofs]. [find s x] is equivalent to [find_from s 0 x].

      @raises Not_found if [x] is not a substring of [tail ofs s]
      
      Example: [String.find_from "foobarbaz" 4 "ba" = 6]
*)

val rfind : string -> string -> int
  (** [rfind s x] returns the starting index of the last occurrence
      of string [x] within string [s].

      {b Note} This implementation is optimized for short strings.

      @raise Not_found if [x] is not a substring of [s]. 

      Example: [String.rfind "foobarbaz" "ba" = 6]
*)

val rfind_from: string -> int -> string -> int
(** [rfind_from s ofs x] behaves as [rfind s x] but starts searching
    from the right at offset [ofs]. [rfind s x] is equivalent to
    [rfind_from s (String.length s - 1) x].

    {b Beware}, it search between the {e beginning} of the string to
    the offset [ofs], {e not} between [ofs] and the end.

    @raises Not_found if [x] is not a substring of [head ofs s]

    Example: [String.rfind_from "foobarbaz" 6 "ba" = 6]
*)


val ends_with : string -> string -> bool
  (** [ends_with s x] returns [true] if the string [s] is ending with [x], [false] otherwise. 

      Example: [String.ends_with "foobarbaz" "rbaz" = true]
*)

val starts_with : string -> string -> bool
  (** [starts_with s x] returns [true] if [s] is starting with [x], [false] otherwise. 

      Example: [String.starts_with "foobarbaz" "fooz" = false]
*)

val exists : string -> string -> bool
  (** [exists str sub] returns true if [sub] is a substring of [str] or
      false otherwise. 

      Example: [String.exists "foobarbaz" "obar" = true]
*)

(** {6 Transformations}*)
  
val lchop : ?n:int -> string -> string
(** Returns the same string but without the first [n] characters.
    By default [n] is 1.
    If [n] is less than zero raises [Invalid_argument].
    If the string has [n] or less characters, returns the empty string.

      Example:
      [String.lchop "Weeble" = "eeble"]
      [String.lchop ~n:3 "Weeble" = "ble"]
      [String.lchop ~n:1000 "Weeble" = ""]
*)

val rchop : ?n:int -> string -> string
(** Returns the same string but without the last [n] characters.
    By default [n] is 1.
    If [n] is less than zero raises [Invalid_argument].
    If the string has [n] or less characters , returns the empty string.
      
      Example:
      [String.rchop "Weeble" = "Weebl"]
      [String.rchop ~n:3 "Weeble" = "Wee"]
      [String.rchop ~n:1000 "Weeble" = ""]
*)

val trim : string -> string
  (** Returns the same string but without the leading and trailing
      whitespaces (according to {!BatChar.is_whitespace}). 

      Example: [String.trim " \t foo\n  " = "foo"]
*)

val quote : string -> string
(** Add quotes around a string and escape any quote or escape
    appearing in that string.  This function is used typically when
    you need to generate source code from a string.

    Examples:
    [String.quote "foo" = "\"foo\""]
    [String.quote "\"foo\"" = "\"\\\"foo\\\"\""]
    [String.quote "\n" = "\"\\n\""]
    etc. 

    More precisely, the returned string conforms to the Caml syntax:
    if printed, it outputs a representation of the input string as an
    OCaml string litteral.
*)

val left : string -> int -> string
(**[left r len] returns the string containing the [len] first
   characters of [r]. If [r] contains less than [len] characters, it
   returns [r].

   Examples:
   [String.left "Weeble" 4 = "Weeb"]
   [String.left "Weeble" 0 = ""]
   [String.left "Weeble" 10 = "Weeble"]
*)

val right : string -> int -> string
(**[left r len] returns the string containing the [len] last characters of [r].
   If [r] contains less than [len] characters, it returns [r].

   Example: [String.right "Weeble" 4 = "eble"]
*)

val head : string -> int -> string
(**as {!left}*)

val tail : string -> int -> string
(**[tail r pos] returns the string containing all but the [pos] first characters of [r]

   Example: [String.tail "Weeble" 4 = "le"]
*)

val strip : ?chars:string -> string -> string
  (** Returns the string without the chars if they are at the beginning or
      at the end of the string. By default chars are " \t\r\n".
 
      Examples:
      [String.strip " foo " = "foo"]
      [String.strip ~chars:" ,()" " boo() bar()" = "boo() bar"]
*)

val replace_chars : (char -> string) -> string -> string
  (** [replace_chars f s] returns a string where all chars [c] of [s] have been
      replaced by the string returned by [f c]. 

      Example: [String.replace_chars (function ' ' -> "(space)" | c -> String.of_char c) "foo bar" = "foo(space)bar"]
*)

val replace : str:string -> sub:string -> by:string -> bool * string
  (** [replace ~str ~sub ~by] returns a tuple constisting of a boolean
      and a string where the first occurrence of the string [sub]
      within [str] has been replaced by the string [by]. The boolean
      is true if a subtitution has taken place. 

      Example: [String.replace "foobarbaz" "bar" "rab" = (true, "foorabbaz")]
*)

val repeat: string -> int -> string
(** [repeat s n] returns [s ^ s ^ ... ^ s] 

    Example: [String.repeat "foo" 4 = "foofoofoofoo"]
*)

(** {6 Splitting around}*)

val split : string -> string -> string * string
  (** [split s sep] splits the string [s] between the first
      occurrence of [sep], and returns the two parts before
      and after the occurence (excluded).

      @raise Not_found if the separator is not found. 

      Examples:
      [String.split "abcabcabc" "bc" = ("a","abcabc")]
      [String.split "abcabcabc" "" = ("","abcabcabc")]
*)

val rsplit : string -> string -> string * string
(** [rsplit s sep] splits the string [s] between the last occurrence
    of [sep], and returns the two parts before and after the
    occurence (excluded).

    @raise Not_found if the separator is not found. 

    Example: [String.rsplit "abcabcabc" "bc" = ("abcabca","")]
*)

val nsplit : string -> string -> string list
  (** [nsplit s sep] splits the string [s] into a list of strings
      which are separated by [sep] (excluded).
      [nsplit "" _] returns the empty list. 

      Example: [String.nsplit "abcabcabc" "bc" = ["a"; "a"; "a"; ""]]
*)

val join : string -> string list -> string
  (** Same as {!concat} *)

val slice : ?first:int -> ?last:int -> string -> string
  (** [slice ?first ?last s] returns a "slice" of the string
      which corresponds to the characters [s.[first]],
      [s.[first+1]], ..., [s[last-1]]. Note that the character at
      index [last] is {b not} included! If [first] is omitted it
      defaults to the start of the string, i.e. index 0, and if
      [last] is omitted is defaults to point just past the end of
      [s], i.e. [length s].  Thus, [slice s] is equivalent to
      [copy s].
      
      Negative indexes are interpreted as counting from the end of
      the string. For example, [slice ~last:(-2) s] will return the
      string [s], but without the last two characters.
      
      This function {b never} raises any exceptions. If the
      indexes are out of bounds they are automatically clipped.

      Example: [String.slice ~first:1 ~last:(-3) " foo bar baz" = "foo bar "]
  *)

val splice: string -> int -> int -> string -> string
  (** [String.splice s off len rep] cuts out the section of [s]
      indicated by [off] and [len] and replaces it by [rep] 

      Negative indexes are interpreted as counting from the end
      of the string. If [off+len] is greater than [length s],
      the end of the string is used, regardless of the value of
      [len].

      If [len] is zero or negative, [rep] is inserted at position
      [off] without replacing any of [s].

      Example: [String.splice "foo bar baz" 3 5 "XXX" = "fooXXXbaz"]
   *)

val explode : string -> char list
  (** [explode s] returns the list of characters in the string [s]. 

      Example: [String.explode "foo" = ['f'; 'o'; 'o']]
*)

val implode : char list -> string
  (** [implode cs] returns a string resulting from concatenating
      the characters in the list [cs]. 

      Example: [String.implode ['b'; 'a'; 'r'] = "bar"]
*)

(** {6 Comparisons}*)

val compare: t -> t -> int
  (** The comparison function for strings, with the same specification as
      {!Pervasives.compare}.  Along with the type [t], this function [compare]
      allows the module [String] to be passed as argument to the functors
      {!Set.Make} and {!Map.Make}. 

      Example: [String.compare "FOO" "bar" = -1] i.e. "FOO" < "bar"
*)

val icompare: t -> t -> int
  (** Compare two strings, case-insensitive. 

      Example: [String.icompare "FOO" "bar" = 1] i.e. "foo" > "bar"
*)

module IString : BatInterfaces.OrderedType with type t = t
  (** uses icompare as ordering function 
      
      Example: [module Nameset = Set.Make(String.IString)]
  *)
  

val numeric_compare: t -> t -> int
  (** Compare two strings, sorting "abc32def" before "abc210abc".
      
      Algorithm: Ignore identical prefixes, if first character
      difference is numeric, parse the whole number as an int and
      compare.

      Example: [String.numeric_compare "xx32" "xx210" = -1]
  *)

module NumString : BatInterfaces.OrderedType with type t = t
(** uses numeric_compare as its ordering function

    Example: [module FilenameSet = Set.Make(String.NumString)]
 *)

(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print: 'a BatInnerIO.output -> string -> unit
(**Print a string.

   Example: [String.print stdout "foo\n"]
*)

val println: 'a BatInnerIO.output -> string -> unit
(**Print a string, end the line.

   Example: [String.println stdout "foo"]
*)

val print_quoted: 'a BatInnerIO.output -> string -> unit
(**Print a string, with quotes as added by the [quote] function.

   [String.print_quoted stdout "foo"] prints ["foo"] (with the quotes).

   [String.print_quoted stdout "\"bar\""] prints ["\"bar\""] (with the quotes).

   [String.print_quoted stdout "\n"] prints ["\n"] (not the escaped
   character, but ['\'] then ['n']).
*)

val t_printer : t BatValue_printer.t

val unquoted_printer : t BatValue_printer.t

(**/**)


(** Exceptionless counterparts for error-raising operations *)
module Exceptionless :
sig
  val to_int : string -> int option
  (** Returns the integer represented by the given string or
      [None] if the string does not represent an integer.*)

  val to_float : string -> float option
  (** Returns the float represented by the given string or
      [None] if the string does not represent a float. *)

  val index : string -> char -> int option
  (** [index s c] returns [Some p], the position of the leftmost
      occurrence of character [c] in string [s] or
      [None] if [c] does not occur in [s]. *)

  val rindex : string -> char -> int option
  (** [rindex s c] returns [Some p], the position of the rightmost
      occurrence of character [c] in string [s] or
      [None] if [c] does not occur in [s]. *)

  val index_from : string -> int -> char -> int option
  (** Same as {!String.Exceptionless.index}, but start
      searching at the character position given as second argument.
      [index s c] is equivalent to [index_from s 0 c].*)

  val rindex_from : string -> int -> char -> int option
  (** Same as {!String.Exceptionless.rindex}, but start
      searching at the character position given as second argument.
      [rindex s c] is equivalent to
      [rindex_from s (String.length s - 1) c]. *)

  val find : string -> string -> int option
  (** [find s x] returns [Some i], the starting index of the first
      occurrence of string [x] within string [s], or [None] if [x]
      is not a substring of [s].

      {b Note} This implementation is optimized for short strings. *)

  val find_from : string -> int -> string -> int option
  (** [find_from s ofs x] behaves as [find s x] but starts searching
      at offset [ofs]. [find s x] is equivalent to [find_from s 0 x].*)

  val rfind : string -> string -> int option
  (** [rfind s x] returns [Some i], the starting index of the last occurrence
      of string [x] within string [s], or [None] if [x] is not a substring
      of [s].

      {b Note} This implementation is optimized for short strings. *)

  val rfind_from: string -> int -> string -> int option
  (** [rfind_from s ofs x] behaves as [rfind s x] but starts searching
      at offset [ofs]. [rfind s x] is equivalent to
      [rfind_from s (String.length s - 1) x]. *)

  val split : string -> string -> (string * string) option
  (** [split s sep] splits the string [s] between the first
      occurrence of [sep], or returns [None] if the separator
      is not found. *)

  val rsplit : string -> string -> (string * string) option
  (** [rsplit s sep] splits the string [s] between the last
      occurrence of [sep], or returns [None] if the separator
      is not found. *)

end (* String.Exceptionless *)

(** Capabilities for strings.
    
    This modules provides the same set of features as {!String}, but
    with the added twist that strings can be made read-only or write-only.
    Read-only strings may then be safely shared and distributed.

    There is no loss of performance involved. *)
module Cap:
sig

type 'a t 
(** The type of capability strings.

    If ['a] contains [[`Read]], the contents of the string may be read.
    If ['a] contains [[`Write]], the contents of the string may be written.

    Other (user-defined) capabilities may be added without loss of
    performance or features. For instance, a string could be labelled
    [[`Read | `UTF8]] to state that it contains UTF-8 encoded data and
    may be used only for reading.  Conversely, a string labelled with
    [[]] (i.e. nothing) can neither be read nor written. It can only
    be compared for textual equality using OCaml's built-in [compare]
    or for physical equality using OCaml's built-in [==].
*)

external length : _ t  -> int = "%string_length"

val is_empty : _ t -> bool 

external get : [> `Read] t -> int -> char = "%string_safe_get"

external set : [> `Write] t -> int -> char -> unit = "%string_safe_set"

external create : int -> _ t = "caml_create_string"

(** {6 Constructors}*)

external of_string : string -> _ t                = "%identity"
    (**Adopt a regular string.*)

external to_string : [`Read | `Write] t -> string = "%identity"
    (** Return a capability string as a regular string.*)

external read_only : [> `Read] t -> [`Read] t     = "%identity"
    (** Drop capabilities to read only.*)

external write_only: [> `Write] t -> [`Write] t   = "%identity"
    (** Drop capabilities to write only.*)

val make : int -> char -> _ t

val init : int -> (int -> char) -> _ t

(** {6 Conversions}*)
val enum : [> `Read] t -> char BatEnum.t

val of_enum : char BatEnum.t -> _ t

val backwards : [> `Read] t -> char BatEnum.t
  
val of_backwards : char BatEnum.t -> _ t

val of_list : char list -> _ t

val to_list : [> `Read] t -> char list

val of_int : int -> _ t

val of_float : float -> _ t

val of_char : char -> _ t

val to_int : [> `Read] t -> int

val to_float : [> `Read] t -> float

(** {6 String traversals}*)

val map : (char -> char) -> [>`Read] t -> _ t
  
val fold_left : ('a -> char -> 'a) -> 'a -> [> `Read] t -> 'a

val fold_right : (char -> 'a -> 'a) -> [> `Read] t -> 'a -> 'a

val filter : (char -> bool) -> [> `Read] t -> _ t

val filter_map : (char -> char option) -> [> `Read] t -> _ t


val iter : (char -> unit) -> [> `Read] t -> unit


(** {6 Finding}*)

val index : [>`Read] t -> char -> int

val rindex : [> `Read] t -> char -> int

val index_from : [> `Read] t -> int -> char -> int

val rindex_from : [> `Read] t -> int -> char -> int

val contains : [> `Read] t -> char -> bool

val contains_from : [> `Read] t -> int -> char -> bool

val rcontains_from : [> `Read] t -> int -> char -> bool

val find : [> `Read] t -> [> `Read] t -> int

val find_from: [> `Read] t -> int -> [> `Read] t -> int

val rfind : [> `Read] t -> [> `Read] t -> int

val rfind_from: [> `Read] t -> int -> [> `Read] t -> int

val ends_with : [> `Read] t -> [> `Read] t -> bool

val starts_with : [> `Read] t -> [> `Read] t -> bool

val exists : [> `Read] t -> [> `Read] t -> bool

(** {6 Transformations}*)
  
val lchop : ?n:int -> [> `Read] t -> _ t

val rchop : ?n:int -> [> `Read] t -> _ t

val trim : [> `Read] t -> _ t

val quote : [> `Read] t -> string

val left : [> `Read] t -> int -> _ t

val right : [> `Read] t -> int -> _ t

val head : [> `Read] t -> int -> _ t

val tail : [> `Read] t -> int -> _ t

val strip : ?chars:[> `Read] t -> [> `Read] t -> _ t

val uppercase : [> `Read] t -> _ t

val lowercase : [> `Read] t -> _ t

val capitalize : [> `Read] t -> _ t

val uncapitalize : [> `Read] t -> _ t

val copy : [> `Read] t -> _ t

val sub : [> `Read] t -> int -> int -> _ t

val fill : [> `Write] t -> int -> int -> char -> unit

val blit : [> `Read] t -> int -> [> `Write] t -> int -> int -> unit

val concat : [> `Read] t -> [> `Read] t list -> _ t

val escaped : [> `Read] t -> _ t

val replace_chars : (char -> [> `Read] t) -> [> `Read] t -> _ t

val replace : str:[> `Read] t -> sub:[> `Read] t -> by:[> `Read] t -> bool * _ t

val repeat: [> `Read] t -> int -> _ t

(** {6 Splitting around}*)
val split : [> `Read] t -> [> `Read] t -> _ t * _ t

val rsplit : [> `Read] t -> string -> string * string

val nsplit : [> `Read] t -> [> `Read] t -> _ t list

val splice: [ `Read | `Write] t  -> int -> int -> [> `Read] t -> string

val join : [> `Read] t -> [> `Read] t list -> _ t

val slice : ?first:int -> ?last:int -> [> `Read] t -> _ t

val explode : [> `Read] t -> char list

val implode : char list -> _ t

(** {6 Comparisons}*)

val compare: [> `Read] t -> [> `Read] t -> int

val icompare: [> `Read] t -> [> `Read] t -> int


(** {7 Printing}*)

val print: 'a BatInnerIO.output -> [> `Read] t -> unit

val println: 'a BatInnerIO.output -> [> `Read] t -> unit

val print_quoted: 'a BatInnerIO.output -> [> `Read] t -> unit

val t_printer : [> `Read] t BatValue_printer.t

(**/**)

(** {6 Undocumented operations} *)
external unsafe_get : [> `Read] t -> int -> char = "%string_unsafe_get"
external unsafe_set : [> `Write] t -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  [> `Read] t -> int -> [> `Write] t -> int -> int -> unit = "caml_blit_string" "noalloc"
external unsafe_fill :
  [> `Write] t -> int -> int -> char -> unit = "caml_fill_string" "noalloc"

(**/**)

(** Exceptionless counterparts for error-raising operations *)
module Exceptionless :
sig
  val to_int : [> `Read] t -> int option

  val to_float : [> `Read] t -> float option

  val index : [>`Read] t -> char -> int option

  val rindex : [> `Read] t -> char -> int option

  val index_from : [> `Read] t -> int -> char -> int option

  val rindex_from : [> `Read] t -> int -> char -> int option

  val find : [> `Read] t -> [> `Read] t -> int option

  val find_from: [> `Read] t -> int -> [> `Read] t -> int option

  val rfind : [> `Read] t -> [> `Read] t -> int option

  val rfind_from: [> `Read] t -> int -> [> `Read] t -> int option

  (* val split : string -> string -> (string * string) option TODO *)
  val split : [> `Read] t -> [> `Read] t -> (_ t * _ t) option

(*   val rsplit : string -> string -> (string * string) option TODO *)
  val rsplit : [> `Read] t -> [> `Read] t -> (_ t * _ t) option

end (* String.Cap.Exceptionless *)

end

