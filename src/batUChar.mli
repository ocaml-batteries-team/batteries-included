(* $Id: uChar.mli,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* TODO: Check -- this is actually part of a package distributed with LGPL + linking exception *)

(** Unicode (ISO-UCS) characters.
    
    This module implements Unicode (actually ISO-UCS) characters.  All
    31-bit code points are allowed.

    {b Note} For conversions to lower/upercase, see modules {!UTF8} and {!Rope}.

    @author Yamagata Yoriyuki (Camomile module)
    @author Edgar Friendly
    @author David Teller

    @documents UChar
*)

open BatCamomile.UChar

val is_whitespace : t -> bool
(** Determine if a character is a whitespace.

    A character is a whitespace if it is either one of the ASCII
    whitespaces (characters 9, 10, 12, 13, 26 and 32), or a part of
    Unicode category [Z] (separators). *)

val is_uppercase : t -> bool
  (** Determine if a character is an uppercase character.*)

val is_lowercase : t -> bool
(** Determine if a character is an lowercase character.*)

val is_newline : t -> bool
  (** Determine if a character is a newline.  Newline characters are
      characters 10 ('\r'), 13 (\'n') and characters which are part of
      Unicode category [`Zl] *)

val lowercase : t -> t
  (** Convert the given character to its equivalent lowercase
      character. *)

val uppercase : t -> t
  (** Convert the given character to its equivalent uppercase
      character. *)

(**
   {6 Conversion functions}
*)

val to_char : t -> char
  (** [to_char u] returns the Latin-1 representation of [u].  

      @raise Out_of_range if [u] can not be represented by Latin-1.*)

val of_digit: int -> t
(** Return the character representing a given digit.
    
    @raise Invalid_argument "UChar.of_digit" if the
    argument is outside the range 0--9*)

(** Alias of [uint_code] *)
val to_int : t   -> int


(**
   {6 Comparaison}
*)

val icompare : t -> t -> int
  (** Compare two unicode characters, case-insensitive. *)

module IUChar : BatInterfaces.OrderedType with type t = t

(** {6 Detailed information}*)

(** Determine to which script this character belongs.*)

type script = 
 [ `Arabic
       | `Armenian
       | `Bengali
       | `Bopomofo
       | `Buhid
       | `Canadian_Aboriginal
       | `Cherokee
       | `Common
       | `Cyrillic
       | `Deseret
       | `Devanagari
       | `Ethiopic
       | `Georgian
       | `Gothic
       | `Greek
       | `Gujarati
       | `Gurmukhi
       | `Han
       | `Hangul
       | `Hanunoo
       | `Hebrew
       | `Hiragana
       | `Inherited
       | `Kannada
       | `Katakana
       | `Khmer
       | `Lao
       | `Latin
       | `Malayalam
       | `Mongolian
       | `Myanmar
       | `Ogham
       | `Old_Italic
       | `Oriya
       | `Runic
       | `Sinhala
       | `Syriac
       | `Tagalog
       | `Tagbanwa
       | `Tamil
       | `Telugu
       | `Thaana
       | `Thai
       | `Tibetan
       | `Yi ] 
val script : t -> script

type category =
  [ `Lu         (** Letter, Uppercase *)
  | `Ll         (** Letter, Lowercase *)
  | `Lt         (** Letter, Titlecase *)
  | `Mn         (** Mark, Non-Spacing *)
  | `Mc         (** Mark, Spacing Combining *)
  | `Me         (** Mark, Enclosing *)
  | `Nd         (** Number, Decimal Digit *)
  | `Nl         (** Number, Letter *)
  | `No         (** Number, Other *)
  | `Zs         (** Separator, Space *)
  | `Zl         (** Separator, Line *)
  | `Zp         (** Separator, Paragraph *)
  | `Cc         (** Other, Control *)
  | `Cf         (** Other, Format *)
  | `Cs         (** Other, Surrogate *)
  | `Co         (** Other, Private Use *)
  | `Cn         (** Other, Not Assigned *)
  | `Lm         (** Letter, Modifier *)
  | `Lo         (** Letter, Other *)
  | `Pc         (** Punctuation, Connector *)
  | `Pd         (** Punctuation, Dash *)
  | `Ps         (** Punctuation, Open *)
  | `Pe         (** Punctuation, Close *)
  | `Pi         (** Punctuation, Initial quote  *)
  | `Pf         (** Punctuation, Final quote  *)
  | `Po         (** Punctuation, Other *)
  | `Sm         (** Symbol, Math *)
  | `Sc         (** Symbol, Currency *)
  | `Sk         (** Symbol, Modifier *)
  | `So         (** Symbol, Other *) ]

(**Determine to which category the character belongs.*)
val category : t -> category

(** {6 Boilerplate code}*)

(** {7 Printing}*)
val print: 'a BatInnerIO.output -> t -> unit
val t_printer : t BatValue_printer.t




