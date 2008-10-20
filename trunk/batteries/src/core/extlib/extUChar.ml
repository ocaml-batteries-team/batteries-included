(* $Id: uChar.ml,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* TODO: Check -- this is actually part of a package distributed with LGPL + linking exception *)

TYPE_CONV_PATH "Batteries.Data.Text" (*For Sexplib, Bin-prot...*)

module UChar =
struct
  open CamomileLibrary
  include UChar

  module Info = UCharInfo.Make(CamomileDefaultConfig)


  let backslash = uint_code (of_char '\\')

    (*The digits, precomputed.*)
  let zero      = (of_char '0')
  let one       = (of_char '1')
  let two       = (of_char '2')
  let three     = (of_char '3')
  let four      = (of_char '4')
  let five      = (of_char '5')
  let six       = (of_char '6')
  let seven     = (of_char '7')
  let eight     = (of_char '8')
  let nine      = (of_char '9')



  let to_char = char_of
  let to_int  = int_of

  let sexp_of_t t = Sexplib.Conv.sexp_of_int (to_int t)
  let t_of_sexp s = of_int (Sexplib.Conv.int_of_sexp s)

  let is_lowercase  c = match Info.general_category c with `Ll -> true | _ -> false
  let is_uppercase  c = match Info.general_category c with `Lu -> true | _ -> false
  let is_whitespace c = match uint_code c with
    | 010 | 013 | 009 | 026 | 032 | 012 -> true
    | _                                 -> match Info.general_category c with `Zs | `Zl | `Zp -> true
	                                                                    |            _    -> false
  let is_newline    c = match uint_code c with
    | 010 | 013 -> true
    | _         -> match Info.general_category c with `Zl -> true
	                                          |    _  -> false

  let print out c = 
    let code = uint_code c in
      if code = backslash then InnerIO.nwrite out "\\\\"
      else if code < 0x80 then 
	InnerIO.nwrite out (UTF8.init 1 (fun _ -> c)) (*Note [UTF8.t] is [string] -- not [ExtUTF8.t]*)
      else
	let n2 = code land 0xffff in
	let n1 = code lsr 16 in
	  if n1 = 0 then 
	    InnerIO.Printf.fprintf out "\\u%04X" n2
	  else
	    InnerIO.Printf.fprintf out "\\U%04X%04X" n1 n2
	      
  let of_digit = function
    | 0 -> zero
    | 1 -> one
    | 2 -> two
    | 3 -> three
    | 4 -> four
    | 5 -> five
    | 6 -> six
    | 7 -> seven
    | 8 -> eight
    | 9 -> nine
    | _ -> raise (Invalid_argument "UChar.of_digit")


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
      
      
let script     = Info.script
let category   = Info.general_category


end
