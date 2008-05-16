open ParserCo
(** Parsing enumerations of characters*)

val parse_string : (char, 'a) t -> string -> ('a, failure) Std.result

(**{6 Utilities}*)

val char : char -> (char, char) t
  (** Recognize exactly one char*)

val string : string -> (char, string) t
  (** Recognize exactly one string*)

val whitespaces : (char, unit) t
  (**Recognizes a (possibly empty) sequence of white-spaces*)

val uppercase : (char, char) t
  (**Recognizes one upper-case ASCII character, including
     accentuated characters.*)

val lowercase : (char, char) t
  (**Recognizes one lower-case ASCII character, including
     accentuated characters.*)

val uppercase_latin1 : (char, char) t  (*@TODO: test*)
  (**Recognizes one upper-case Latin-1 character, including
     accentuated characters.*)

val lowercase_latin1 : (char, char) t  (*@TODO: test*)
  (**Recognizes one lower-case Latin-1 character, including
     accentuated characters.*)

val digit : (char, char) t
  (**Recognizes one digit*)

(** {6 OCaml-oriented stuff}*)

val first_identifier_char : (char, char) t
  (**Recognizes one character suitable for appearing as 
     the first character of an identfier (i.e. any letter or '_')*)

val later_identifier_char : (char, char) t
  (**Recognizes one character suitable for appearing as a non-first
     character of an identifier (i.e. any letter, '_', a digit or "'")*)


val identifier : (char, string) t
  (**Recognizes one identifier, with OCaml conventions*)

val lower_identifier: (char, string) t
  (**Recognizes one, with OCaml conventions, starting with lower-case*)

val capitalized_identifier: (char, string) t
  (**Recognizes one identifier starting with an upper-case, with OCaml conventions*)

val ocaml_comment: (char, unit) t
  (**Recognizes one OCaml-style comment, starting with [(*] and ending with 
     [*)], with possible comments nested in these comments. *)

val ocaml_symbol: (char, string) t
  (**Recognizes an OCaml-style symbol, that is a sequence of ['!'],
     ['%'], ['&'], ['$'], ['#'], ['+'], ['/'], [':'], ['<'], ['='],
     ['>'], ['?'], ['@'], ['\\'], ['~'], ['^'], ['|'], ['*']*)

val ocaml_char: (char, char) t
  (**Recognizes an OCaml-style char, that is one character surrounded
     by single quotes (or something more complicated in case of escape)*)

val ocaml_string: (char, string) t
  (**Recognizes an OCaml-style string, that is a sequence of characters
     surrounded by double quotes (or something more complicated in case
     of escape)*)

val integer: (char, int) t
  (**Recognizes one OCaml-style integer, possibly prefixed with [-]*)

val float: (char, float) t
  (**Recognizes one OCaml-tyle float, possibly prefixed with [-]
     and postfixed with an exponent*)

(** {6 C/C++-oriented stuff}*)


val c_comment: (char, unit) t
  (**Recognizes one C-style comment, starting with [//] and ending with a newline*)

val cpp_comment_only: (char, unit) t
  (**Recognizes one C++-style comment, starting with [/*] and ending with [*/]*)

val cpp_comment: (char, unit) t
  (**Recognizes one C++-style comment, starting with [//] or
     one starting with [/*]*)

