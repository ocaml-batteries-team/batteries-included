open ParserCo
(** Parsing character strings *)

val parse_string : (char * ParserCo.loc, 'a) t -> string -> ('a, ParserCo.failure) Std.result

(**{6 Utilities}*)

val char : char -> (char, char) t
  (** Recognize exactly one char*)

val none_of : char list -> (char, char) t
  (**Accept any value not in a list
     As [ParserCo.none_of], just with improved error message.*)

val not_char : char -> (char, char) t
  (**Accept any value not a given char
     As [none_of]. *)

val string : string -> (char, string) t
  (** Recognize exactly one string*)

val case_char : char -> (char, char) t
  (** As [char], but case-insensitive *)

val case_string : string -> (char, string) t
  (** As [case_string], but case-insensitive *)

val newline : (char, unit) t
  (**Recognizes a newline*)

val whitespace : (char, unit) t
  (**Recognizes white-space*)

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
  (**Recognizes one decimal digit*)

val hex : (char, char) t
  (**Recognizes one hexadecimal digit (case-insensitive)*)

