open ParserCo
(** Parsing character strings *)

(** The position inside one file or one stream. *)
type position =
{
  offset: int;(**Offset on the line (starting at 0)*)
  line:   int (**Line number (starting at 0)*)
}

val advance : char -> position -> position

val source_of_string : string      -> (char, position) Source.t
val source_of_enum   : char Enum.t -> (char, position) Source.t

val parse_string : (char, 'a, position) t -> string -> ('a, position report) Std.result

val parse_enum : (char, 'a, position) t -> char Enum.t -> ('a, position report * position * string list * string) Std.result
(**{6 Utilities}*)

val char : char -> (char, char, position) t
  (** Recognize exactly one char*)

val none_of : char list -> (char, char, position) t
  (**Accept any value not in a list
     As [ParserCo.none_of], just with improved error message.*)

val not_char : char -> (char, char, position) t
  (**Accept any value not a given char
     As [none_of]. *)

val string : string -> (char, string, position) t
  (** Recognize exactly one string*)

val case_char : char -> (char, char, position) t
  (** As [char], but case-insensitive *)

val case_string : string -> (char, string, position) t
  (** As [case_string], but case-insensitive *)

val newline : (char, char, position) t
  (**Recognizes a newline*)

val whitespace : (char, char, position) t
  (**Recognizes white-space*)

val uppercase : (char, char, position) t
  (**Recognizes one upper-case ASCII character, including
     accentuated characters.*)

val lowercase : (char, char, position) t
  (**Recognizes one lower-case ASCII character, including
     accentuated characters.*)

val letter: (char, char, position) t
  (**Recognizes one lower- or upper-case ASCII character, including
     accentuated characters.*)

val uppercase_latin1 : (char, char, position) t  (*@TODO: test*)
  (**Recognizes one upper-case Latin-1 character, including
     accentuated characters.*)

val lowercase_latin1 : (char, char, position) t  (*@TODO: test*)
  (**Recognizes one lower-case Latin-1 character, including
     accentuated characters.*)

val latin1: (char, char, position) t
  (**Recognizes one lower- or upper-case Latin1 character, including
     accentuated characters.*)

val digit : (char, char, position) t
  (**Recognizes one decimal digit*)

val hex : (char, char, position) t
  (**Recognizes one hexadecimal digit (case-insensitive)*)

