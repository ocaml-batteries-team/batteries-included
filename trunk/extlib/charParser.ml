open ParserCo
open ExtString
open ExtChar
open ExtInt
open ExtFloat

(** {6 Entry point} *)
type position =
{
  offset: int;
  line:   int
}
let start_position =
{ offset = 0;
  line   = 0 }

let advance c p =
  if Char.is_newline c then { offset = 0; line = p.line + 1}
  else                      { (p) with offset = p.offset + 1}

let source_of_enum   s = Source.of_enum s start_position advance

let source_of_string s = source_of_enum (String.enum s)

let parse_string p s =
  run p (source_of_string s)

(** {6 Utilities}*)
let char   c = label ("\"" ^ String.of_char c ^ "\"") (exactly c)

let string s = label ("\"" ^ s ^ "\"") (
  let len = String.length s in
  let rec aux i = 
    if i < len then exactly s.[i] >>= fun _ -> aux (i+1)
    else return s
  in aux 0
)

let case_char c =
  if Char.is_letter c then one_of [Char.uppercase c; Char.lowercase c]
  else char c

let case_string s = label ("case insensitive \"" ^ s ^ "\"") (
  let s' = String.lowercase s in
  let len = String.length s'  in
  let rec aux i = 
    if i < len then exactly s'.[i] >>= fun _ -> aux (i+1)
    else return s
  in aux 0
)   

let whitespace = sat Char.is_whitespace

let uppercase = label "upper case char" (satisfy Char.is_uppercase)
let lowercase = label "lower case char" (satisfy Char.is_lowercase)
let letter    = label "letter" (satisfy Char.is_letter)

let uppercase_latin1   = label "upper case char (possibly accentuated)"
  ( satisfy Char.is_uppercase_latin1 )

let lowercase_latin1   = label "lower case char (possibly accentuated)"  
  ( satisfy Char.is_lowercase_latin1 )
let latin1    = label "letter (possibly accentuated)" (satisfy Char.is_latin1)

let digit = label "digit"
  ( satisfy Char.is_digit)

let hex = label "hex"
  ( satisfy (fun x -> ( '0' <= x && x <= '9' ) || ('a' <= x && x <= 'f') || ('A' <= x && x <= 'F')))

let first s = String.get s 0

let not_char c = label ("anything but '" ^ String.of_char c ^ "'")
  (satisfy (fun x -> x <> c))

let none_of l = label (
  String.of_list (Vect.to_list (Vect.append ']'
    (List.fold_left (fun acc x -> Vect.append x acc)
       (Vect.of_list (String.to_list "anything but ['"))
       l))))
  (none_of l)

let newline = sat Char.is_newline

