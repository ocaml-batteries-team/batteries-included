open ParserCo
open ExtString
open ExtChar
open ExtInt
open ExtFloat

let parse_string p s =
  run p '\n' (String.enum s)

let char   c = label ("\"" ^ String.of_char c ^ "\"") (exactly c)

let string s = label ("\"" ^ s ^ "\"") (
  let len = String.length s in
  let rec aux i = 
    if i < len then exactly s.[i] >>= fun _ -> aux (i+1)
    else return s
  in aux 0
)

let whitespaces = zero_plus (satisfy Char.is_whitespace) >>= fun _ -> return ()

let uppercase : (char, char) t         = label "upper case char" (satisfy Char.is_uppercase)
let lowercase : (char, char) t         = label "lower case char" (satisfy Char.is_lowercase)

let uppercase_latin1   = label "upper case char (possibly accentuated)"
  ( satisfy Char.is_uppercase_latin1 )

let lowercase_latin1   = label "lower case char (possibly accentuated)"  
  ( satisfy Char.is_lowercase_latin1 )

let digit : (char,char) t = label "digit"
  ( satisfy Char.is_digit)

let hex = label "hex"
  ( satisfy (fun x -> ( '0' <= x && x <= '9' ) || ('a' <= x && x <= 'f') || ('A' <= x && x <= 'F')))

let first_identifier_char : (char, char) t = either [uppercase; lowercase; (satisfy ( ( = ) '_' ))]

let later_identifier_char = first_identifier_char <|> digit <|> (char '\'')

let identifier = map String.of_list (first_identifier_char >:: zero_plus later_identifier_char) 

let first s = String.get s 0

let lower_identifier       = filter (fun s -> Char.is_lowercase (first s)) identifier 
let capitalized_identifier = filter (fun s -> Char.is_uppercase (first s)) identifier 

let lpar = Vect.of_list ['('; '*']
let rpar = Vect.of_list ['*'; ')']

let not_char c = label ("anything but '" ^ String.of_char c ^ "'")
  (satisfy (fun x -> x <> c))

let discard : 'a -> (_ -> 'a) = fun x -> (fun _ -> x)

let ocaml_comment = 
  string "(*" >>= fun _ ->
    let rec content () = 
      any         >>= function
	| '(' -> ( any >>= function
		     | '*' -> content () >>= discard (content ())
		     | _   -> content () )
	| '*' -> ( any >>= function
		     | ')' -> return ()
		     | _   -> content() )
	|  _  -> content ()
    in content ()

let ocaml_symbol = label "OCaml-style symbol" (one_plus (satisfy (Char.is_symbol))) >>= fun s ->
  return (String.of_list s)

let discarded x _ = x
    
let ocaml_escape = label "OCaml-style escaped character" 
  (
    any >>= function
      | 'n' -> return '\n'
      | 'r' -> return '\r'
      | 't' -> return '\t'
      | '\\'-> return '\\'
      | 'b' -> return '\b'
      | '"' -> Printf.eprintf "[ESCAPE] \""; return '"'
      | 'x' -> 
	  times 2 hex >>= fun t ->
	  return (Char.chr (Int.of_string (String.implode ('0'::'x'::t))))
      | '0' .. '9' as x -> 
	  times 2 digit >>= fun t ->
	  return (Char.chr (Int.of_string (String.implode (x::t))))
      | _ -> fail
  )



let ocaml_char = label "OCaml-style single character" 
  (char '\'' >>= fun _ ->
   any       >>= function
     | '\\' -> ocaml_escape
     | c    -> return c
  ) >>= fun c -> 
    char '\'' >>= fun _ -> return c

let ocaml_string =
  char '"' >>= fun _ ->
    let rec content chars =
      any >>= function
	| '"'  -> 
	    return chars
	| '\\' -> 
	    ocaml_escape >>= fun e -> 
	    content (e::chars)
        | e    -> 
	    content (e::chars)
    in content [] >>= fun c -> 
      return (String.of_list (List.rev c))
	  
let newline = satisfy Char.is_newline

let c_comment =
  string "//"               >>= fun _ ->
  zero_plus (not_char '\n') >>= fun _ ->
  newline                   >>= fun _ -> return ()

let cpp_comment_only =
  string "/*" >>= fun _ ->
    let rec content () =
      any >>= function
	| '*' -> ( any >>= function
		     | '/' -> return ()
		     | _   -> content () )
	| _   -> content ()
    in content ()


let cpp_comment =
  c_comment <|> cpp_comment_only


let integer = 
  maybe (char '-') >>= fun sign   ->
  one_plus digit   >>= fun digits -> 
    let number = Int.of_string (String.of_list digits) in
      match sign with
	| Some _ -> return (~- number)
	| None   -> return number


let float =
  maybe (char '-')                         >>= fun sign     ->
  map String.of_list (zero_plus digit)     >>= fun int_part ->
  maybe (
    char '.'  >>= fun _ -> 
    map String.of_list (zero_plus digit) ) >>= fun decimal_part ->
  maybe (
    char 'E'  >>= fun _ -> 
    maybe (char '+' <|> char '-') >>= fun sign ->
    let sign = Option.default '+' sign  in
    one_plus digit >>= fun expo -> 
      return ("E" ^ (String.of_char sign) ^ (String.of_list expo)))
                                           >>= fun expo ->

  let number = match (decimal_part, expo) with
    | Some d, Some e -> Some (int_part ^ "." ^ d ^ e)
    | Some d, None   -> Some (int_part ^ "." ^ d)
    | None,   Some e -> Some (int_part ^ e )
    | None,   None   -> None
  in match number with
    | None   -> fail
    | Some n -> let absolute = Float.of_string n in
	return (
	match sign with
	  | None   -> absolute
	  | Some _ -> ~-. absolute)

