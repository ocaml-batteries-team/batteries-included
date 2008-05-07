open Std

type offset =
  | Eof 
  | Offset of int

type failure =
    {
      labels : string list;
      offset : offset
    }

type 'a source = ('a * int) LazyList.t

type ('a, 'b) t = 'a source -> (('b * 'a source), failure) result

let ( >>= ) = bind


open ExtChar
open ExtString
open ExtInt


let get_offset e =
  match Lazy.force e with
    | Nil              -> Eof
    | Cons ((_, i), _) -> Offset i

(* Primitives *)
let satisfy f e =
  match Lazy.force e with
    | Nil                     -> fail Eof
    | Cons ((x,_),t) when f x -> Ok (x, t)
    | Cons ((_,i),_)          -> fail (Offset i)

let label s p e =
  match p e with
    | Ok    _ as x -> x
    | Error {offset = o} -> Error {offset = o ; labels = [s] }

let either l e =
  let rec aux err = function
  | []   -> { offset = get_offset e; labels = err }
  | h::t -> 
      match h e with
	| Ok _ as x               -> x
	| Error {labels = labels} -> aux (err @ labels) 
  in aux []

let ( <|> ) p1 p2 = either [p1;p2]

let maybe p e = 
  match p e with
    | Ok (result, rest) -> Ok (Some result, rest)
    | Error _           -> Ok (None,        e   )

let bind m f e =
  match m e with
    | Ok (result, rest) -> f result rest
    | Error _ as e      -> e

let eof e =
  match Lazy.force e with
    | Nil           -> Ok     ((), nil)
    | Cons (_, i)   -> Error  {labels = "end"; offset = Offset i}

let any e = 
  match Lazy.force e with
    | Nil             -> Error  {label = "anything"; offset = Eof}
    | Cons ((x,_), t) -> Ok     (x,t)

let return r e = Ok (r, e)

let condition f e p =
  match f e with
    | Error _ as err -> err
    | Ok (x,t)as succ -> if p x then succ else Error { label = []; offset = get_offset e}

let exactly x =
  satisfy (( = ) x)

let zero_plus p e =
  let rec aux e acc = match p e with
    | Error _ -> Ok (List.rev acc, e)
    | Ok (x,t)-> aux t x::acc
  in aux e []

let one_plus p e =
  match p e with
    | Error _ as err -> err
    | Ok (x, t)      -> match zero_plus p t with
	| Error _   -> assert false (*zero_plus always succeds*)
	| Ok(xs,t') -> Ok(x::xs, t')

let filter p f e =
  match p e with
    | Error e -> Error e
    | Ok(x,t) -> Ok(f x,t)

let times n p e =
  let rec aux acc i = if i > 0 then p >>= fun x -> (aux (x::acc) ( i - 1 ))
	   else return acc
  in filter (aux [] n) List.rev



(* Examples *)

let char   c = label ("\"" ^ String.of_char c ^ "\"") (exactly c)

let string s = label ("\"" ^ s ^ "\"") (
  let len = String.length s in
  let rec aux i = (exactly s.(i)) >>= (discarding (if i < len then aux (i+1) else return ()))
  in aux 0
)

let whitespaces = zero_or_plus (satisfy Char.is_whitespace)

let uppercase          = label "upper case char" (satisfy Char.is_uppercase)
let lowercase          = label "lower case char" (satisfy Char.is_lowercase)

let uppercase_latin1   = label "upper case char (possibly accentuated)"
  ( satisfy Char.is_uppercase_latin1 )

let lower_case_latin1   = label "lower case char (possibly accentuated)"  
  ( satisfy Char.is_lowercase_latin1 )

let digit = label "digit"
  ( satisfy Char.is_digit)

let identifier_char = upper_case <|> lower_case <|> satisfy ( ( = ) '_' )

let identifier = filter (one_or_plus identifier_char) Vect.of_list

let first s = Vect.at 0 s

let lower_identifier = condition identifier (fun s -> Char.is_lowercase (first s))
let capitalized_identifier = condition identifier (fun s -> Char.is_uppercase (first s))

let lpar = Vect.of_list ['('; '*']
let rpar = Vect.of_list ['*'; ')']

let ocaml_comment =
  let rec aux = 
      (string "(*") >>= discarded
      (zero_or_plus (aux <|> (anything_but (string "*)")))) >>= (fun x ->
      (string "*)") >>= discarded
	Vect.postpend rpar (Vect.prepend  lpar x))
  in aux

let ocaml_symbol = label "OCaml-style symbol" (satisfy (Char.is_symbol))

let discarded x _ = x
    
let ocaml_escape = label "OCaml-style escaped character" 
  (
        (char 'n'      >>= discarded (return '\n'))
    <|> (char 'r'      >>= discarded (return '\r'))
    <|> (char 't'      >>= discarded (return '\t'))
    <|> (char '\\'     >>= discarded (return '\\'))
    <|> (times 3 digit >>= (fun l -> return Char.chr (Int.of_string (implode l))))
  )



let ocaml_char = label "OCaml-style single character" 
  char '\'' >>=
  (
         (char '\\' discarded ocaml_escape)
     <|> (any x)
  )
	  
let ocaml_string = 
  let rec aux = 
    zero_or_plus (anything_but ((char '\'') <|> (char '"'))) >>= 
      ( fun l ->
	  let vect = Vect.of_list l in					    
	    maybe (char '\'' >>= discarded ocaml_escape) >>= function
	      | None   -> return vect
	      | Some c -> aux >>= 
		  (fun rest -> 
		     return (Vect.postpend (Vect.postpend (Vect.singleton c) l )) rest) )
  in
  label "OCaml-style double-quoted string"
    (char '"'    >>= discarded (
     discarded aux  >>= (fun result ->
     char '"'    >>= discarded result)))

let newline = satisfy Char.is_newline

let c_comment =
  string "//" >>= discarded (
  zero_or_plus (anything_but (char '\n')) >>= fun result ->
  newline     >>= discarded return result  )

let cpp_comment_only =
  string "/*" >>= discarded (
  zero_or_plus (anything_but (string "*/")) >>= (fun result ->
  string "*/" >>= discarded (return (Vect.of_list result))))

let cpp_comment =
  c_comment <|> cpp_comment

let integer = 
  let digits =
    one_or_plus digit >>= (fun r -> Int.of_string (String.of_list r))
  in
       (char '-' >>= discarded (
        digits   >>= fun r -> ~- r ) )
    <|> digits

let float =
  let digits   = String.of_list (zero_plus digit)
  let exponent = char 'E'                      >>= (discarding ->
                 maybe (char '+' <|> char '-') >>= (fun sign   -> let sign = default sign '+' in
                 digits
		 
  in
  digits  >>=   (fun i -> 
  (maybe (char '.' >>= discarded digits)) >>= (fun decimal ->
  (maybe exponent) >>= (fun expo ->
     let number = match (decimal, expo) with
       | Some d, Some e -> Some (i ^ "." ^ d ^ e)
       | Some d, None   -> Some (i ^ "." ^ d)
       | None,   Some e -> Some (i ^ e )
       | None,   None   -> None
     in match number with
       | None -> fail
       | Some n -> return (Float.of_string number))))


