open BatInnerPervasives
open BatParserCo
open BatCharParser

include Genlex


let string_of_token = function
  | Kwd s -> Printf.sprintf "Kwd %S" s
  | Ident s -> Printf.sprintf "Ident %S" s
  | Int i -> Printf.sprintf "Int %d" i
  | Float f -> Printf.sprintf "Float %f" f
  | String s -> Printf.sprintf "String %S" s
  | Char c -> Printf.sprintf "Char %C" c

type lexer_error =
  | IllegalCharacter of char
  | NotReallyAChar
  | NotReallyAnEscape
  | EndOfStream

exception LexerError of lexer_error * int
exception EarlyEndOfStream

type enum =
  {
    mutable position : int;
    content  : char BatEnum.t
  }

let junk e =
  e.position <- e.position + 1;
  BatEnum.junk e.content

let peek e =
  BatEnum.peek e.content

type t = (string, token) Hashtbl.t

let of_list x =
  let kwd_table = Hashtbl.create (List.length x) in
  List.iter (fun s -> Hashtbl.add kwd_table s (Kwd s)) x;
  kwd_table

let to_enum_filter kwd_table =
  let initial_buffer = String.create 32 in

  let buffer = ref initial_buffer       in
  let bufpos = ref 0                    in

  let reset_buffer () = buffer := initial_buffer; bufpos := 0 in

  let store c =
    if !bufpos >= String.length !buffer then
      begin
        let newbuffer = String.create (2 * !bufpos) in
        String.blit !buffer 0 newbuffer 0 !bufpos; buffer := newbuffer
      end;
    String.set !buffer !bufpos c;
    incr bufpos in

  let get_string () =
    let s = String.sub !buffer 0 !bufpos in buffer := initial_buffer; s
  in
  let ident_or_keyword id =
    try Hashtbl.find kwd_table id with
      Not_found -> Ident id
  and keyword_or_error c pos =
    let s = BatString.of_char c in
    try Hashtbl.find kwd_table s with
      Not_found -> raise (LexerError (IllegalCharacter c, pos))
  in

  let rec next_token (enum : enum)  =
    match peek enum with
      Some (' ' | '\010' | '\013' | '\009' | '\026' | '\012') ->
      junk enum; next_token enum
    | Some ('A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' as c) ->
      junk enum;
      let s = enum in reset_buffer (); store c; ident s
    | Some
        ('!' | '%' | '&' | '$' | '#' | '+' | '/' | ':' | '<' | '=' | '>' |
         '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
      junk enum;
      let s = enum in reset_buffer (); store c; ident2 s
    | Some ('0'..'9' as c) ->
      junk enum;
      let s = enum in reset_buffer (); store c; number s
    | Some '\'' ->
      junk enum;
      let c =
        try char enum with
          EarlyEndOfStream -> raise (LexerError (NotReallyAChar, enum.position))
      in
      begin match peek enum with
          Some '\'' -> junk enum; Some (Char c)
        | None      -> raise EarlyEndOfStream
        | _ -> raise (LexerError (NotReallyAChar, enum.position))
      end
    | Some '"' ->
      junk enum;
      let s = enum in reset_buffer (); Some (String (string s))
    | Some '-' -> junk enum; neg_number enum
    | Some '(' -> junk enum; maybe_comment enum
    | Some c -> junk enum; Some (keyword_or_error c enum.position)
    | _ -> None
  and ident (enum : enum) =
    match peek enum with
      Some
        ('A'..'Z' | 'a'..'z' | '\192'..'\255' | '0'..'9' | '_' | '\'' as c) ->
      junk enum; let s = enum in store c; ident s
    | _ -> Some (ident_or_keyword (get_string ()))
  and ident2 (enum : enum) =
    match peek enum with
      Some
        ('!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '=' |
         '>' | '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
      junk enum; let s = enum in store c; ident2 s
    | _ -> Some (ident_or_keyword (get_string ()))
  and neg_number (enum : enum) =
    match peek enum with
      Some ('0'..'9' as c) ->
      junk enum;
      let s = enum in reset_buffer (); store '-'; store c; number s
    | _ -> let s = enum in reset_buffer (); store '-'; ident2 s
  and number (enum : enum) =
    match peek enum with
      Some ('0'..'9' as c) ->
      junk enum; let s = enum in store c; number s
    | Some '.' ->
      junk enum; let s = enum in store '.'; decimal_part s
    | Some ('e' | 'E') ->
      junk enum; let s = enum in store 'E'; exponent_part s
    | _ -> Some (Int (int_of_string (get_string ())))
  and decimal_part (enum : enum) =
    match peek enum with
      Some ('0'..'9' as c) ->
      junk enum; let s = enum in store c; decimal_part s
    | Some ('e' | 'E') ->
      junk enum; let s = enum in store 'E'; exponent_part s
    | _ -> Some (Float (float_of_string (get_string ())))
  and exponent_part (enum : enum) =
    match peek enum with
      Some ('+' | '-' as c) ->
      junk enum; let s = enum in store c; end_exponent_part s
    | _ -> end_exponent_part enum
  and end_exponent_part (enum : enum) =
    match peek enum with
      Some ('0'..'9' as c) ->
      junk enum; let s = enum in store c; end_exponent_part s
    | _ -> Some (Float (float_of_string (get_string ())))
  and string (enum : enum) =
    match peek enum with
      Some '"' -> junk enum; get_string ()
    | Some '\\' ->
      junk enum;
      let c =
        try escape enum with
          EarlyEndOfStream -> raise (LexerError (NotReallyAnEscape, enum.position))
      in
      let s = enum in store c; string s
    | Some c -> junk enum; let s = enum in store c; string s
    | _ -> raise EarlyEndOfStream
  and char (enum : enum) =
    match peek enum with
      Some '\\' ->
      junk enum;
      begin try escape enum with
        | EarlyEndOfStream -> raise (LexerError(NotReallyAChar, enum.position))
      end
    | Some c -> junk enum; c
    | _ -> raise EarlyEndOfStream
  and escape (enum : enum) =
    match peek enum with
      Some 'n' -> junk enum; '\n'
    | Some 'r' -> junk enum; '\r'
    | Some 't' -> junk enum; '\t'
    | Some ('0'..'9' as c1) ->
      junk enum;
      begin match peek enum with
          Some ('0'..'9' as c2) ->
          junk enum;
          begin match peek enum with
              Some ('0'..'9' as c3) ->
              junk enum;
              Char.chr
                ((Char.code c1 - 48) * 100 + (Char.code c2 - 48) * 10 +
                   (Char.code c3 - 48))
            | Some _ -> raise (LexerError(NotReallyAnEscape, enum.position))
            | None   -> raise EarlyEndOfStream
          end
        | Some _ -> raise (LexerError(NotReallyAnEscape, enum.position))
        | _      -> raise EarlyEndOfStream
      end
    | Some c -> junk enum; c
    | _ -> raise EarlyEndOfStream
  and maybe_comment (enum : enum) =
    match peek enum with
      Some '*' -> junk enum; let s = enum in comment s; next_token s
    | _ -> Some (keyword_or_error '(' enum.position)
  and comment (enum : enum) =
    match peek enum with
      Some '(' -> junk enum; maybe_nested_comment enum
    | Some '*' -> junk enum; maybe_end_comment enum
    | Some _   -> junk enum; comment enum
    | _        -> raise EarlyEndOfStream
  and maybe_nested_comment (enum : enum) =
    match peek enum with
      Some '*' -> junk enum; let s = enum in comment s; comment s
    | Some _   -> junk enum; comment enum
    | _        -> raise EarlyEndOfStream
  and maybe_end_comment (enum : enum) =
    match peek enum with
      Some ')' -> junk enum; ()
    | Some '*' -> junk enum; maybe_end_comment enum
    | Some _   -> junk enum; comment enum
    | _        -> raise EarlyEndOfStream
  in
  fun input -> BatEnum.from_while (fun _count -> next_token {position = 0; content = input})


let to_stream_filter (kwd_table:t) (x:char Stream.t) : token Stream.t =
  (BatStream.of_enum (to_enum_filter kwd_table (BatStream.enum x)))

let to_lazy_list_filter kwd_table x =
  (BatLazyList.of_enum (to_enum_filter kwd_table (BatLazyList.enum x)))



let ocaml_escape = label "OCaml-style escaped character"
    (
      any >>= function
      | 'n' -> return '\n'
      | 'r' -> return '\r'
      | 't' -> return '\t'
      | '\\'-> return '\\'
      | 'b' -> return '\b'
      | '"' -> return '"'
      | 'x' ->
        times 2 hex >>= fun t ->
        return (Char.chr (BatInt.of_string (BatString.implode ('0'::'x'::t))))
      | '0' .. '9' as x ->
        times 2 digit >>= fun t ->
        return (Char.chr (BatInt.of_string (BatString.implode (x::t))))
      | _ -> fail
    )


module Languages =
struct

  module type Definition =
  sig
    val comment_delimiters : (string * string) option
    val line_comment_start : string option
    val nested_comments  : bool
    val ident_start      : (char, char, position) BatParserCo.t
    val ident_letter     : (char, char, position) BatParserCo.t
    val op_start         : (char, char, position) BatParserCo.t
    val op_letter        : (char, char, position) BatParserCo.t
    val reserved_names   : string list
    val case_sensitive   : bool
  end


  module Library =
  struct
    (**A good approximation of language definition for OCaml's lexer*)
    module OCaml =
    struct
      let comment_delimiters = Some ("(*", "*)")
      let line_comment_start = None
      let nested_comments = true
      let ident_start     = either [uppercase; lowercase; one_of ['_'; '`']]
      let ident_letter    = either [uppercase; lowercase; digit; one_of ['\''; '_']]
      let op_start        = satisfy (BatChar.is_symbol)
      let op_letter       = op_start
      let reserved_names  = ["fun"; "let"; "module"; "begin"; "end"; "sig"; "function";
                             "{"; "}"; ";"; "|"; ","; ":"; "."; ](*@TODO: Complete*)
      let case_sensitive  = true
    end



    (**A good approximation of language definition for C++'s lexer*)
    module C =
    struct
      let comment_delimiters = Some ("/*", "*/")
      let line_comment_start = Some "//"
      let nested_comments = true
      let ident_start     = either [uppercase; lowercase; char '_']
      let ident_letter    = either [ident_start; digit]
      let op_start        = one_of [';';':';'!';'$';'%';'&';'*';'+';'.';'/';'<';'=';'>';'?';'^';'|';'-';'~']
      let op_letter       = op_start
      let reserved_names  = ["continue"; "volatile"; "register"; "unsigned"; "typedef"; "default";
                             "sizeof"; "switch"; "return"; "extern"; "struct"; "static"; "signed"; "while";
                             "break"; "union"; "const"; "else"; "case"; "enum";
                             "auto"; "goto"; "for"; "if"; "do" ]
      let case_sensitive  = true
    end
  end




  (** Create a lexer based on conventions*)
  module Make (M:Definition) =
  struct
    open M
    (** {6 Case management} *)
    let char =
      if case_sensitive then char
      else                   case_char

    let string =
      if case_sensitive then string
      else                   case_string

    let adapt_case =
      if case_sensitive then identity
      else String.lowercase

    let string_compare =
      if case_sensitive then String.compare
      else BatString.icompare

    (** {6 Whitespace management} *)
    let line_comment =
      match line_comment_start with
      | None   -> fail
      | Some s ->
        (*label "Line comment"*)label "" (
          string s                         >>= fun _ ->
          ignore_zero_plus (not_char '\n') >>= fun _ ->
          newline                          >>= fun _ ->
          return ())
    (*Note: we use [string] rather than [CharParser.string], as the line comment
		  may be introduced by a word rather than a symbol (e.g. Basic's [REM]), hence
		  may depend on case sensitivity.*)

    let multiline_comment =
      match comment_delimiters with
      | None        -> fail
      | Some (l, r) ->
        (*label "Multi-line comment"*) label "" (
          let l0 = String.get l 0
          and r0 = String.get r 0
          and string_r = string r in
          let in_comment () =
            if nested_comments then
              let not_lr = label ("Neither \""^l^"\" nor ^ \"" ^r^"\"")
                  (none_of [r0; l0]) in
              let rec aux () =
                label "aux" (
                  either [  string r >>= (fun _ -> return ());
                            string l >>= (fun _ -> aux () >>= fun _ -> aux ()) ;
                            (ignore_one_plus not_lr)      >>= fun _ -> aux () ]
                )
              in aux ()
            else
              string l >>>
              label "Contents of comments" (
                let rec aux () =
                  maybe string_r >>= function
                  | Some _ -> return ()
                  | None   -> any >>> aux ()
                in aux ()
              )
          in in_comment ())

    let comment = ( line_comment <|> multiline_comment ) >>> return ()

    let whitespaces =
      ignore_zero_plus (either
          [ satisfy BatChar.is_whitespace >>= (fun _ -> return ());
            comment ])

    let to_symbol p =
      p           >>= fun r ->
      whitespaces >>= fun _ ->
      return (BatString.of_list r)

    let lexeme p =
      p           >>= fun r ->
      whitespaces >>= fun _ ->
      return r

    (** {6 Actual content} *)
    let identifier_content = either [ident_start >:: zero_plus ident_letter ;
                                     op_start    >:: zero_plus op_letter]

    let is_reserved s = List.mem s reserved_names

    let ident_or_kwd = label "identifier or reserved"
        (label ""
           ( to_symbol identifier_content >>= fun s ->
             (*	    Printf.eprintf "Got something %S\n" s;*)
             return (adapt_case s)))

    let ident = label "identifier or operator"
        (label ""
           (ident_or_kwd >>= fun s ->
            if is_reserved s then fail
            else                  ((*Printf.eprintf "Got ident %S\n" s;*)return s)))

    (*      let kwd   = label "keyword"
		(ident_or_kwd >>= fun s ->
		  if is_reserved s then (Printf.eprintf "Got reserved %S\n" s; return s)
		  else                  fail)*)
    let kwd = label "keyword" (ident_or_kwd)

    let identifier s = label ("specific identifier \""^s^"\"") (label ""
          (ident >>= fun s' ->
           if string_compare s s' = 0 then return ()
           else                            fail))

    let keyword s = label ("specific keyword \""^s^"\"") (label ""
          (kwd >>= fun s' ->
           if string_compare s s' = 0 then return ()
           else                            fail))

    (*      let as_identifier  p = p >>= fun s -> if List.mem s reserved_names    then fail else return s

            let as_operator    p = p >>= fun s -> if List.mem s reserved_op_names then fail else return s*)

    (*      let any_reserved = label "reserved name"
		( to_symbol (ident_start >:: zero_plus ident_letter)   >>= fun s ->
		    if List.mem s reserved_names then return s
		    else                               fail)

            let any_reserved_op = label "reserved operator"
		( to_symbol (op_start >:: zero_plus op_letter)    >>= fun s ->
		    if List.mem s reserved_op_names then return s
		    else                                 fail)*)

    let char_literal = label "Character literal"
        (BatCharParser.char '\'' >>= fun _ ->
         any       >>= function
         | '\\' -> ocaml_escape
         | c    -> return c
        ) >>= fun c ->
      BatCharParser.char '\'' >>= fun _ -> return c

    let string_literal =  label "String Literal"
        (lexeme
           (BatCharParser.char '"' >>>
            let rec content chars =
              any >>= function
              | '"'  ->
                return chars
              | '\\' ->
                ocaml_escape >>= fun e ->
                content (e::chars)
              | e    -> (*Printf.eprintf "Just received char %c\n" e;*)
                content (e::chars)
            in content [] >>= fun c ->
            (*Printf.eprintf "Sending full string %S\n" (String.of_list (List.rev c));*)
            return (BatString.of_list (List.rev c))))


    let integer =
      label "OCaml-style integer" (
        lexeme(maybe (BatCharParser.char '-') >>= fun sign   ->
          one_plus digit   >>= fun digits ->
          let number = BatInt.of_string (BatString.of_list digits) in
          match sign with
          | Some _ -> return (~- number)
          | None   -> return number ))

    let float =
      label "OCaml-style floating-point number" (
        lexeme (maybe (BatCharParser.char '-')                   >>= fun sign     ->
          post_map BatString.of_list (zero_plus digit)     >>= fun int_part ->
          maybe (
            BatCharParser.char '.'  >>= fun _ ->
            post_map BatString.of_list (zero_plus digit) ) >>= fun decimal_part ->
          maybe (
            BatCharParser.char 'E'  >>= fun _ ->
            maybe (BatCharParser.char '+' <|> BatCharParser.char '-') >>= fun sign ->
            let sign = BatOption.default '+' sign  in
            one_plus digit >>= fun expo ->
            return ("E" ^ (BatString.of_char sign) ^ (BatString.of_list expo)))
          >>= fun expo ->

          let number = match (decimal_part, expo) with
            | Some d, Some e -> Some (int_part ^ "." ^ d ^ e)
            | Some d, None   -> Some (int_part ^ "." ^ d)
            | None,   Some e -> Some (int_part ^ e )
            | None,   None   -> None
          in match number with
          | None   -> fail
          | Some n -> let absolute = BatFloat.of_string n in
            return (
              match sign with
              | None   -> absolute
              | Some _ -> ~-. absolute)
        ))

    let number =
      ( float   >>= fun f -> return (`Float f))
      <|>( integer >>= fun i -> return (`Integer i) )

    (** Getting it all together. *)
    let check_reserved =
      if case_sensitive then
        fun x ->
          if is_reserved x then (Kwd x)
          else                  (Ident x)
      else
        fun x -> let x = String.lowercase x in
          if is_reserved x then (Kwd x)
          else                  (Ident x)


    let as_parser =
      whitespaces >>= fun _ -> either [
        ident_or_kwd              >>= (fun x -> return (check_reserved x));
        float                     >>= (fun x -> return (Float x) );
        integer                   >>= (fun x -> return (Int x) );
        string_literal            >>= (fun x -> return (String x) );
        char_literal              >>= (fun x -> return (Char x) )
      ]

    let feed = source_map as_parser

    let start = whitespaces


  end

end
