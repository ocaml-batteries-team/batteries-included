module Genlex = struct

include Genlex

open ExtHashtbl
open ExtString

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
      content  : char Enum.t
    }

let junk e =
  e.position <- e.position + 1;
  Enum.junk e.content

let peek e =
  Enum.peek e.content



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
    let s = String.of_char c in
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
    | Some c   -> junk enum; comment enum
    | _        -> raise EarlyEndOfStream
  and maybe_nested_comment (enum : enum) =
    match peek enum with
      Some '*' -> junk enum; let s = enum in comment s; comment s
    | Some c   -> junk enum; comment enum
    | _        -> raise EarlyEndOfStream
  and maybe_end_comment (enum : enum) =
    match peek enum with
      Some ')' -> junk enum; ()
    | Some '*' -> junk enum; maybe_end_comment enum
    | Some c   -> junk enum; comment enum
    | _        -> raise EarlyEndOfStream
  in
  fun input -> Enum.from_while (fun count -> next_token {position = 0; content = input})
    
	
let to_stream_filter (kwd_table:t) (x:char Stream.t) : token Stream.t =
  (ExtStream.Stream.of_enum (to_enum_filter kwd_table (ExtStream.Stream.enum x)))

let to_lazy_list_filter kwd_table x =
  (LazyList.of_enum (to_enum_filter kwd_table (LazyList.enum x)))
end
