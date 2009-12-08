(*
 * pa_strings.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

open Camlp4.PreCast
open Pa_estring

(* Validation of UTF-8 strings *)

let validate_utf8 loc str =
  let str = Camlp4.Struct.Token.Eval.string ~strict:() str in
  let len = String.length str in
  let rec trail i minimum acc = function
    | 0 ->
        if acc < minimum then
          Loc.raise loc (Failure "overlong UTF-8 sequence")
        else
          loop i
    | count ->
        if i = len then
          Loc.raise loc (Failure "unterminated UTF-8 sequence ")
        else
          let n = Char.code str.[i] in
          if n land 0xc0 = 0x80 then
            trail (i + 1) minimum ((acc lsl 6) lor (n land 0x3f)) (count - 1)
          else
            Loc.raise loc (Failure "unterminated UTF-8 sequence")
  and loop i =
    if i = len then
      ()
    else
      let n = Char.code str.[i] in
      let i = i + 1 in
      if n land 0x80 = 0 then
        loop i
      else if n land 0xe0 = 0xc0 then
        trail i 0x80 (n land 0x1f) 1
      else if n land 0xf0 = 0xe0 then
        trail i 0x800 (n land 0x0f) 2
      else if n land 0xf8 = 0xf0 then
        trail i 0x10000 (n land 0x07) 3
      else
        Loc.raise loc (Failure "invalid start of UTF-8 sequence")
  in
  loop 0

(* Recoding Latin1 -> UTF-8 *)

let utf8_of_latin1 str =
  let str = Camlp4.Struct.Token.Eval.string ~strict:() str in
  let buf = Buffer.create (String.length str) in
  String.iter (fun ch ->
                 let code = Char.code ch in
                 if code < 128 then
                   Buffer.add_char buf ch
                 else begin
                   Buffer.add_char buf (char_of_int (0xc0 lor (code lsr 6)));
                   Buffer.add_char buf (char_of_int (0x80 lor (code land 0x3f)))
                 end) str;
  Buffer.contents buf

let _ =

  (* UTF-8 strings *)

  (*register_expr_specifier "u"
    (fun ctx _loc str ->
       let id = register_shared_expr ctx <:expr< BatUTF8.of_string $str:str$ >> in
       <:expr< $id:id$ >>);*)
  register_expr_specifier "u"
    (fun ctx _loc str ->
       validate_utf8 _loc str;
       <:expr< BatUTF8.of_string_unsafe $str:str$ >>);
  register_when_specifier "u"
    (fun ctx _loc id str ->
       validate_utf8 _loc str;
       let shared_id = register_shared_expr ctx <:expr< BatUTF8.of_string_unsafe $str:str$ >> in
       <:expr< BatUTF8.compare $id:shared_id$ $id:id$ = 0 >>);

  (* Strings with capabilities *)

  register_expr_specifier "rw"
    (fun ctx _loc str ->
       <:expr< BatString.Cap.of_string $str:str$ >>);
  register_when_specifier "rw"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< BatString.Cap.of_string $str:str$ >> in
       <:expr< BatString.Cap.compare $id:shared_id$ $id:id$ = 0 >>);

  register_expr_specifier "ro"
    (fun ctx _loc str ->
       let id = register_shared_expr ctx <:expr< BatString.Cap.read_only (BatString.Cap.of_string $str:str$) >> in
       <:expr< $id:id$ >>);
  register_when_specifier "ro"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< BatString.Cap.of_string $str:str$ >> in
       <:expr< BatString.Cap.compare $id:shared_id$ $id:id$ = 0 >>);

  register_expr_specifier "wo"
    (fun ctx _loc str ->
       <:expr< BatString.Cap.write_only (BatString.Cap.of_string $str:str$) >>);
  register_when_specifier "wo"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx <:expr< BatString.Cap.of_string $str:str$ >> in
       <:expr< BatString.Cap.compare $id:shared_id$ $id:id$ = 0 >>);

  (* Ropes *)

  let rope_of_latin1 _loc str =
    let str = utf8_of_latin1 str in
    <:expr< BatRope.of_ustring (BatUTF8.of_string_unsafe $str:String.escaped str$) >>
  in

  register_expr_specifier "r"
    (fun ctx _loc str ->
       let id = register_shared_expr ctx (rope_of_latin1 _loc str) in
       <:expr< $id:id$ >>);
  register_when_specifier "r"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx (rope_of_latin1 _loc str) in
       <:expr< BatRope.compare $id:shared_id$ $id:id$ = 0 >>);

  let rope_of_utf8 _loc str =
    validate_utf8 _loc str;
    <:expr< BatRope.of_ustring (BatUTF8.of_string_unsafe $str:str$) >>
  in

  register_expr_specifier "ur"
    (fun ctx _loc str ->
       let id = register_shared_expr ctx (rope_of_utf8 _loc str) in
       <:expr< $id:id$ >>);
  register_when_specifier "ur"
    (fun ctx _loc id str ->
       let shared_id = register_shared_expr ctx (rope_of_utf8 _loc str) in
       <:expr< BatUTF8.compare $id:shared_id$ $id:id$ = 0 >>)
