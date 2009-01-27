open Sexplib
include Sexp
include Conv

open Extlib.IO

(** {6 Types}*)
#if ocaml_version < (3, 11) (*The type was renamed between versions*)
type 'a sexp_opaque = 'a
#endif

(** {6 Parsing}*)

let reraise_parse_error pe global_pos =
#if ocaml_version >= (3, 11) (*The exception was renamed between versions*)
  raise (Parse_error pe)(*We can't perform the repositioning, as that's a private type*)
#else
  raise (ParseError pe) (*We can't perform the repositioning, as that's a private type*)
#endif
(*  let ps = pe.parse_state in
  let ppos = ps.parse_pos in
  let new_ppos = { ppos with buf_pos = global_pos + ppos.buf_pos } in
  let new_ps = { ps with parse_pos = new_ppos } in
  let new_pe = { pe with parse_state = new_ps } in
  raise (ParseError new_pe)*)


(*Function reimplemented as the original only accepts an [in_channel]*)
let input_sexp ?text_line ?text_char ?(buf_pos=0) ic =
  let buf = String.create 1 in
  let rec loop this_parse =
    let c = read ic in
      buf.[0] <- c;
      let parse_res =
	try this_parse ~pos:0 ~len:1 buf
#if ocaml_version >= (3, 11) (*The exception was renamed between versions*)
	with Parse_error pe -> reraise_parse_error pe buf_pos
#else
       	with ParseError pe -> reraise_parse_error pe buf_pos
#endif
      in
	match parse_res with
	  | Done (sexp, _) -> sexp
	  | Cont (_, this_parse) -> loop this_parse
  in
  let this_parse ~pos ~len str = parse ?text_line ?text_char ~pos ~len str in
    loop this_parse


(*Function reimplemented as the original only accepts an [in_channel]*)
let input_rev_sexps
    ?text_line ?text_char
    ?(buf_pos = 0) ?(buf = String.create 8192) ic =
  let rev_sexps_ref = ref [] in
  let buf_len = String.length buf in
  let is_incomplete_ref = ref false in
  let buf_pos_ref = ref buf_pos in
  let rec loop this_parse pos len =
    if len > 0 then
      let parse_res =
        try this_parse ~pos ~len buf
#if ocaml_version >= (3, 11) (*The exception was renamed between versions*)
        with Parse_error pe -> reraise_parse_error pe !buf_pos_ref
#else
        with ParseError pe -> reraise_parse_error pe !buf_pos_ref
#endif
      in
      match parse_res with
      | Done (sexp, new_pos) ->
          rev_sexps_ref := sexp :: !rev_sexps_ref;
          let n_parsed = new_pos.buf_pos - pos in
          is_incomplete_ref := false;
          let text_line = new_pos.text_line in
          let text_char = new_pos.text_char in
          let this_parse ~pos ~len str =
            parse ~text_line ~text_char ~pos ~len str
          in
          if n_parsed = len then
            let new_len = input ic buf 0 buf_len in
            buf_pos_ref := !buf_pos_ref + new_pos.buf_pos;
            loop this_parse 0 new_len
          else loop this_parse new_pos.buf_pos (len - n_parsed)
      | Cont (ws_only, this_parse) ->
          is_incomplete_ref := not ws_only;
          buf_pos_ref := !buf_pos_ref + len + pos;
          loop this_parse 0 (input ic buf 0 buf_len)
    else if !is_incomplete_ref then raise End_of_file
    else !rev_sexps_ref
  in
  let this_parse ~pos ~len str = parse ?text_line ?text_char ~pos ~len str in
  loop this_parse 0 (input ic buf 0 buf_len)

(*Function reimplemented as the original only accepts an [in_channel]*)
let input_sexps ?text_line ?text_char ?buf_pos ?buf ic =
  let rev_sexps = input_rev_sexps ?text_line ?text_char ?buf_pos ?buf ic in
  List.rev rev_sexps

(** {6 Printing}*)
let buffer () = Buffer.create 4096

let with_new_buffer oc f =
  let buf = buffer () in
    f buf;
    write_buf oc buf

let output_hum oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_hum sexp ~buf)

let output_hum_indent indent oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_hum ~indent sexp ~buf)

let output_mach oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_mach sexp ~buf)

let output = output_mach

let print  = output_hum

