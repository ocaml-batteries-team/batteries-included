(*
 * pa_optcomp.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of optcomp.
 *)

open Camlp4.PreCast

(* Subset of supported caml types *)
type typ =
  | Tvar of string
  | Tbool
  | Tint
  | Tchar
  | Tstring
  | Ttuple of typ list

(* Subset of supported caml values *)
type value =
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Tuple of value list

type ident = string
    (* An identifier. It is either a lower or a upper identifier. *)

module Env = Map.Make(struct type t = ident let compare = compare end)

type env = value Env.t

type directive =
  | Dir_define of ident * Ast.expr
  | Dir_default of ident * Ast.expr
  | Dir_if of Ast.expr
  | Dir_else
  | Dir_elif of Ast.expr
  | Dir_endif
  | Dir_include of Ast.expr
  | Dir_error of Ast.expr
  | Dir_directory of Ast.expr

(* +-------------+
   | Environment |
   +-------------+ *)

let env = ref Env.empty
let define id value = env := Env.add id value !env

let _ =
  define "ocaml_version" (Scanf.sscanf Sys.ocaml_version "%d.%d" (fun major minor -> Tuple [Int major; Int minor]))

let dirs = ref []
let add_include_dir dir = dirs := dir :: !dirs

(* +-----------------------+
   | Expression evaluation |
   +-----------------------+ *)

let rec type_of_value = function
  | Bool _ -> Tbool
  | Int _ -> Tint
  | Char _ -> Tchar
  | String _ -> Tstring
  | Tuple l -> Ttuple (List.map type_of_value l)

let rec string_of_type = function
  | Tvar v -> "'" ^ v
  | Tbool -> "bool"
  | Tint -> "int"
  | Tchar -> "char"
  | Tstring -> "string"
  | Ttuple l -> "(" ^ String.concat " * " (List.map string_of_type l) ^ ")"

let invalid_type loc expected real =
  Loc.raise loc (Failure
                   (Printf.sprintf "this expression has type %s but is used with type %s"
                      (string_of_type real) (string_of_type expected)))

let type_of_patt patt =
  let rec aux (a, n) = function
    | <:patt< $tup:x$ >> ->
      let l, x = List.fold_left
        (fun (l, x) patt -> let t, x = aux x patt in (t :: l, x))
        ([], (a, n)) (Ast.list_of_patt x []) in
      (Ttuple(List.rev l), x)
    | _ ->
        (Tvar(Printf.sprintf "%c%s"
                (char_of_int (Char.code 'a' + a))
                (if n = 0 then "" else string_of_int n)),
         if a = 25 then (0, n + 1) else (a + 1, n))
  in
  fst (aux (0, 0) patt)

let rec eval env = function

  (* Literals *)
  | <:expr< true >> -> Bool true
  | <:expr< false >> -> Bool false
  | <:expr< $int:x$ >> -> Int(int_of_string x)
  | <:expr< $chr:x$ >> -> Char(Camlp4.Struct.Token.Eval.char x)
  | <:expr< $str:x$ >> -> String(Camlp4.Struct.Token.Eval.string ~strict:() x)
  | <:expr< $tup:x$ >> -> Tuple(List.map (eval env) (Ast.list_of_expr x []))

  (* Variables *)
  | <:expr@loc< $lid:x$ >>
  | <:expr@loc< $uid:x$ >> ->
    begin try
      Env.find x env
    with
        Not_found ->
          Loc.raise loc (Failure (Printf.sprintf "unbound value %s" x))
    end

  (* Value comparing *)
  | <:expr< $x$ = $y$ >> -> let x, y = eval_same env x y in Bool(x = y)
  | <:expr< $x$ < $y$ >> -> let x, y = eval_same env x y in Bool(x < y)
  | <:expr< $x$ > $y$ >> -> let x, y = eval_same env x y in Bool(x > y)
  | <:expr< $x$ <= $y$ >> -> let x, y = eval_same env x y in Bool(x <= y)
  | <:expr< $x$ >= $y$ >> -> let x, y = eval_same env x y in Bool(x >= y)
  | <:expr< $x$ <> $y$ >> -> let x, y = eval_same env x y in Bool(x <> y)

  (* min and max *)
  | <:expr< min $x$ $y$ >> -> let x, y = eval_same env x y in min x y
  | <:expr< max $x$ $y$ >> -> let x, y = eval_same env x y in max x y

  (* Arithmetic *)
  | <:expr< $x$ + $y$ >> -> Int(eval_int env x + eval_int env y)
  | <:expr< $x$ - $y$ >> -> Int(eval_int env x - eval_int env y)
  | <:expr< $x$ * $y$ >> -> Int(eval_int env x * eval_int env y)
  | <:expr< $x$ / $y$ >> -> Int(eval_int env x / eval_int env y)
  | <:expr< $x$ mod $y$ >> -> Int(eval_int env x mod eval_int env y)

  (* Boolean operations *)
  | <:expr< not $x$ >> -> Bool(not (eval_bool env x))
  | <:expr< $x$ or $y$ >> -> Bool(eval_bool env x or eval_bool env y)
  | <:expr< $x$ || $y$ >> -> Bool(eval_bool env x || eval_bool env y)
  | <:expr< $x$ && $y$ >> -> Bool(eval_bool env x && eval_bool env y)

  (* String operations *)
  | <:expr< $x$ ^ $y$ >> -> String(eval_string env x ^ eval_string env y)

  (* Pair operations *)
  | <:expr< fst $x$ >> -> fst (eval_pair env x)
  | <:expr< snd $x$ >> -> snd (eval_pair env x)

  (* Let-binding *)
  | <:expr< let $p$ = $x$ in $y$ >> ->
    let vx = eval env x in
    let env =
      try
        bind env p vx
      with
          Exit -> invalid_type (Ast.loc_of_expr x) (type_of_patt p) (type_of_value vx)
    in
    eval env y

  | e -> Loc.raise (Ast.loc_of_expr e) (Stream.Error "expression not supported")

and bind env patt value = match patt with
  | <:patt< $lid:id$ >> ->
    Env.add id value env

  | <:patt< $tup:patts$ >> ->
    let patts = Ast.list_of_patt patts [] in
    begin match value with
      | Tuple values when List.length values = List.length patts ->
          List.fold_left2 bind env patts values
      | _ ->
          raise Exit
    end

  | _ ->
      Loc.raise (Ast.loc_of_patt patt) (Stream.Error "pattern not supported")

and eval_same env ex ey =
  let vx = eval env ex and vy = eval env ey in
  let tx = type_of_value vx and ty = type_of_value vy in
  if tx = ty then
    (vx, vy)
  else
    invalid_type (Ast.loc_of_expr ey) tx ty

and eval_int env e = match eval env e with
  | Int x -> x
  | v -> invalid_type (Ast.loc_of_expr e) Tint (type_of_value v)

and eval_bool env e = match eval env e with
  | Bool x -> x
  | v -> invalid_type (Ast.loc_of_expr e) Tbool (type_of_value v)

and eval_string env e = match eval env e with
  | String x -> x
  | v -> invalid_type (Ast.loc_of_expr e) Tstring (type_of_value v)

and eval_pair env e = match eval env e with
  | Tuple [x; y] -> (x, y)
  | v -> invalid_type (Ast.loc_of_expr e) (Ttuple [Tvar "a"; Tvar "b"]) (type_of_value v)

(* +-----------------------+
   | Parsing of directives |
   +-----------------------+ *)

let rec skip_space stream = match Stream.peek stream with
  | Some((BLANKS _ | COMMENT _), _) ->
      Stream.junk stream;
      skip_space stream
  | _ ->
      ()

let rec parse_eol stream =
  let tok, loc = Stream.next stream in
  match tok with
    | BLANKS _ | COMMENT _ ->
        parse_eol stream
    | NEWLINE | EOI ->
        ()
    | _ ->
        Loc.raise loc (Stream.Error "end of line expected")

let parse_ident stream =
  skip_space stream;
  let tok, loc = Stream.next stream in
  begin match tok with
    | LIDENT id | UIDENT id ->
        id
    | _ ->
        Loc.raise loc (Stream.Error "identifier expected")
  end

let parse_expr stream =
  (* Lists of opened brackets *)
  let opened_brackets = ref [] in

  (* Return the next token of [stream] until all opened parentheses
     have been closed and a newline is reached *)
  let rec next_token _ =
    Some(match Stream.next stream, !opened_brackets with
           | (NEWLINE, loc), [] ->
               EOI, loc

           | (KEYWORD("(" | "[" | "{" as b), _) as x, l ->
               opened_brackets := b :: l;
               x

           | (KEYWORD ")", loc) as x, "(" :: l ->
               opened_brackets := l;
               x

           | (KEYWORD "]", loc) as x, "[" :: l ->
               opened_brackets := l;
               x

           | (KEYWORD "}", loc) as x, "{" :: l ->
               opened_brackets := l;
               x

           | x, _ ->
               x)
  in

  Gram.parse_tokens_after_filter Syntax.expr_eoi
    (Gram.Token.Filter.filter (Gram.get_filter ()) (Stream.from next_token))

let parse_directive stream = match Stream.npeek 2 stream with
  | [KEYWORD "#", loc; (LIDENT dir | KEYWORD dir), _] ->
      (* Move the location to the beginning of the line *)
      let (file_name,
           start_line, start_bol, start_off,
           stop_line,  stop_bol,  stop_off,
           ghost) = Loc.to_tuple loc in
      let loc = Loc.of_tuple (file_name,
                              start_line, start_bol, start_bol,
                              start_line, start_bol, start_bol,
                              ghost) in

      begin match dir with
        | "define" ->
            Stream.junk stream;
            Stream.junk stream;
            let id = parse_ident stream in
            let expr = parse_expr stream in
            Some(Dir_define(id, expr), loc)

        | "default" ->
            Stream.junk stream;
            Stream.junk stream;
            let id = parse_ident stream in
            let expr = parse_expr stream in
            Some(Dir_default(id, expr), loc)

        | "if" ->
            Stream.junk stream;
            Stream.junk stream;
            Some(Dir_if(parse_expr stream), loc)

        | "else" ->
            Stream.junk stream;
            Stream.junk stream;
            parse_eol stream;
            Some(Dir_else, loc)

        | "elif" ->
            Stream.junk stream;
            Stream.junk stream;
            Some(Dir_elif(parse_expr stream), loc)

        | "endif" ->
            Stream.junk stream;
            Stream.junk stream;
            parse_eol stream;
            Some(Dir_endif, loc)

        | "include" ->
            Stream.junk stream;
            Stream.junk stream;
            Some(Dir_include(parse_expr stream), loc)

        | "directory" ->
            Stream.junk stream;
            Stream.junk stream;
            Some(Dir_directory(parse_expr stream), loc)

        | "error" ->
            Stream.junk stream;
            Stream.junk stream;
            Some(Dir_error(parse_expr stream), loc)

        | _ ->
            None
      end

  | _ ->
      None

let parse_command_line_define str =
  match Gram.parse_string Syntax.expr (Loc.mk "<command line>") str with
    | <:expr< $lid:id$ >>
    | <:expr< $uid:id$ >> -> define id (Bool true)
    | <:expr< $lid:id$ = $e$ >>
    | <:expr< $uid:id$ = $e$ >> -> define id (eval !env e)
    | _ -> invalid_arg str

(* +----------------+
   | BLock skipping |
   +----------------+ *)

let rec skip_line stream =
  match Stream.next stream with
    | NEWLINE, _ -> ()
    | EOI, loc -> Loc.raise loc (Stream.Error "#endif missing")
    | _ -> skip_line stream

let rec next_directive stream = match parse_directive stream with
  | Some dir -> dir
  | None -> skip_line stream; next_directive stream

let rec next_endif stream =
  let dir, loc = next_directive stream in
  match dir with
    | Dir_if _ -> skip_if stream; next_endif stream
    | Dir_else
    | Dir_elif _
    | Dir_endif -> dir
    | _ -> next_endif stream

and skip_if stream =
  let dir, loc = next_directive stream in
  match dir with
    | Dir_if _ ->
        skip_if stream;
        skip_if stream

    | Dir_else ->
        skip_else stream

    | Dir_elif _ ->
        skip_if stream

    | Dir_endif ->
        ()

    | _ -> skip_if stream

and skip_else stream =
  let dir, loc = next_directive stream in
  match dir with
    | Dir_if _ ->
        skip_if stream

    | Dir_else ->
        Loc.raise loc (Stream.Error "#else without #if")

    | Dir_elif _ ->
        Loc.raise loc (Stream.Error "#elif without #if")

    | Dir_endif ->
        ()

    | _ ->
        skip_else stream

(* +-----------------+
   | Token filtering |
   +-----------------+ *)

type context = Ctx_if | Ctx_else

(* State of the token filter *)
type state = {
  stream : (Gram.Token.t * Loc.t) Stream.t;
  (* Input stream *)

  mutable bol : bool;
  (* Wether we are at the beginning of a line *)

  mutable stack : context list;
  (* Nested contexts *)

  mutable tmp : (Gram.Token.t * Loc.t) list;
  (* List of temporary tokens which must be sent first *)

  on_eoi : Gram.Token.t * Loc.t -> Gram.Token.t * Loc.t;
  (* Eoi handler, it is used to restore the previous sate on #include
     directives *)
}

(* Return the next token from a stream, interpreting directives. *)
let rec next_token state_ref =
  let state = !state_ref in
  match state.tmp with
    | x :: l ->
        state.tmp <- l;
        x

    | [] ->
        if state.bol then
          match parse_directive state.stream, state.stack with
            | Some(Dir_if e, _), _ ->
                let rec aux e =
                  if eval_bool !env e then begin
                    state.stack <- Ctx_if :: state.stack;
                    next_token state_ref
                  end else
                    match next_endif state.stream with
                      | Dir_else ->
                          state.stack <- Ctx_else :: state.stack;
                          next_token state_ref

                      | Dir_elif e ->
                          aux e

                      | Dir_endif ->
                          next_token state_ref

                      | _ ->
                          assert false
                in
                aux e

            | Some(Dir_else, loc), ([] | Ctx_else :: _) ->
                Loc.raise loc (Stream.Error "#else without #if")

            | Some(Dir_elif _, loc), ([] | Ctx_else :: _) ->
                Loc.raise loc (Stream.Error "#elif without #if")

            | Some(Dir_endif, loc), [] ->
                Loc.raise loc (Stream.Error "#endif without #if")

            | Some(Dir_else, loc), Ctx_if :: l ->
                skip_else state.stream;
                state.stack <- l;
                next_token state_ref

            | Some(Dir_elif _, loc), Ctx_if :: l ->
                skip_if state.stream;
                state.stack <- l;
                next_token state_ref

            | Some(Dir_endif, loc), _ :: l ->
                state.stack <- l;
                next_token state_ref

            | Some(Dir_define(id, e), _), _ ->
                define id (eval !env e);
                next_token state_ref

            | Some(Dir_default(id, e), _), _ ->
                if not (Env.mem id !env) then
                  define id (eval !env e);
                next_token state_ref

            | Some(Dir_include e, _), _ ->
                let fname = eval_string !env e in
                (* Try to looks up in all include directories *)
                let fname =
                  try
                    List.find (fun dir -> Sys.file_exists (Filename.concat dir fname)) !dirs
                  with
                      (* Just try in the current directory *)
                      Not_found -> fname
                in
                let ic = open_in fname in
                let nested_state = {
                  stream = Gram.filter (Gram.lex (Loc.mk fname) (Stream.of_channel ic));
                  bol = true;
                  stack = [];
                  tmp = [];
                  on_eoi = (fun _ ->
                              (* Restore previous state and close channel on
                                 eoi *)
                              state_ref := state;
                              close_in ic;
                              next_token state_ref)
                } in
                (* Replace current state with the new one *)
                state_ref := nested_state;
                next_token state_ref

            | Some(Dir_directory e, loc), _ ->
                let dir = eval_string !env e in
                add_include_dir dir;
                (* Reput the directive in the stream because camlp4 will use
                   it *)
                state.tmp <- List.map (fun tok -> (tok, loc))
                  [KEYWORD "#"; LIDENT "directory"; BLANKS " "; STRING(dir, String.escaped dir); NEWLINE];
                next_token state_ref

            | Some(Dir_error e, loc), _ ->
                Loc.raise loc (Failure (eval_string !env e))

            | None, _ ->
                let tok, loc = Stream.next state.stream in
                state.bol <- tok = NEWLINE;
                if tok = EOI then begin
                  if state.stack <> [] then
                    Loc.raise loc (Stream.Error "#endif missing");
                  state.on_eoi (tok, loc)
                end else
                  (tok, loc)

        else begin
          let tok, loc = Stream.next state.stream in
          state.bol <- tok = NEWLINE;
          (tok, loc)
        end

let stream_filter filter stream =
  let state_ref = ref { stream = stream;
                        bol = true;
                        tmp = [];
                        stack = [];
                        on_eoi = (fun x -> x) } in
  filter (Stream.from (fun _ -> Some(next_token state_ref)))

(* +--------------+
   | Registration |
   +--------------+ *)

let _ =
  Camlp4.Options.add "-D" (Arg.String parse_command_line_define)
    "<string> Define for #define directive.";
  Camlp4.Options.add "-I" (Arg.String add_include_dir)
    "<string> Add a directory to #include search path.";

  Gram.Token.Filter.define_filter (Gram.get_filter ()) stream_filter
