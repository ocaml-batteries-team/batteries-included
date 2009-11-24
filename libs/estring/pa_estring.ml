(*
 * pa_estring.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open Printf
open Camlp4.Sig
open Camlp4.PreCast

type specifier = string

type context = {
  mutable next_id : int;
  mutable shared_exprs : (Loc.t * string * Ast.expr) list;
}

let lookup tbl key =
  try
    Some(Hashtbl.find tbl key)
  with
      Not_found -> None

(* +---------------------+
   | Lists with location |
   +---------------------+ *)

type 'a llist =
  | Nil of Loc.t
  | Cons of Loc.t * 'a * 'a llist

let loc_of_llist = function
  | Nil loc -> loc
  | Cons(loc, x, l) -> loc

let rec llength_rec acc = function
  | Nil _ -> acc
  | Cons(_, _, ll) -> llength_rec (acc + 1) ll

let llength ll = llength_rec 0 ll

let rec lfoldr f g = function
  | Nil loc -> g loc
  | Cons(loc, x, l) -> f loc x (lfoldr f g l)

let rec list_of_llist = function
  | Nil _ -> []
  | Cons(_, x, l) -> x :: list_of_llist l

let rec llist_of_list loc = function
  | [] -> Nil loc
  | x :: l -> Cons(loc, x, llist_of_list (Loc.move `start 1 loc) l)

let rec ldrop n l =
  if n <= 0 then
    l
  else match l with
    | Cons(_, _, l) -> ldrop (n - 1) l
    | l -> l

let rec ltake n l =
  if n <= 0 then
    Nil (loc_of_llist l)
  else match l with
    | Cons(loc, x, l) -> Cons(loc, x, ltake (n - 1) l)
    | l -> l

let rec lappend ll1 ll2 = match ll1 with
  | Nil _ -> ll1
  | Cons(loc, x, ll) -> Cons(loc, x, lappend ll ll2)

let llist_expr f ll = lfoldr (fun _loc x acc -> <:expr< $f _loc x$ :: $acc$ >>) (fun _loc -> <:expr< [] >>) ll
let llist_patt f ll = lfoldr (fun _loc x acc -> <:patt< $f _loc x$ :: $acc$ >>) (fun _loc -> <:patt< [] >>) ll

(* +--------------------+
   | Strings unescaping |
   +--------------------+ *)

(* String appears in the camlp4 ast as they apears in the source
   code. So if we want to process a string then we need to first
   unescape it. Camlp4 provide such a function
   (Camlp4.Struct.Token.Eval.string) but the problem is that we do not
   know exactly the location of unescaped characters:

   For instance: "\tx\tA" will be unescaped in " x A", and the
   position of "A" in the resulting string will be changed.

   So here is an implementation of an unescaping function which also
   compute the location of each unescaped characters. *)

module Unescape =
struct
  let add n loc = Loc.move `start n loc
  let inc loc = add 1 loc
  let addl n loc = Loc.move_line n loc
  let incl loc = addl 1 loc
  let resetl loc = addl 0 loc

  let dec x = Char.code x - Char.code '0'
  let hex = function
    | '0'..'9' as x -> Char.code x - Char.code '0'
    | 'a'..'f' as x -> Char.code x - Char.code 'a' + 10
    | 'A'..'F' as x -> Char.code x - Char.code 'A' + 10
    | x -> assert false

  let rec skip_indent cont loc = function
    | (' ' | '\t') :: l -> skip_indent cont (inc loc) l
    | l -> cont loc l

  let skip_opt_linefeed cont loc = function
    | '\n' :: l -> cont (incl loc) l
    | l -> cont loc l

  let rec string loc = function
    | [] -> Nil loc
    | '\\' :: l ->
        let loc = inc loc in
        begin match l with
          | '\n' :: l -> skip_indent string (incl loc) l
          | '\r' :: l -> skip_opt_linefeed (skip_indent string) (resetl loc) l
          | 'n' :: l -> Cons(loc, '\n', string (inc loc) l)
          | 'r' :: l -> Cons(loc, '\r', string (inc loc) l)
          | 't' :: l -> Cons(loc, '\t', string (inc loc) l)
          | 'b' :: l -> Cons(loc, '\b', string (inc loc) l)
          | '\\' :: l -> Cons(loc, '\\', string (inc loc) l)
          | '"' :: l  -> Cons(loc, '"', string (inc loc) l)
          | '\'' :: l -> Cons(loc, '\'', string (inc loc) l)
          | ' ' :: l -> Cons(loc, ' ', string (inc loc) l)
          | ('0'..'9' as c1) :: ('0'..'9' as c2) :: ('0'..'9' as c3) :: l ->
              Cons(loc,
                   char_of_int (100 * (dec c1) + 10 * (dec c2) + (dec c3)),
                   string (add 3 loc) l)
          | 'x'
            :: ('0'..'9' | 'a'..'f' | 'A'..'F' as c1)
            :: ('0'..'9' | 'a'..'f' | 'A'..'F' as c2) :: l ->
              Cons(loc,
                   char_of_int (16 * (hex c1) + (hex c2)),
                   string (add 3 loc) l)
          | _ -> Loc.raise loc (Stream.Error "illegal backslash")
        end
    | '\r' :: l -> Cons(loc, '\r', string (resetl loc) l)
    | '\n' :: l -> Cons(loc, '\n', string (incl loc) l)
    | ch :: l -> Cons(loc, ch, string (inc loc) l)
end

let unescape loc str =
  let l = ref [] in
  for i = String.length str - 1 downto 0 do
    l := str.[i] :: !l
  done;
  Unescape.string loc !l

(* +------------------------+
   | Specifier registration |
   +------------------------+ *)

module String_set = Set.Make(String)

let specifiers = ref String_set.empty
let add_specifier spec =
  specifiers := String_set.add spec !specifiers

let expr_specifiers = Hashtbl.create 42
let patt_specifiers = Hashtbl.create 42
let when_specifiers = Hashtbl.create 42

let register_expr_specifier specifier f =
  add_specifier specifier;
  Hashtbl.add expr_specifiers specifier f

let register_patt_specifier specifier f =
  add_specifier specifier;
  Hashtbl.add patt_specifiers specifier f

let register_when_specifier specifier f =
  add_specifier specifier;
  Hashtbl.add when_specifiers specifier f

(* +------------------------------+
   | String specifier recognition |
   +------------------------------+ *)

(* Strings with a specifier are recognized using a token filter. This
   is to avoid recognizing things like [u "string"], [X.u"string"].

   Strings with a specifier are replaced by an identifier of the form
   "__estring_string_NNN_XXX". *)

let strings = Hashtbl.create 42
  (* Mapping identifier of the form "__estring_XXX" -> specifier + string literal *)

let estring_prefix = sprintf "__estring_string_%d_" (Oo.id (object end))
  (* Prefix for identifiers referring to strings with specifier. The
     [Oo.id (object end)] is a trick to generate a fresh id so several
     estring instances can works together. *)

let gen_string_id =
  let nb = ref 0 in
  fun () ->
    let x = !nb in
    nb := x + 1;
    estring_prefix ^ string_of_int x

let wrap_stream stm =
  (* The previous token *)
  let previous = ref EOI in

  let func pos =
    try
      let prev = !previous
      and tok, loc = Stream.next stm in

      previous := tok;

      match tok with
        | (LIDENT id | UIDENT id) when prev <> KEYWORD "." && String_set.mem id !specifiers ->
            begin match Stream.peek stm with
              | Some(STRING(s, orig), loc) ->
                  Stream.junk stm;
                  let string_id = gen_string_id () in
                  Hashtbl.add strings string_id (id, orig);
                  Some(LIDENT string_id, loc)
              | _ ->
                  Some(tok, loc)
            end

        | _ ->
            Some(tok, loc)
    with
        Stream.Failure -> None
  in
  Stream.from func

(* +--------------------+
   | Strings conversion |
   +--------------------+ *)

let register_shared_expr context expr =
  let id = "__estring_shared_" ^ string_of_int context.next_id in
  context.next_id <- context.next_id + 1;
  let _loc = Ast.loc_of_expr expr in
  context.shared_exprs <- (_loc, id, expr) :: context.shared_exprs;
  <:ident< $lid:id$ >>

let is_special_id id =
  let rec aux1 i =
    if i = String.length estring_prefix then
      aux2 i
    else
      i < String.length id && id.[i] = estring_prefix.[i] && aux1 (i + 1)
  and aux2 i =
    (i < String.length id) && match id.[i] with
      | '0' .. '9' -> aux3 (i + 1)
      | _ -> false
  and aux3 i =
    if i = String.length id then
      true
    else match id.[i] with
      | '0' .. '9' -> aux3 (i + 1)
      | _ -> false
  in
  aux1 0

let expand_expr context _loc id =
  match lookup strings id with
    | Some(specifier, string) -> begin
        match lookup expr_specifiers specifier with
          | Some f ->
              f context _loc string
          | None ->
              Loc.raise _loc (Failure "pa_estring: this specifier can not be used here")
      end

    | None ->
        <:expr< $lid:id$ >>

let expand_patt context _loc id =
  match lookup strings id with
    | Some(specifier, string) -> begin
        match lookup patt_specifiers specifier with
          | Some f ->
              f context _loc string
          | None ->
              Loc.raise _loc (Failure "pa_estring: this specifier can not be used here")
      end

    | None ->
        <:patt< $lid:id$ >>

(* Replace extended strings with identifiers and collect conditions *)
let map_match context (num, conds) = object
  inherit Ast.map as super

  method patt p = match super#patt p with
    | <:patt@_loc< $lid:id$ >> as p when is_special_id id -> begin
        match lookup strings id with
          | Some(specifier, string) -> begin
              match lookup when_specifiers specifier with
                | Some f ->
                    let id = <:ident< $lid:"__estring_var_" ^ string_of_int !num$ >> in
                    incr num;
                    conds := f context _loc id string :: !conds;
                    <:patt< $id:id$ >>

                | None ->
                    expand_patt context _loc id
            end

          | None ->
              p
      end

    | p -> p
end

let map context = object(self)
  inherit Ast.map as super

  method expr e = match super#expr e with
    | <:expr@_loc< $lid:id$ >> when is_special_id id -> expand_expr context _loc id
    | e -> e

  method patt p = match super#patt p with
    | <:patt@_loc< $lid:id$ >> when is_special_id id -> expand_patt context _loc id
    | p -> p

  method match_case = function
    | <:match_case@_loc< $p$ when $c$ -> $e$ >> ->
        let conds = ref [] in
        let p = (map_match context (ref 0, conds))#patt p
        and c = self#expr c and e = self#expr e in
        let gen_mc first_cond conds =
          <:match_case< $p$ when $List.fold_left (fun acc cond -> <:expr< $cond$ && $acc$ >>) first_cond conds$ -> $e$ >>
        in
        begin match c, !conds with
          | <:expr< >>, [] ->
              <:match_case< $p$ when $c$ -> $e$ >>

          | <:expr< >>, c :: l ->
              gen_mc c l

          | e, l ->
              gen_mc e l
        end

    | mc ->
        super#match_case mc
end

let map_expr e =
  let context = { next_id = 0; shared_exprs = [] } in
  let e = (map context)#expr e in
  List.fold_left
    (fun acc (_loc, id, expr) -> <:expr< let $lid:id$ = $expr$ in $acc$ >>)
    e context.shared_exprs

let map_class_expr e =
  let context = { next_id = 0; shared_exprs = [] } in
  let e = (map context)#class_expr e in
  List.fold_left
    (fun acc (_loc, id, expr) -> <:class_expr< let $lid:id$ = $expr$ in $acc$ >>)
    e context.shared_exprs

let rec map_binding = function
  | <:binding@_loc< $id$ = $e$ >> ->
      <:binding< $id$ = $map_expr e$ >>
  | <:binding@_loc< $a$ and $b$ >> ->
      <:binding< $map_binding a$ and $map_binding b$ >>
  | x ->
      x

let map_def = function
  | Ast.StVal(loc, is_rec, binding) ->
      Ast.StVal(loc, is_rec, map_binding binding)
  | Ast.StExp(loc, expr) ->
      Ast.StExp(loc, map_expr expr)
  | Ast.StCls(loc, ce) ->
      Ast.StCls(loc, map_class_expr ce)
  | x ->
      x

(* +--------------+
   | Registration |
   +--------------+ *)

let _ =
  (* Register the token filter for specifiers *)
  Gram.Token.Filter.define_filter (Gram.get_filter ()) (fun filter stm -> filter (wrap_stream stm));

  let map = (Ast.map_str_item map_def)#str_item in

  (* Register the mapper for implementations *)
  AstFilters.register_str_item_filter map;

  (* Register the mapper for the toplevel *)
  AstFilters.register_topphrase_filter map
