(*
 * pa_format.ml
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

open Camlp4.PreCast
open Pa_estring

(* Syntax extension for format string *)

(* An element of a format: *)
type element =
  | Cst of char
      (* A constant *)
  | Dir of Ast.expr option * (Loc.t * string) list * Ast.expr
      (* [Dir(flags, labels, expr)] a directive:

         - expr is the expression of the directive
         - flags are optionnal flags
         - labels are labels for directive arguments *)
  | Value_printer of (Loc.t * string) option * Ast.expr
      (* [VDir(label, expr)] a ``value-printer'' directive:

         - label is an optionnal label for the directive
         - expr is the expression of the value-printer *)

type ast = element llist
    (* A format ast is a list of elements *)

let rev_implode l =
  let len = List.length l in
  let s = String.create len in
  ignore (List.fold_left (fun i ch -> s.[i] <- ch; i - 1) (len - 1) l);
  s

(* +----------------+
   | Format parsing |
   +----------------+ *)

module Parse =
struct

  (* Parse directive flags: *)
  module Flags =
  struct

    let number = function
      | Cons(loc, ('0' .. '9' as ch), l) ->
          let rec aux acc = function
            | Cons(loc, ('0' .. '9' as ch), l) ->
                aux (acc * 10 + (Char.code ch - Char.code '0')) l
            | l ->
                (acc, l)
          in
          aux (Char.code ch - Char.code '0') l
      | l ->
          Loc.raise (loc_of_llist l) (Failure "digit expected")

    let rec loop acc = function
      | Cons(_loc, '-', l) ->
          loop (<:rec_binding< BatPervasives.pf_justify = `left >> :: acc) l

      | Cons(_loc, '0', l) ->
          loop (<:rec_binding< BatPervasives.pf_padding_char = '0' >> :: acc) l

      | Cons(_loc, ('+' | ' ' as ch), l) ->
          loop (<:rec_binding< BatPervasives.pf_positive_prefix = $chr:Char.escaped ch$ >> :: acc) l

      | Cons(_loc, '1' .. '9', _) as l ->
          let n, l = number l in
          loop (<:rec_binding< BatPervasives.pf_width = Some $int:string_of_int n$ >> :: acc) l
      | Cons(_loc, '.', (Cons(_l2, '0'..'9',_) as l)) ->
	  let n, l = number l in
	  loop (<:rec_binding< BatPervasives.pf_frac_digits = Some $int:string_of_int n$ >> :: acc) l
      | l ->
          (acc, l)

    let main l =
      let _loc = loc_of_llist l in
      match loop [] l with
        | ([], l) ->
            (None, l)

        | (flags, l) ->
            (Some(Ast.ExRec(_loc,
                            Ast.rbSem_of_list flags,
                            <:expr< BatPervasives.default_printer_flags >>)),
             l)
  end

  let is_ident_start = function
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '_'
    | '\192' .. '\214'
    | '\216' .. '\246'
    | '\248' .. '\255' ->
        true
    | _ ->
        false

  let is_ident_body = function
    | '\'' | '0' .. '9' ->
        true
    | ch ->
        is_ident_start ch

  (* Parse directive body: *)
  module Directive =
  struct

    let simple l =
      let rec aux acc = function
        | Cons(loc, ('A' .. 'Z' | 'a' .. 'z' as ch), l) ->
            aux (ch :: acc) l
        | l ->
            (acc, l)
      in
      let _loc = loc_of_llist l in
      let chars, l = aux [] l in
      (<:expr< $lid:"printer_" ^ rev_implode chars$ >>, l)

    let ident_body l =
      let rec aux acc = function
        | Cons(loc, ch, l) when is_ident_body ch ->
            aux (ch :: acc) l
        | l ->
            (acc, l)
      in
      match aux [] l with
        | ([], _) ->
            Loc.raise (loc_of_llist l) (Failure "identifier expected")
        | (chars, l) ->
            (rev_implode chars, l)

    let rec ident l =
      let _loc = loc_of_llist l in
      let x, l = ident_body l in
      match l with
        | Cons(_, '.', l) ->
            let id, l = ident l in
            (<:ident< $uid:x$ . $id$ >>, l)
        | l ->
            (<:ident< $lid:"printer_" ^ x$ >>, l)

    let composed l =
      let app _loc a b = match a with
        | None -> Some b
        | Some a -> Some(<:expr< $a$ $b$ >>)
      in
      let rec aux1 acc = function
        | Cons(_, ')', l) ->
            (acc, l)
        | Cons(_, ' ', l) ->
            aux1 acc l
        | Cons(_loc, '(', l) ->
            let e, l = aux2 l in
            aux1 (app _loc acc e) l
        | l ->
            let _loc = loc_of_llist l in
            let id, l = ident l in
            aux1 (app _loc acc <:expr< $id:id$ >>) l
      and aux2 l =
        match aux1 None l with
          | (None, _) ->
              Loc.raise (loc_of_llist l) (Failure "printer expected")
          | (Some e, l) ->
              (e, l)
      in
      aux2 l

    let names initial_l =
      let rec aux loc chars names l = match l, chars with
        | Cons(loc, (',' | ':'), l), [] ->
            Loc.raise loc (Failure "identifier expected")
        | Cons(_, ',', l), _ ->
            aux Loc.ghost [] ((loc, rev_implode chars) :: names) l
        | Cons(_, ':', l), _ ->
            (List.rev ((loc, rev_implode chars) :: names), l)
        | Cons(loc, ch, l), [] when is_ident_start ch ->
            aux loc [ch] names l
        | Cons(_, ch, l), _ when is_ident_body ch ->
            aux loc (ch :: chars) names l
        | l ->
            ([], initial_l)
      in
      aux Loc.ghost [] [] initial_l

    let main l = match l with
      | Cons(loc, ('A' .. 'Z' | 'a' .. 'z'), _) ->
          let e, l = simple l in
          ([], e, l)
      | Cons(loc, '(', l) ->
          let names, l = names l in
          let expr, l = composed l in
          (names, expr, l)
      | l ->
          Loc.raise (loc_of_llist l) (Failure "invalid start of printing directive")
  end

  (* Parse value printer directive: *)
  module Value_printer =
  struct
    let rec loop acc = function
      | Cons(loc, '}', l) ->
          (rev_implode acc, l)
      | Cons(loc, ch, l) ->
          loop (ch :: acc) l
      | Nil loc ->
          Loc.raise loc (Failure "'}' missing")

    let name initial_l =
      let rec aux chars l = match l, chars with
        | Cons(loc, ':', l), [] ->
            Loc.raise loc (Failure "identifier expected")
        | Cons(_, ':', l), _ ->
            (Some(loc_of_llist initial_l, rev_implode chars), l)
        | Cons(_, ch, l), [] when is_ident_start ch ->
            aux [ch] l
        | Cons(_, ch, l), _ when is_ident_body ch ->
            aux (ch :: chars) l
        | l ->
            (None, initial_l)
      in
      aux [] initial_l

    let main l =
      let name, l = name l in
      let str, l = loop [] l in
      (name, str, l)
  end

  let rec map_id = function
    | <:ident@_loc< $id:id1$ . $id:id2$ >> ->
        <:ident< $id:id1$ . $id:map_id id2$ >>
    | <:ident@_loc< $lid:id$ >> ->
        <:ident< $lid:id ^ "_printer"$ >>
    | id ->
        Loc.raise (Ast.loc_of_ident id) (Failure "pa_strings: i do not understand this identifier")

  (* Convert a type to a value printer: *)
  let rec vprinter_of_ctyp = function
    | <:ctyp@_loc< _ >> ->
        <:expr< fun paren out x -> BatIO.nwrite out "<abstract>" >>
    | <:ctyp@_loc< $id:id$ >> ->
        <:expr< $id:map_id id$ >>
    | Ast.TyApp (_loc, b, a) ->
        <:expr< $vprinter_of_ctyp b$ $vprinter_of_ctyp a$ >>
    | <:ctyp@_loc< $tup:t$ >> ->
        let l = Ast.list_of_ctyp t [] in
        List.fold_left
          (fun acc t ->
             let _loc = Ast.loc_of_expr acc in
             <:expr< $acc$ $vprinter_of_ctyp t$ >>)
          <:expr< BatValue_printer.$lid:"print_tuple" ^ string_of_int (List.length l)$ >>
          l
    | t ->
        Loc.raise (Ast.loc_of_ctyp t) (Failure "pa_strings: i do not understand this type")

  (* Parse a format and return a list of elements: *)
  let rec main = function
    | Nil loc ->
        Nil loc

    (* A literal '%' or a flush: *)
    | Cons(loc, '%', Cons(loc2, ('%' | '!' as ch), l)) ->
        Cons(loc, Cst '%', Cons(loc2, Cst ch, main l))

    (* The start of a value-printer directive: *)
    | Cons(_, '%', Cons(_, '{', l)) ->
        let loc = loc_of_llist l in
        let name, str, l = Value_printer.main l in
        let typ = Gram.parse_string Syntax.ctyp loc str in
        Cons(loc, Value_printer(name, vprinter_of_ctyp typ), main l)

    (* The start of a directive: *)
    | Cons(loc, '%', l) ->
        let flags, l = Flags.main l in
        let names, dir, l = Directive.main l in
        Cons(loc, Dir(flags, names, dir), main l)

    | Cons(loc, ch, l) ->
        Cons(loc, Cst ch, main l)
end

(* +--------------------------+
   | Format ast to expression |
   +--------------------------+ *)

(* Create the pattern string from the format ast.

   For example:

   make_pattern [Cst "x="; Dir([], "d"); Cst " y="; Dir([], "d")] = "x=%(0) y=%(1)"
*)
let make_pattern ast =
  let rec aux n = function
    | Cons(_, Cst c, l) -> String.make 1 c :: aux n l
    | Cons(_, (Dir _ | Value_printer _), l) -> "%(" :: string_of_int n :: ")" :: aux (n + 1) l
    | Nil _ -> []
  in
  String.concat "" (aux 0 ast)

(* Returns the expression of a directives, handling labelled
   arguments.

   For example:

   expr_of_directive _loc [] "d" = <:expr< printer_d >>
   expr_of_directive _loc ["a"; "b"] "s" = <:expr< fun k ~a ~b -> printer_s k a b >>
*)
let expr_of_directive _loc names expr =
  match names with
    | [] -> expr
    | _ ->
        let rec make_lidents n = function
          | (loc, "_") :: l -> (false, loc, "__" ^ string_of_int n) :: make_lidents (n + 1) l
          | (loc, name) :: l -> (true, loc, name) :: make_lidents n l
          | [] -> []
        in
        let lids = (false, _loc, "__k") :: make_lidents 0 names in
        List.fold_right
          (fun (labeled, _loc, id) acc ->
             if labeled then
               <:expr< fun ~ $id$ -> $acc$ >>
             else
               <:expr< fun $lid:id$ -> $acc$ >>)
          lids
          (List.fold_left (fun acc (labeled, _loc, id) ->
                             <:expr< $acc$ $lid:id$ >>) expr lids)

(* Builds the expression of a printer from a format ast.

   For example:

   make_printer _loc [Cst "x="; Dir([], "d"); Cst " y="; Dir([], "s")] =
     <:expr<
       printer_d
         (fun __printer ->
            __printers.(0) <-- __printer;
            printer_s
              (fun __printer ->
                 __printers.(1) <-- __printer;
                 __k (fun oc -> BatPrint.format oc __pattern __printers)))
     >>
*)
let make_printer _loc ast =
  let rec aux n = function
    | Cons(_, Cst _, l) -> aux n l
    | Cons(_loc, Dir(flags, names, dir), l) ->
        let dir = expr_of_directive _loc names dir in
        let dir = match flags with
          | None -> dir
          | Some f -> <:expr< $dir$ ~flags:$f$ >>
        in
        <:expr< $dir$ (fun __printer ->
                         __printers.($int:string_of_int n$) <- __printer;
                         $aux (n + 1) l$) >>
    | Cons(_loc, Value_printer(name, dir), l) ->
        let pid, id = match name with
          | Some(_loc, id) ->
              (Ast.PaLab(_loc, id, Ast.PaNil _loc), <:ident< $lid:id$ >>)
          | None ->
              (<:patt< __x >>, <:ident< __x >>)
        in
        <:expr< fun $pid$ ->
                  __printers.($int:string_of_int n$) <-
                     (fun __out -> $dir$ false __out $id:id$);
                  $aux (n + 1) l$ >>
    | Nil _loc ->
        <:expr< __k (fun oc -> BatPrint.format oc __pattern __printers) >>
  in
  aux 0 ast

let count_directives l =
  let rec aux n = function
    | Cons(_, (Dir _ | Value_printer _), l) ->
        aux (n + 1) l
    | Cons(_, Cst _, l) ->
        aux n l
    | Nil _ ->
        n
  in
  aux 0 l

let _ =
  register_expr_specifier "p"
    (fun ctx _loc str ->
       let ast = Parse.main (unescape _loc str) in

       (* Count the number of directives *)
       let directive_count = count_directives ast in

       (* Creates the format expression *)
       <:expr< { BatPrint.pattern = $str:String.escaped(make_pattern ast)$;
                 BatPrint.printer = (fun __pattern __k ->
                                                let __printers =
                                                  Array.create
                                                    $int:string_of_int directive_count$
                                                    Pervasives.ignore in
                                                $make_printer _loc ast$) } >>)

