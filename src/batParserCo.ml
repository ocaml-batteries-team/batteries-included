open BatList
open BatString
open List
open BatLazyList
open BatIO
open BatPrintf

type 'a state =
  | Eof
  | State of 'a

type 'a report =  Report of ('a state * string * 'a report) list

let ( &&& ) (Report l) (Report l') = Report (l @ l')

let debug_mode = ref false

(** {3 Positions} *)
module Source =
struct

  type ('a, 'b) t = ('a * 'b) BatLazyList.t

  let of_lazy_list l init f =
    let rec aux l acc = match get l with
      | None        -> nil
      | Some (h, t) ->
        let acc' = f h acc in
        lazy( Cons ((h, acc'), (aux t acc')))
    in aux l init

  let of_enum l =
    of_lazy_list (of_enum l)

  open Lexing

  (**TODO: Handle EOF !*)
  let of_lexer l = assert false
  (**    LazyList.of_enum (BatEnum.from (fun () ->

			 l.refill_buff l;
		        (l.lex_buffer, (l.lex_start_p, l.lex_curr_p))))*)

  let get_state l = match peek l with
    | Some (_, s) -> State s
    | None        -> Eof

  let set_full_state l init f =
    let rec aux l acc = match get l with
      | None        -> nil
      | Some ((h, _), t) ->
        let acc' = f h acc in
        lazy( Cons ((h, acc'), (aux t acc')))
    in aux l init
end

open Source

type ('a, 'b, 'c) result =
  | Success    of 'b * ('a, 'c) Source.t             (**Succeed and consume.*)
  | Backtrack  of 'b * 'c report * ('a, 'c) Source.t (**Succeed because of backtracking, typically without consuming.*)
  | Setback    of 'c report                          (**Error, backtracking in progress.*)
  | Failure    of 'c report                          (**Fatal error.*)


type ('a, 'b, 'c) t =   ('a, 'c) Source.t -> ('a, 'b, 'c) result

let apply p e = p e(**To improve reusability*)

(** {3 Error-handling} *)


(*exception Backtrack of Obj.t report*)
(**Recoverable error.

   These errors are caused by [fail].*)

(*exception Fail      of Obj.t report*)
(**Fatal error.

   These errors are caused by [must].*)

let fail        e     = Setback (Report [])
let succeed     v e   = Success (v, e)
let backtracked v r e = Backtrack (v, r, e)
let return            = succeed
let fatal       e     = Failure (Report [])
(* Primitives *)



let satisfy f e = match get e with
  | Some ((x,_),t) when f x -> succeed x t
  | _                       -> fail e

let depth = ref 0
let label s p e =
  if BatString.is_empty s then
    match apply p e with
    | Success _ as x      -> x
    | Setback c           -> Setback (Report [])
    | Failure c           -> Failure (Report [])
    | Backtrack (b, c, t) -> Backtrack (b, Report [], t)
  else
    let make_report c = Report [get_state e, s, c] in
    if !debug_mode then
      begin
        eprintf "%*s>>> %s\n" !depth " " s;
        incr depth;
        flush_all ()
      end;
    match apply p e with
    | Success _ as x ->
      if !debug_mode then begin
        decr depth;
        eprintf "%*s<<< %s\n" !depth " " s;
        flush_all ()
      end;
      x
    | Setback c ->
      if !debug_mode then begin
        decr depth;
        eprintf "%*s^^^ %s\n" !depth " " s;
        flush_all ()
      end;
      Setback (make_report c)
    | Failure c ->
      if !debug_mode then begin
        decr depth;
        eprintf "%*s!!! %s\n" !depth " " s;
        flush_all ()
      end;
      Failure (make_report c)
    | Backtrack (b, c, t) ->
      if !debug_mode then begin
        decr depth;
        eprintf "%*s/// %s\n" !depth " " s;
        flush_all ()
      end;
      Backtrack (b, make_report c, t)

let must p e = match apply p e with
  | Setback x -> Failure x
  | y         -> y

let should p e = match apply p e with
  | Failure x -> Setback x
  | y         -> y

let either l e =
  let rec aux err = function
    | []   -> Setback (Report err)
    | h::t -> match apply h e with
      | Success   _
      | Failure   _
      | Backtrack (_, _, _) as result -> result
      | Setback (Report labels)       -> aux (err @ labels) t
  in aux [] l

let ( <|> ) p1 p2 = either [p1;p2]

let maybe p e = match apply p e with
  | Setback c                        -> Backtrack (None, c, e)
  | Success (result, rest)           -> Success   (Some result, rest)
  | Backtrack (result, report, rest) -> Backtrack (Some result, report, rest)
  | Failure _ as result              -> result

let (~?) = maybe

(*
  [bind m f e]
  If [m] succeeded by backtracking and [f] fails or
  succeeds by backtracking, merge the reports of [m] and [f].
*)
let bind m f e = match apply m e with
  | Setback _ | Failure _ as result -> result
  | Success   (result, rest)        -> apply f result rest
  | Backtrack (result, report, rest)     ->
    match apply f result rest with
    | Backtrack (result', report', rest') -> Backtrack (result', report &&& report', rest')
    | Setback   report'                   -> Setback (report &&& report')
    | Failure   report'                   -> Failure (report &&& report')
    | Success _ as result                 -> result

let ( >>= ) = bind

let ( >>> ) p q =
  p >>= fun _ -> q

let cons p q =
  p >>= fun p_result ->
  q >>= fun q_result ->
  return (p_result::q_result)

let ( >:: ) = cons

let state e = succeed (get_state e) e

let eof e = label "End of file" (fun e -> match get e with
    | None -> succeed () e
    | _    -> fail e) e

let any e = label "Anything" (fun e -> match get e with
    | None           -> fail e
    | Some ((x,_),t) -> succeed x t) e



let zero_plus ?sep p e =
  let p' = match sep with
    | None   -> p
    | Some s -> s >>> p
  in
  let rec aux acc l = match apply p' l with
    | Success   (x, rest)              -> aux (x::acc) rest
    | Backtrack (result, report, rest) -> backtracked (List.rev (result::acc)) report rest
    | Setback   report                 -> backtracked (List.rev acc) report l
    | Failure _ as result              -> result
  in match apply p e with
  | Success   (x, rest)              -> aux [x] rest
  | Backtrack (result, report, rest) -> backtracked [result] report rest
  | Setback   report                 -> backtracked []       report e
  | Failure _ as result              -> result

let ( ~* ) p = zero_plus p

let ignore_zero_plus ?sep p e =
  let p' = match sep with
    | None   -> p
    | Some s -> s >>> p
  in
  let rec aux l = match apply p' l with
    | Success   (x, rest)              -> aux rest
    | Backtrack (result, report, rest) -> backtracked () report rest
    | Setback   report                 -> backtracked () report l
    | Failure _ as result              -> result
  in match apply p e with
  | Success   (_, rest)              -> aux rest
  | Backtrack (result, report, rest) -> backtracked () report rest
  | Setback   report                 -> backtracked () report e
  | Failure _ as result              -> result

let one_plus ?sep p = p >::
    match sep with
    | None   -> zero_plus p
    | Some s -> zero_plus (s >>> p)

let ( ~+ ) p = one_plus p

let ignore_one_plus ?sep p = p >>>
  match sep with
  | None   -> ignore_zero_plus p
  | Some s -> ignore_zero_plus (s >>> p)

(** [prefix t l] returns [h] such that [[h::t] = l]*)
let prefix suffix l =
  let rec aux acc rest = match get rest with
    | None                         -> []
    | Some (h, t) when t == suffix -> List.rev (h::acc)
    | Some (h, t)                  -> aux (h::acc) t
  in aux [] l

let scan p e =
  let just_prefix rest = List.map fst (prefix rest e) in
  match apply p e with (*First proceed with parsing*)
  | Success (result, rest)           -> succeed (just_prefix rest) rest
  | Backtrack (result, report, rest) -> backtracked (just_prefix rest) report rest
  | Setback _ | Failure _ as result  -> result

let lookahead p e = match apply p e with
  | Setback c                        -> Backtrack (None, c, e)
  | Success (result, _)              -> Success   (Some result, e)
  | Backtrack (result, report, _)    -> Backtrack (Some result, report, e)
  | Failure _ as result              -> result

let interpret_result = function
  | Setback f | Failure f                -> BatInnerPervasives.Bad f
  | Success (r, _) | Backtrack (r, _, _) -> BatInnerPervasives.Ok r

let suspend : ('a, 'b, 'c) t -> ('a, (unit -> ('b, 'c report) BatInnerPervasives.result), 'c) t = fun s e ->
  let resume () = interpret_result (s e) in
  Success (resume, e)

let run p e = interpret_result (apply p e)

let source_map p e =
  let rec aux e = match peek e with
    | None        -> nil
    | Some (_, c) -> match apply p e with
      | Success   (result, rest)    -> lazy (Cons ((result, c), (aux rest)))
      | Backtrack (result, _, rest) -> lazy (Cons ((result, c), (aux rest)))
      | Setback _ | Failure _       -> nil (*@TODO: improve error reporting !*)
  in aux e


(**
   {3 Utilities}
*)
let filter f p =
  p >>= fun x ->
  if f x then return x
  else        fail

let exactly x = satisfy (( = ) x)


let post_map f p =
  p >>= fun x -> return (f x)

let times n p =
  let rec aux acc i = if i > 0 then p >>= fun x -> (aux (x::acc) ( i - 1 ))
    else return acc
  in (aux [] n) >>= fun x -> return (List.rev x)

let ( ^^ ) p n = times n p

let one_of l e =
  let exists x = List.exists (( = ) x) l in
  satisfy exists e

let none_of l e =
  let for_all x = List.for_all (( <> ) x) l in
  satisfy for_all e

let range a b = satisfy (fun x -> a <= x && x <= b)

let sat f = (satisfy f) >>> return ()

module Infix = struct
  let (<|>), (~?), (>>=), (>>>), (>::), ( ~* ), (~+), (^^) = (<|>), (~?), (>>=), (>>>), (>::), ( ~* ), (~+), (^^)
end
