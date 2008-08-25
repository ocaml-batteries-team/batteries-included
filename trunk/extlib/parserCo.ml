open LazyList

type 'a state =
  | Eof
  | State of 'a

(** {3 Positions} *)
module Source =
struct

  type ('a, 'b) t = ('a * 'b) LazyList.t

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
(**    LazyList.of_enum (Enum.from (fun () -> 
				   
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

type ('a, 'b, 'c) t = ('a, 'c) Source.t -> 'b * ('a, 'c) Source.t

(*let where e = match peek e.contents with
  | None          -> Eof
  | Some (_, pos) -> Loc pos

let pos e = (where e, e)*)

(*let loc e = let loc = match where e with 
  | Loc x -> x 
  | Eof   -> loc_eof
in (loc, e)*)

(*let advance e l = {(e) with contents = l}*)

(** {3 Error-handling} *)

(*type 'a failure =
    {
      labels  : string list;
      position: 'a;
    }
exception Failed of Obj.t failure*)
exception Failed of string list

(*let fail : ('a, 'b, 'c) t = fun (e : ('a, 'c) Source.t) -> raise (Failed { labels = []; position = Obj.repr e})*)
let fail _ = raise (Failed [])

(* Primitives *)



let satisfy f e =
  match get e with
    | Some ((x,_),t) when f x -> (x, t)
    | _                       -> fail e

let label s p e =
  Printf.eprintf ">>> %s\n" s;
  flush_all ();
  try let x = p e in
    Printf.eprintf "<<< %s\n" s;
    flush_all ();
    x
  with Failed l ->
    (
      Printf.eprintf "!!! %s\n" s;
      flush_all ();
      raise (Failed (s::l))
    )

let either l e =
  let rec aux err = function
    | []   -> raise (Failed err)
    | h::t -> 
	try h e
	with (Failed labels) -> aux (err @ labels) t
  in aux [] l

let ( <|> ) p1 p2 = either [p1;p2] 

let maybe p e =
  try  let (result, rest) = p e in (Some result, rest)
  with Failed _ -> (None, e)

let (~?) = maybe

let bind m f e = 
  let (result, rest) = m e in
    f result rest

let ( >>= ) = bind

let state e = (get_state e, e)

let eof e = match get e with
  | None -> ((), e)
  | _    -> raise (Failed ["end of file"])

let any e = match get e with
  | None       -> raise (Failed ["anything"])
  | Some ((x,_),t) -> (x, t)

let return r e = (r, e)

let filter f p e =
  let (next, rest) as result = p e in
    if f next then result
    else           fail e

let exactly x = satisfy (( = ) x)

let cons p q =
  p >>= fun p_result ->
  q >>= fun q_result ->
    return (p_result::q_result)

let ( >:: ) = cons

let ( >>> ) p q =
  p >>= fun _ -> q

let zero_plus ?sep p e =
  let p' = match sep with
    | None   -> p
    | Some s -> s >>> p
  in
  let rec aux acc l = match maybe p' l with
    | (None,   rest) -> (List.rev acc, rest)
    | (Some x, rest) -> aux (x::acc) rest
  in match maybe p e with
    | (None,   rest) -> ([], rest)
    | (Some x, rest) -> aux [x] rest

let ( ~* ) p = zero_plus p

let one_plus ?sep p = p >:: 
  match sep with
    | None   -> zero_plus p
    | Some s -> zero_plus (s >>> p)

let ( ~+ ) p = one_plus p

let post_map f p e =
  let (result, rest) = p e in
    (f result, rest)

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

(*let one_of l e =
  match get e with
    | Some (x, rest) when List.exists (fun y -> (e.compare x y) = 0) l -> (x, advance e rest)
    | _                                                                -> fail e*)

(*let none_of l e = 
  match get e with
    | Some (x, rest) when List.exists (fun y -> e.compare x y = 0) l -> fail e
    | Some (x, rest)                                                 -> (x, advance e rest)
    | _                                                              -> fail e*)

let range a b = satisfy (fun x -> a <= x && x <= b)

let prefix suffix l =
  let rec aux acc rest =
    match get rest with
      | None                         -> []
      | Some (h, t) when t == suffix -> List.rev (h::acc)
      | Some (h, t)                  -> aux (h::acc) t
  in aux [] l

let scan p e =
  let (_, rest) = p e in (*First proceed with parsing*)
    (List.map fst (prefix rest e), rest)

let enum_runs p e = Enum.unfold e
  (fun x -> if LazyList.is_empty x then None
   else Some (p x) )

let list_runs p e = LazyList.unfold e
  (fun x -> if LazyList.is_empty x then None
   else try Some (p x) with
     | Failed _ -> None)

let to_lazy_list_map p e =
  LazyList.unfold e
    (fun x -> if LazyList.is_empty x then None
              else Some (p x))

let to_enum_map p e =
  LazyList.unfold e
    (fun x -> if LazyList.is_empty x then None
              else Some (p x))

(*
let to_list_map p ?newline e =
  let e' = put_loc ?newline e in
  LazyList.unfold e' 
    (fun x -> if LazyList.is_empty x then None
              else Some (p x) )*)

let sat f e =
  match get e with
    | Some ((x,_),t) when f x -> ((), t)
    | _                   -> fail e

let lookahead p e =
  (fst ((maybe p) e), e)

let run p e = 
  try    Std.Ok (fst (p e))
  with   Failed f -> Std.Error (get_state e, f)

let enum_runs (p:('a, 'b, 'c) t) e =
  Enum.unfold e (fun e -> if LazyList.is_empty e then None
		 else try Some (p e)
		 with Failed _ -> None)

let list_runs p e =
  LazyList.of_enum (enum_runs p e)

(*let compose (p:('a, 'b, 'c) t) (q:('b, 'd, 'c) t) : ('a, 'd, 'c) t = fun (e:('a, 'c) Source.t) ->
  let e' = unfold e (fun (e:('a, 'c) Source.t) -> match peek e with
		       | None        -> None
		       | Some (a, c) -> Some (assert false))
  in e'*)
(*  q (unfold e (fun e -> match peek e with
		 | None       -> assert false
		 | Some (a,c) -> assert false))*)
(*(Some (c, p e))))*)

