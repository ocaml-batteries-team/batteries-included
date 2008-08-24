open LazyList



(** {3 Positions} *)

type loc =
    {
      offset : int;
      line   : int
    }

type position = 
  | Eof
  | Loc of loc


module Source =
struct

  type 'a t = 
      {
	pos:      'a -> loc;
	compare:  'a -> 'a -> int;
	contents: 'a LazyList.t
      }

  let of_lazy_list l =
    { pos = (fun _ -> {offset = -1; line = -1});
      compare = Pervasives.compare;
      contents= l }

  let of_enum l =
    of_lazy_list (of_enum l)

  let map f t =
    {(t) with contents = LazyList.map f t.contents}

  let pos_offset   e =
    let offset = ref 0 in
      { contents = 
	  LazyList.map (
	    (fun x -> let result = (x, {offset = !offset; line = -1}) in
	       incr offset; result)) e.contents;
	compare = (fun (x,_) (y,_) -> e.compare x y);
	pos     = snd}

  let pos_newlines nl (e:'a t) : ('a * loc) t =
    let offset = ref 0
    and line   = ref 1 in
      { contents = 
	  LazyList.map (
	    (fun x -> let result = (x, {offset = !offset; line = !line}) in
	     let _ = if nl x then (offset := 0; incr line ) 
	     else          incr offset
	     in result) )
	    e.contents;
	compare = (fun (x,_) (y,_) -> e.compare x y);
	pos     = snd}

  let set_pos e pos =
    { (e) with pos = pos}

  let set_compare e cmp =
    { (e) with compare = cmp}
end

open Source

type 'a source = 'a Source.t

type ('a, 'b) t = 'a source -> 'b * 'a source

let where e = match peek e.contents with
  | None   -> Eof
  | Some x -> Loc (e.pos x)

let pos e = (where e, e)

let loc e = (e.loc, e)

let advance e l = {(e) with contents = l}

(** {3 Error-handling} *)

type failure =
    {
      labels  : string list;
      position: position;
    }
exception Failed of failure

let fail e = raise (Failed { labels = []; position = where e})

(* Primitives *)



let satisfy f e =
  match get e.contents with
    | Some (x,t) when f x -> (x, advance e t)
    | _                   -> fail e

let label s p e =
  Printf.eprintf ">>> %s\n" s;
  flush_all ();
  try let x = p e in
    Printf.eprintf "<<< %s\n" s;
    flush_all ();
    x
  with Failed {labels = labels; position = loc} -> 
    (
      Printf.eprintf "!!! %s\n" s;
      flush_all ();
      raise (Failed {labels = [s]; position = loc})
    )

(*  try p e 
  with Failed {labels = labels; position = loc} -> raise (Failed {labels = [s]; position = loc})*)

let either (l:('a, 'b) t list) : ('a, 'b) t = fun e ->
  let rec aux err = function
  | []   -> raise (Failed { position = where e; labels = err })
  | h::t -> 
      try h e
      with Failed {labels = labels} -> aux (err @ labels) t
  in aux [] l

let ( <|> ) (p1:('a, 'b) t) (p2:('a, 'b) t) : ('a, 'b) t = fun e -> either [p1;p2] e

let aggressive l e =
  let rec aux err = function
    | []   -> raise (Failed { position = where e; labels = err })
    | h::t ->
	try h e
	with Failed {labels = labels; position = loc} when where e = loc ->
	  aux (err @ labels) t
  in aux [] l
	  
let ( <!> ) (p1:('a, 'b) t) (p2:('a, 'b) t) : ('a, 'b) t = fun e -> aggressive [p1;p2] e


let maybe p e =
  try  let (result, rest) = p e in (Some result, rest)
  with Failed _ -> (None, e)

let (~?) = maybe

let bind m f e = 
  let (result, rest) = m e in
    f result rest

let ( >>= ) = bind


   

(*  let (result, rest) = p e in*)
    

let eof e = match get e.contents with
  | None -> ((), e)
  | _    -> raise (Failed {labels = ["end of file"]; position = where e})

let any e = match get e.contents with
  | None       -> raise (Failed {labels = ["anything"]; position = Eof})
  | Some (x,t) -> (x, advance e t)

let return r e = (r, e)

let filter (f:'b -> bool) (p:('a, 'b) t) (e:'a source) =
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

let map (f:'b -> 'c) (p:('a, 'b) t) (e:'a source) = 
  let (result, rest) = p e in
    (f result, rest)

let times (n:int) (p:('a, 'b) t) : ('a, 'b list) t=
  let rec aux acc i = if i > 0 then p >>= fun x -> (aux (x::acc) ( i - 1 ))
	   else return acc
  in (aux [] n) >>= fun x -> return (List.rev x)

let ( ^^ ) p n = times n p

let one_of l e =
  match get e.contents with
    | Some (x, rest) when List.exists (fun y -> (e.compare x y) = 0) l -> (x, advance e rest)
    | _                                                                -> fail e

let none_of l e = 
  match get e.contents with
    | Some (x, rest) when List.exists (fun y -> e.compare x y = 0) l -> fail e
    | Some (x, rest)                                               -> (x, advance e rest)
    | _                                                            -> fail e

let range a b = satisfy (fun x -> a <= x && x <= b)

let scan (p:('a, _) t) : ('a, 'a list) t = fun e ->
  let (_, rest) = p e in (*First proceed with parsing*)
  let (extract:'a LazyList.t) =
    match get rest.contents with
      | None        -> e.contents (*Take the rest of the list*)
      | Some (x, (t:'a LazyList.t)) -> let pos = e.pos x in
	  take_while (fun (x':'a) -> (e.pos x') <> pos) t
  in (to_list extract, rest)

let enum_runs (p:('a, 'b) t) e = Enum.unfold e
  (fun x -> if LazyList.is_empty x.contents then None
   else Some (p x) )

let list_runs p e = LazyList.unfold e
  (fun x -> if LazyList.is_empty x.contents then None
   else Some (p x) )

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
  match get e.contents with
    | Some (x,t) when f x -> ((), advance e t)
    | _                   -> fail e

let lookahead p e =
  (fst ((maybe p) e), e)

let run (p: ('a, 'b) t) e =
  try    Std.Ok (fst (p e))
  with   Failed f -> Std.Error f
