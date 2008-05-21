open LazyList

type actual_loc     =
    {
      offset : int;
      line   : int
    }

type loc =
  | Eof 
  | Loc of actual_loc

type failure =
    {
      labels : string list;
      loc    : loc
    }

exception Failure of failure
  (** Exception raised in case of parse failure.*)

type 'a source = ('a * int * int) LazyList.t
  (** A list of characters / offsets / lines *)

type ('a, 'b) t = 'a source -> ('b * 'a source)
  (** The type of a parser.
      Its evaluation may raise [Failure] *)

let fail_at = function
    Some ((_, offset, line), _) -> raise (Failure {labels = []; loc = Loc {offset = offset; line = line}})
  | None                        -> raise (Failure {labels = []; loc = Eof})

let fail_at_loc loc = raise (Failure {labels = []; loc = loc})

let get_loc e =
  match peek e with
    | Some(_, offset,line) -> Loc {offset = offset; line = line}
    | None -> Eof

let fail e = fail_at_loc (get_loc e)

(* Primitives *)

let satisfy f e =
  match get e with
    | Some ((x,_,_),t) when f x -> (x, t)
    | _ as y                    -> fail_at y

let label s p e =
  Printf.eprintf ">>> %s\n" s;
  flush_all ();
  try let x = p e in
    Printf.eprintf "<<< %s\n" s;
    flush_all ();
    x
  with Failure {labels = labels; loc = loc} -> 
    (
      Printf.eprintf "!!! %s\n" s;
      flush_all ();
      raise (Failure {labels = [s]; loc = loc})
    )

(*  try p e 
  with Failure {labels = labels; loc = loc} -> raise (Failure {labels = [s]; loc = loc})*)

let either (l:('a, 'b) t list) : ('a, 'b) t = fun e ->
  let rec aux err = function
  | []   -> raise (Failure { loc = get_loc e; labels = err })
  | h::t -> 
      try h e
      with Failure {labels = labels} -> aux (err @ labels) t
  in aux [] l

let ( <|> ) (p1:('a, 'b) t) (p2:('a, 'b) t) : ('a, 'b) t = fun e -> either [p1;p2] e

let aggressive l e =
  let rec aux err = function
    | []   -> raise (Failure { loc = get_loc e; labels = err })
    | h::t ->
	try h e
	with Failure {labels = labels; loc = loc} when get_loc e = loc ->
	  aux (err @ labels) t
  in aux [] l
	  
let ( <!> ) (p1:('a, 'b) t) (p2:('a, 'b) t) : ('a, 'b) t = fun e -> aggressive [p1;p2] e


let maybe p e =
  try  let (result, rest) = p e in (Some result, rest)
  with Failure _ -> (None, e)

let (~?) = maybe




let bind m f e = 
  let (result, rest) = m e in
    f result rest

let ( >>= ) = bind

let eof e = match get e with
  | None -> ((), e)
  | _    -> raise (Failure {labels = ["end of file"]; loc = get_loc e})

let any e = match get e with
  | None             -> raise (Failure {labels = ["anything"]; loc = Eof})
  | Some ((x,_,_),t) -> (x, t)

let return r e = (r, e)

let filter (f:'b -> bool) (p:('a, 'b) t) (e:'a source) =
  let (next, rest) as result = p e in
    if f next then result
    else           fail_at_loc (get_loc e)

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
  match get e with
    | Some ((x, _, _), rest) when List.mem x l -> (x, rest)
    | _ as y                                   -> fail_at y

let none_of l e = 
  match get e with
    | Some ((x, _, _), rest) as y when List.mem x l -> fail_at y 
    | None                                          -> fail_at None
    | Some ((y, _, _), rest)                        -> (y, rest)

let range a b = satisfy (fun x -> a <= x && x <= b)

let clean_up e =
  LazyList.map (fun (x, _, _) -> x) e

let scan (p:('a, _) t) : ('a, 'a list) t = fun e ->
  let (_, rest) = p e in (*First proceed with parsing*)
  let (extract:'a source) =
    match get rest with
      | None                -> e (*Take the rest of the list*)
      | Some ((_, i, j), _) -> take_while (fun (_, i', j') -> i <> i' || j <> j') e
  in (to_list (clean_up extract), rest)


let put_loc ?newline l = (*Label a lazy list with offsets and line numbers*)
  let offset = ref 0
  and line   = ref 1 in
    LazyList.map (
      match newline with
	| Some nl ->
	    (fun x -> let result = (x, !offset, !line) in
	       if x = nl then 
		 (
		   offset := 0;
		   incr line
		 ) else incr offset;
	       result) 
	| None ->
	    (fun x -> let result = (x, !offset, -1) in incr offset; result) 
    )  l


let run (p:('a, 'b) t) ?newline e =
  let e' = put_loc ?newline (of_enum e)  in fst (p e')

let run_filter (p:('a, 'b) t) ?newline (e:'a Enum.t) =
  let e' = put_loc ?newline (of_enum e) in
  Enum.unfold e' 
    (fun x -> if LazyList.is_empty x then None
              else Some (p x) )

let run_filter_list p ?newline e =
  let e' = put_loc ?newline e in
  LazyList.unfold e' 
    (fun x -> if LazyList.is_empty x then None
              else Some (p x) )


let sat f e =
  match get e with
    | Some ((x,_,_),t) when f x -> ((), t)
    | _ as y                    -> fail_at y

let lookahead p e =
  (fst ((maybe p) e), e)

