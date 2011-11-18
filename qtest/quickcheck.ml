(** Adapted from the Jane Street Capital Core quickcheck.ml,
    licensed as LGPL + linking exception *)
(** Module for easily generating unit tests.  Based on code posted by
    padiolea\@irisa.fr to the caml mailing list. *)

open Printf

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let sum_int = List.fold_left (+) 0

let (==>) b1 b2 = if b1 then b2 else true (* could use too => *)



(* Value generators *)


type 'a gen = unit -> 'a

let ug () = ()

let bg () = Random.bool ()

let fg () = 
  exp (Random.float 15. *. (if Random.float 1. < 0.5 then 1. else -1.))
  *. (if Random.float 1. < 0.5 then 1. else -1.)

let pfg () = abs_float (fg ())
let nfg () = -.(pfg ())

(* natural number generator *)
let nng () = 
  let p = Random.float 1. in
  if p < 0.5 then Random.int 10
  else if p < 0.75 then Random.int 100
  else if p < 0.95 then Random.int 1_000
  else Random.int 10_000

let neg_ig () = -(nng ())

(* Uniform random int generator *)
let upos = 
  if Sys.word_size = 32 then 
    fun () -> Random.bits ()
  else (* word size = 64 *)
    fun () -> 
      Random.bits ()                        (* Bottom 30 bits *)
      lor (Random.bits () lsl 30)           (* Middle 30 bits *)
      lor ((Random.bits () land 3) lsl 60)  (* Top 2 bits *)  (* top bit = 0 *)
	
let uig () = if Random.bool () then - upos () - 1 else upos ()

let lg_size size gen () =
  foldn ~f:(fun acc _ -> (gen ())::acc) ~init:[] (size ())
let lg gen () = lg_size nng gen ()

let ag_size size gen () =
  Array.init (size ()) (fun _ -> gen ())
let ag gen () = ag_size nng gen ()

let pg gen1 gen2 () = (gen1 (), gen2 ())

let tg g1 g2 g3 () = (g1 (),g2 (), g3 ())


let cg () = char_of_int (Random.int 255)

let printable_chars =
  let l = 126-32+1 in
  let s = String.create l in
  for i = 0 to l-2 do
    s.[i] <- char_of_int (32+i)
  done;
  s.[l-1] <- '\n';
  s

let printable () = printable_chars.[Random.int (String.length printable_chars)]
let numeral () = char_of_int (48 + Random.int 10)

let sg_size ?(gen = cg) size () =
  let s = String.create (size ()) in
  for i = 0 to String.length s - 1 do
    s.[i] <- gen ()
  done;
  s
let sg ?gen () = sg_size ?gen nng ()

(* Additional pretty-printers *)

let pp_list pp l = "[" ^ (String.concat "; " (List.map pp l)) ^ "]"
let pp_array pp l = "[|" ^ (String.concat "; " (Array.to_list (Array.map pp l))) ^ "|]"
let pp_pair p1 p2 (t1,t2) = "(" ^ p1 t1 ^ ", " ^ p2 t2 ^ ")"
let pp_triple p1 p2 p3 (t1,t2,t3) = "(" ^ p1 t1 ^ ", " ^ p2 t2 ^ ", " ^ p3 t3 ^ ")"



(* Generator * pretty-printer pairs *)

let unit = (ug, fun _ -> "()")

let bool = (bg, string_of_bool)

let float = (fg, string_of_float)
let pos_float = (pfg, string_of_float)
let neg_float = (nfg, string_of_float)

let int = (uig, string_of_int)
let pos_int = (upos, string_of_int)
let small_int = (nng, string_of_int)
let neg_int = (neg_ig, string_of_int)

let char = (cg, sprintf "%C")
let printable_char = (printable, sprintf "%C")
let numeral_char = (numeral, sprintf "%C")

let string_gen_of_size size gen = (sg_size ~gen size, sprintf "%S")
let string_gen gen = (sg ~gen, sprintf "%S")

let string = string_gen cg
let string_of_size size = string_gen_of_size size cg

let printable_string = string_gen printable
let printable_string_of_size size = string_gen_of_size size printable

let numeral_string = string_gen numeral
let numeral_string_of_size size = string_gen_of_size size numeral

let list (gen,pp) = (lg gen, pp_list pp)
let list_of_size size (gen,pp) = (lg_size gen, pp_list pp)

let array (gen,pp) = (ag gen, pp_array pp)
let array_of_size size (gen,pp) = (ag_size gen, pp_array pp)

let pair (g1,p1) (g2,p2) = (pg g1 g2, pp_pair p1 p2)
let triple (g1,p1) (g2,p2) (g3,p3) = (tg g1 g2 g3, pp_triple p1 p2 p3)



(* Generator combinators *)


(** given a list, returns generator that picks at random from list *)
let oneofl xs () = 
  List.nth xs (Random.int (List.length xs))

(** Given a list of generators, returns generator that randomly uses one of the generators
    from the list *)
let oneof xs =
  List.nth xs (Random.int (List.length xs))

(** Generator that always returns given value *)
let always x () = x

(** Given list of [(frequency,value)] pairs, returns value with probability proportional
    to given frequency *)
let frequency xs = 
  let sums = sum_int (List.map fst xs) in
  let i = Random.int sums in
  let rec aux acc = function 
    | ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs 
    | _ -> failwith "frequency" 
  in
  aux 0 xs

(** like frequency, but returns generator *)
let frequencyl l = frequency (List.map (fun (i,e) -> (i,always e)) l)



(* Laws *)


(** [laws iter gen func] applies [func] repeatedly ([iter] times) on output of [gen], and
    if [func] ever returns false, then the input that caused the failure is returned
    optionally.  *)
let rec laws iter gen func =
  if iter <= 0 then None
  else
    let input = gen () in
    try 
      if not (func input) then Some input 
      else laws (iter-1) gen func
    with _ -> Some input

let default_count = 500

(** Like laws, but throws an exception instead of returning an option.  *)
let laws_exn ?(count=default_count) name (gen,pp) func =
  match laws count gen func with
    | None -> ()
    | Some i -> failwith (Printf.sprintf "law %s failed for %s" name (pp i))

let rec statistic_number = function
  | []    -> []
  | x::xs -> let (splitg, splitd) = List.partition (fun y -> y = x) xs in
    (1 + List.length splitg, x) :: statistic_number splitd

(* in percentage *)
let statistic xs =
  let stat_num = statistic_number xs in
  let totals = sum_int (List.map fst stat_num) in
  List.map (fun (i, v) -> ((i * 100) / totals), v) stat_num
    
let laws2 iter func gen =
  let res = foldn ~init:[] iter 
    ~f:(fun acc _ -> let n = gen () in (n, func n) :: acc)
  in
  let stat = statistic (List.map (fun (_, (_, v)) -> v) res) in
  let res = List.filter (fun (_, (b, _)) -> not b) res in
  if res = [] then (None, stat) else (Some (fst (List.hd res)), stat)
