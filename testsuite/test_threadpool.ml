open OUnit

open Future.ThreadPool
open Print

type pool = Future.ThreadPool.t
type 'a future = 'a thread

let spawn : pool -> ('a -> 'b) -> 'a -> 'b future = fun pool f x ->
  spawn pool f x;;
	
let wait  : 'a future  -> 'a              = fun t -> 
  join t;;

let parmap : pool -> ('a -> 'b) -> 'a list -> 'b list future = fun pool f l ->
open List in
  spawn pool (map wait) (map (spawn pool f) l);;

let f x =
  x + 1;;

let sample = List.of_enum (1 -- 1000);;
let sum l  = List.fold_left (+) 0 l;;

let test_parmap size () =
    let pool = create 15 in
  assert_equal 
    (sum (wait (parmap pool f sample)))
    (sum (List.map f sample))

let tests = "ThreadPool" >::: [
  "parmap 1" >:: test_parmap 1;
  "parmap 2" >:: test_parmap 2;
  "parmap 8" >:: test_parmap 8
]
