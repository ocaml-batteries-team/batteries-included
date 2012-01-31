

let rec foo x0 f = function
  [] -> x0 | x::xs -> f x (foo x0 f xs)

(*$T foo
  foo  0 ( + ) [1;2;3] = 6
  foo  0 ( * ) [1;2;3] = 0
  foo  1 ( * ) [4;5]   = 20
  foo 12 ( + ) []      = 12
*)

(*$Q foo
  Q.small_int (fun i-> foo i (+) [1;2;3] = List.fold_left (+) i [1;2;3])
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> foo i (+) l = List.fold_left (+) i l)
*)
 
(*$R foo 
  let thing = foo  1 ( * ) 
  and li = [4;5] in
  assert_bool "something_witty" (thing li = 20);
  assert_bool "something_wittier" (1=1)
*)

let rec pretentious_drivel x0 f = function [] -> x0
  | x::xs -> pretentious_drivel (f x x0) f xs

(*$T pretentious_drivel
  pretentious_drivel 1 (+) [4;5] = foo 1 (+) [4;5]
*)

(*$T pretentious_drivel as x
  x 1 (+) [4;5] = foo 1 (+) [4;5]
*)

let rec even = function 0 -> true
  | n -> odd (pred n)
and odd = function 0 -> false
  | n -> even (pred n)

(*$Q even; odd
  Q.small_int (fun n-> odd (abs n+3) = even (abs n))
  *)


(*$Q even as x ; odd as y
  Q.small_int (fun n-> y (abs n+3) = x (abs n))
*)


(*$Q foo, pretentious_drivel as x
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> x i (+) l = List.fold_left (+) i l)
*)

(*$Q foo, pretentious_drivel
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> foo i (+) l = List.fold_left (+) i l)
*)

(*$= pretentious_drivel as x  & ~printer:string_of_int
  (x 1 (+) [4;5])   (foo 1 (+) [4;5])
*)

(*$= pretentious_drivel as x  
  (x 1 (+) [4;5])   (foo 1 (+) [4;5])
*)

(* empty headers: space, nothing, explicit empty param *)
(*$T 
  2+2 = 4
*)
(*$T
  2+1 = 3
*)
(*$T &
  2+3 = 5
*)