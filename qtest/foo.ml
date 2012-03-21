
(** This is the test file to check that qtest works as expected...
  ... because the tester needs to be tested as well. *)

let rec foo x0 f = function
  [] -> x0 | x::xs -> f x (foo x0 f xs)

(*$T foo
  foo  0 ( + ) [1;2;3] = 6  (* hehe *)
  foo  0 ( * ) [1;2;3] = 0  (* haha (*hoho *) *)
  foo  1 ( * ) [4;5]   = 20
  foo 12 ( + ) []      = 12
*)

(*$T foo
  foo 1 ( * ) [4;5] = foo 2 ( * ) [1;5;2]
*)

(*$= foo & ~printer:string_of_int
  (foo 1 ( * ) [4;5]) (foo 2 ( * ) [1;5;2])
*)

(*$Q foo
  Q.small_int (fun i-> foo i (+) [1;2;3] = List.fold_left (+) i [1;2;3])
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> foo i (+) l = List.fold_left (+) i l)
*)
 
(*$R foo 
  let thing = foo  1 ( * ) 
  and li = [4;5] in
  assert_bool "something_witty" (thing li = 20);
   (* pertinent comment *)
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


(*$Q forall x in [foo; pretentious_drivel]
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> x i (+) l = List.fold_left (+) i l)
*)

(*$Q forall foo in [foo; pretentious_drivel] & ~count:500
  (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> foo i (+) l = List.fold_left (+) i l)
*)

(*$= pretentious_drivel as x  & ~printer:string_of_int
  (x 1 (+) [4;5])   (foo 1 (+) [4;5])
*)

(* first argument to an equality passed in parameter *)
(*$=  & ~printer:string_of_int   10
  (foo 1 (+) [5;4])
 
  (foo 1 (+) [4;5])
*)

module Foomod : sig
  val bar : string
(*   val baz : string *)
end = struct
(*$< Foomod *)
  let bar = "bar"
  (*$T bar
    bar.[0] = 'b'
  *)

  (* TODO injection numbering is not right... yet it seems to be ?!? *)
  
  (*${*)
  let baz = "baz"
  (*$}*)
  
  (*$begin:inject*)
  let baz = "boz"
  (*$end:inject*)
  
  (*$T baz
    baz.[2] = 'z'
  *)

  (*$inject let brom = baz *)
  (*$T brom
    brom.[2] = 'z'
  *)

  (* global open *)
  (*$open List, Array *)

  (*${*)
    open Set;;
    open Sys;;
  (*$}*)
  
(*$>*)
end
  (* $T bar
    bar.[0] = 'b'
  *)


module Zooo = struct 
(*$begin:open Zooo *)
  let myplus = (+)
  (*$T myplus
    myplus 4 9 = 13
  *)
(*$end:open*)
end

  (*$T &
    Foomod.bar.[0] = 'b'
  *)

(*$T & 6 =
  2*3
  4+2
*)

(*$= pretentious_drivel as x  
  (x 1 (+) [4;5])   (foo 1 (+) [4;5])
*)
  
(* empty headers: space, nothing, explicit empty param *)
(*$T &
  2+2 = 4 (* some comment *)
*)
(*$T
  2+1 = 3
*)
(*$T &
             1    = 2-1
            2+3 \
              = \
              \
              5
  
  1+1=2
*)

let fuz x = x
let rec flu = function
  | [] -> []
  | x :: l -> if List.mem x l then flu l else x :: flu l
  
(*
(*$Q fuz; flu &  ~small:List.length\
  & ~count:100 \
  & (* test *)
  (Q.list Q.small_int) (fun x -> fuz x = flu x)
*)
*)

let strange_string = " \"
(*$Q fuz; flu &  ~small:List.length\
  & ~count:100 \
  & (* test *)
  (Q.list Q.small_int) (fun x -> fuz x = flu x)
*)
"

(*$T & 6 \
  & =
  2*3
*)