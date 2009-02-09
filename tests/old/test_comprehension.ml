let seq a b = [? List : k | k <- a -- b ]

(* global module specifiers test *)
let test pow foo bar =
  [? List : pow p k | p <- List : seq 2 100; prime p; k <- Array : foo bar ]

(* list-specific test *)
let test pow prime =
  [ pow p k | p <- seq 2 100; prime p; k <- seq 1 p ]

(* guard stacking test *)
let test gen p p' =
  [? a | a <- gen; p a; p' a ]

(* pattern-in-generator tests *)
let _ = (* irrefutable *)
  [? a + b | (a, b) <- List : List.combine (seq 2 100) (seq 100 2) ]
let singletons input_list = (* refutable *)
  [? sing | [sing] <- input_list ]

(* modules erasing test *)
let test foo p = (* same modules *)
  [? List : a | a <- List : foo; p a ]
let _ = (* enum special case *)
  [? List : (a,b) | a <- Enum : 2--100; a = 5; b <- List : seq 2 100 ?]

