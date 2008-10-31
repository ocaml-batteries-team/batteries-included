print_string test
  where test = "test_where succeeded"

let enum = ( 1 -- 999 ) // odd 
 where odd x = 
   x mod 2 = 1
 and even x = 
   x mod 2 = 0

let fibo n = fst (fibo_aux n)
  where rec fibo_aux = function
    | 0 -> (1, 1)
    | 1 -> (1, 2)
    | n -> (a, a + b) where let (a, b) = fibo_aux ( n - 1 )


(** Battery of regression tests (no pun intended)

    Those tests are intended to be parsed, not compiled
    (they probably don't even compile).
    The test code is reproduced in comments to ease manual
    checking of the test output. If "camlp4 pa_where.cmo"
    raises an error while parsing the file, there is a bug
    in the implementation. If there is no error, there may
    still be a precedence/associativity issue.
*)

(** In-expression where *)

(* let _ = b where c = d *)
let _ = b where c = d

(* let _ = *)
(*   b where c = d *)
(*     where e = f *)
let _ =
  b where c = d
    where e = f

(* let _ = b where let c = d *)
let _ = b where c = d

(* let _ = *)
(*   b where let c = d *)
(*     where e = f *)
let _ =
  b where let c = d
    where e = f

(* let a = b where rec b = c *)
(* let a = b where let rec b = c *)
let a = b where rec b = c
let a = b where let rec b = c


(* let _ =  *)
(*   b where (c = d where e = f) *)
let _ = 
  b where c = (d where e = f)


(* let a = b *)
(*       where c = d *)
(*       and e = f *)
let a = b
      where c = d
      and e = f

(* let _ = *)
(*   b where c = d *)
(*       and e = f *)
(*     where g = h *)
let _ =
  b where c = d
      and e = f
    where g = h

(** Toplevel where *)

(* let a = b *)
(* where val c = d *)
let a = b
where val c = d

(* let a = b *)
(* where val c = d *)
(*       and e = f *)
(* where val g = h *)
let a = b
where val c = d
      and e = f
where val g = h

(* type a = b *)
(* where val c = d *)
type a = b
where val c = d

(** Mixing *)

(* let a = b *)
(* and c = d *)
(*       where e = f *)
let a = b
and c = d
      where e = f

(* let a = b *)
(*       where c = d *)
(* where val e = f *)
let a = b
      where c = d
where val e = f

(* () where val a = *)
(*     b *)
(*     where c = d *)
() where val a =
    b
    where c = d
