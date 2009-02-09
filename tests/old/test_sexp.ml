(*open Batteries*)

TYPE_CONV_PATH "Test_sexp"

type t = A | B with sexp

type u = t Data.Persistent.List.t with sexp

let (stdout:'a System.IO.output) = stdout;;

Languages.SExpr.output_hum stdout (sexp_of_u [A;B;A])

