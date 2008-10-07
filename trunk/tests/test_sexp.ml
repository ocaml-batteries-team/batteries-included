open Batteries

TYPE_CONV_PATH "Test_sexp"

type t = A | B with sexp

type u = t Data.Persistent.List.t with sexp

Languages.SExpr.output_hum System.IO.stdout (sexp_of_u [A;B;A])

