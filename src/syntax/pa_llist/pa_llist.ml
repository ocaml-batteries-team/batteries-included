(* Small extension to provide lazy pattern matching.  *)
open Camlp4.PreCast;;
open Syntax;;

EXTEND Gram
  GLOBAL: patt expr ;

  expr: LEVEL "simple"
    [ [ "[%"; "]" -> <:expr< lazy BatLazyList.Nil >> ] ];

  patt: LEVEL "simple"
    [ [ "[%"; "]" -> <:patt< lazy BatLazyList.Nil >> ] ];
  patt: LEVEL "::"
    [ [ p1 = SELF; "%::"; p2 = SELF ->
        <:patt< lazy BatLazyList.Cons($p1$,$p2$) >> ] ] ;
END
