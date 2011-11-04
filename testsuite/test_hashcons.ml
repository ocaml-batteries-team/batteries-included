open OUnit
open BatHashcons

type lterm = lterm_ hobj
and lterm_ =
  | Var of string
  | App of lterm * lterm
  | Lam of string * lterm

module LtermFuncs = struct
  type t = lterm_
  let equal lt1 lt2 = match lt1, lt2 with
    | Var j, Var k -> j = k
    | App (lt11, lt12), App (lt21, lt22) ->
        lt11 == lt21 && lt12 == lt22
    | Lam (x, lt1), Lam (y, lt2) ->
        x = y && lt1 == lt2
    | _ -> false
  let hash = function
    | Var x          -> H.hc1_ 0 (Hashtbl.hash x)
    | App (lt1, lt2) -> H.hc1_ 1 (H.hc1 lt1 (H.hc0 lt2))
    | Lam (x, lt)    -> H.hc1_ 2 (H.hc1_ (Hashtbl.hash x) (H.hc0 lt))
end
module LtermHC = MakeTable (LtermFuncs)
let _tab = LtermHC.create 1
let var x       : lterm = LtermHC.hashcons _tab (Var x)
let app lt1 lt2 : lterm = LtermHC.hashcons _tab (App (lt1, lt2))
let lam x lt    : lterm = LtermHC.hashcons _tab (Lam (x, lt))

let test_identity () =
  let mk_s x y z =
    lam x begin
      lam y begin
        lam z begin
          let xz = app (var x) (var z) in
          let yz = app (var y) (var z) in
          app xz yz
        end
      end
    end
  in
  assert_bool "mk_s produces different objects"
    (mk_s "x" "y" "z" == mk_s "x" "y" "z")

let tests = "Hashcons" >::: [
  "Pointer identity" >:: test_identity
]
