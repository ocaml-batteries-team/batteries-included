(* Test : open <module> in <expr>
let () = let module OPENIN_1 = struct open M
                                         let res = foo
                                            end
  in OPENIN_1.res
*)
let () =
   open M in foo


(* Test : struct <module content> end in <expr>
let () =
  let module OPENIN_2 = struct type foo
                                let bar = ()
                                   let res = foobar
                                      end
  in OPENIN_2.res
*)
let () =
  struct type foo let bar = () end in
  foobar

(* Test : module <name> = <name_1>, <name_2> ... *)
module B = struct end
module C = struct end
module D = struct end
module E = struct end
module F = struct end
module A = B include C, D, E, F

(* Test : let module <name> = <name_1>, <name_2> in ... *)
let module A = B include C, D, E, F in ()
