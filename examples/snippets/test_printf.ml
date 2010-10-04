open Batteries
open Print

let _ =
  (* Simple test *)
  printf p"x = (%d, %s)\n" 1 "a";

  (* With flags: *)
  printf p"x = %04x\n" 42;

  (* Test with labelled directives: *)
  printf p"Hello %(name:s), i am ocaml version %(version:s)\n%!"
    ~name:(try Sys.getenv "USER" with _ -> "toto")
    ~version:Sys.ocaml_version;

  (* Printing an object: *)
  printf p"o = %obj\n"
    (object(self)
       method print oc = fprintf oc p"<object:id = %u>" (Oo.id self)
     end);

  (* Printing a list: *)
  printf p"l = %{int option list}\n" [Some 1; None; Some 2];

  (* A custom directive, printing pair of integers: *)
  let printer_foo k (x, y) = k (fun oc -> fprintf oc p"(%d, %d)" x y) in

  printf p"pair = %foo\n" (42, 1024);

  (* A custom directive, taking multiple arguments: *)
  let printer_test k x y z = k (fun oc -> fprintf oc p"(%d, %d, %d)" x y z) in

  printf p"x = %test\n" 1 2 3;

  (* Labelled directives with multiple argument: *)
  printf p"x = %(x,y,z:test)\n" ~x:1 ~y:2 ~z:2;
  printf p"x = %(x,_,z:test)\n" ~x:1 2 ~z:2
