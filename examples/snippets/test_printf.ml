open Print

let _ =
  (* Simple test *)
  printf p"x = (%d, %s)\n" 1 "a";

  (* Test with labelled directives: *)
  printf p"Hello %(name:s), i am ocaml version %(version:s)\n%!"
    ~name:(try Sys.getenv "USER" with _ -> "toto")
    ~version:Sys.ocaml_version;

  (* A custom printer: *)
  let pdir_o k obj = k obj#print in

  printf p"obj = %o\n" (object
                          method print oc = IO.nwrite oc "plop"
                        end);

  (* A printer with multiple argument: *)
  let pdir_test k x y z =
    k (fun oc ->
         IO.write oc '(';
         IO.nwrite oc (string_of_int x);
         IO.nwrite oc ", ";
         IO.nwrite oc (string_of_int y);
         IO.nwrite oc ", ";
         IO.nwrite oc (string_of_int z);
         IO.write oc ')')
  in

  printf p"x = %test\n" 1 2 3;

  (* Labelled directives with multiple argument: *)
  printf p"x = %(x,y,z:test)\n" ~x:1 ~y:2 ~z:2;
  printf p"x = %(x,_,z:test)\n" ~x:1 2 ~z:2
