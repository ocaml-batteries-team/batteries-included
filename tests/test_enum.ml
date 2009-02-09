let array     = [|'1';'2';'3';'4';'5'|]
let array2    = [|[|'1';'2';'3';'4';'5'|];
                  [|'6';'7';'8';'9';'A'|];
                  [|'B';'C';'D';'E';'F'|]|]
let array3    = [|[|[|'1';'2';'3';'4';'5'|];
                    [|'6';'7';'8';'9';'A'|];
                    [|'B';'C';'D';'E';'F'|]|];
                  [|[|'G';'H';'I';'J';'K'|];
                    [|'L';'M';'N';'O';'P'|];
                    [|'Q';'R';'S';'T';'U'|]|]|]
let list      = ['1';'2';'3';'4';'5']
let string    = "12345"
let bigarray1 = Bigarray.Array1.of_array Bigarray.char Bigarray.c_layout array
let bigarray2 = Bigarray.Array2.of_array Bigarray.char Bigarray.c_layout array2
let bigarray3 = Bigarray.Array3.of_array Bigarray.char Bigarray.c_layout array3

let utf8   = UTF8.of_string string
let rope   = Rope.of_ustring utf8

let nl      = print_newline
let out  s  = Enum.print Char.print  stdout s; nl ()
let uout s  = Enum.print UChar.print stdout s; nl ()

open Testing

let test_array = ("Enumerations on arrays 1", fun () -> 
open Array in
begin
  let source = array in
  let v1 = of_backwards (enum source)
  and v2 = of_enum (backwards source)
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" (print Char.print) v1 (print Char.print) v2)
  end
)
let test_array2 = ("Enumerations on arrays 2", fun () -> 
open Array in
begin
  let source = array in
  let v1 = of_backwards (backwards source)
  and v2 = source
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" (print Char.print) v1 (print Char.print) v2)
  end
)

let test_list = ("Enumerations on lists 1", fun () -> 
open List in
begin
  let source = list in
  let v1 = of_backwards (enum source)
  and v2 = of_enum (backwards source)
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" (print Char.print) v1 (print Char.print) v2)
  end
)
let test_list2 = ("Enumerations on lists 2", fun () -> 
open List in
begin
  let source = list in
  let v1 = of_backwards (backwards source)
  and v2 = source
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" (print Char.print) v1 (print Char.print) v2)
  end
)

let test_string = ("Enumerations on strings 1", fun () -> 
open String in
begin
  let source = string in
  let v1 = of_backwards (enum source)
  and v2 = of_enum (backwards source)
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%S <> %S" v1 v2)
end
)
let test_string2 = ("Enumerations on strings 2", fun () -> 
open String in
begin
  let source = string in
  let v1 = of_backwards (backwards source)
  and v2 = source
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%S <> %S" v1 v2)
end
)

let test_bigarray = ("Enumerations on big arrays 1", fun () -> 
open Array in
begin
  let v1 = of_enum (enum array)
  and v2 = of_enum (Bigarray.Array1.enum bigarray1)
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" (print Char.print) v1 (print Char.print) v2)
  end
)
let test_bigarray2 = ("Enumerations on big arrays 2", fun () -> 
open Array in
begin
  let v1 = of_enum (Enum.flatten (Enum.map enum (enum array2)))
  and v2 = of_enum (Bigarray.Array2.enum bigarray2)
  in if v1 = v2 then Pass
    else 
      let print_array = print Char.print  in
	Fail (Printf.sprintf2 "%a <> %a" print_array v1 print_array v2)
  end
)
let test_bigarray3 = ("Enumerations on big arrays 3", fun () -> 
open Array in
begin
  let v1 = of_enum (Enum.flatten (Enum.map enum (Enum.flatten (Enum.map enum (enum array3)))))
  and v2 = of_enum (Bigarray.Array3.enum bigarray3)
  in if v1 = v2 then Pass
    else 
      let print_array = print Char.print  in
	Fail (Printf.sprintf2 "%a <> %a" print_array v1 print_array v2)
  end
)


(*let test_list  = ("Enumerations on lists",  fun() ->
   open List in
    if of_backwards (enum list) = of_enum (backwards list) then Pass
    else Fail ""   
)
let test_string  = ("Enumerations on strings",  fun() ->
   open String in
    if of_backwards (enum string) = of_enum (backwards string) then Pass
    else Fail ""   
)

let _ = 
  end;
  open List in
  begin
    String.print stdout "List\n"; 
    print Char.print  stdout list; nl();
    out (enum list); 
    out (backwards list); 
    print Char.print  stdout (of_backwards (backwards list)); nl ()
  end;
  open String in
  begin
    String.print stdout "String\n"; 
    println stdout string; 
    out (enum string); 
    out (backwards string); 
    println stdout (of_backwards (backwards string));
  end;
  open Rope in
  begin
    String.print stdout "Rope\n"; 
    print stdout rope; nl();
    uout (enum rope); 
    uout (backwards rope); 
    print stdout (of_backwards (backwards rope)); nl ()
  end;
(*These functions have been removed from the library  
  open Rope in
  begin
    String.print stdout "Rope (bulk)\n"; 
    print stdout rope; nl();
    Enum.print UTF8.print stdout (bulk_enum rope); nl ();
    Enum.print UTF8.print stdout (bulk_backwards rope); nl();
    print stdout (of_bulk_backwards (bulk_backwards rope)); nl ()
  end;*)
  open UTF8 in
  begin
    String.print stdout "UTF8\n"; 
    print stdout utf8; nl();
    uout (enum utf8); 
    uout (backwards utf8); 
    print stdout (of_backwards (backwards utf8)); nl ()
  end
*)
