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

let test_rope = ("Enumerations on ropes 1", fun () -> 
open Rope in
begin
  let source = rope in
  let v1 = of_backwards (enum source)
  and v2 = of_enum (backwards source)
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" Rope.print v1 Rope.print v2)
end
)
let test_rope2 = ("Enumerations on ropes 2", fun () -> 
open Rope in
begin
  let source = rope in
  let v1 = of_backwards (backwards source)
  and v2 = source
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" Rope.print v1 Rope.print v2)
end
)

let test_UTF8 = ("Enumerations on UTF8 1", fun () -> 
open UTF8 in
begin
  let source = utf8 in
  let v1 = of_backwards (enum source)
  and v2 = of_enum (backwards source)
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" UTF8.print v1 UTF8.print v2)
end
)
let test_UTF82 = ("Enumerations on UTF8 2", fun () -> 
open UTF8 in
begin
  let source = utf8 in
  let v1 = of_backwards (backwards source)
  and v2 = source
  in if v1 = v2 then Pass
    else Fail (Printf.sprintf2 "%a <> %a" UTF8.print v1 UTF8.print v2)
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


