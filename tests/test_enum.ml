open Data.Mutable
open Data.Text
open Data.Numeric

let array  = [|'1';'2';'3';'4';'5'|]
let list   = ['1';'2';'3';'4';'5']
let string = "12345"
let utf8   = UTF8.of_string string
let rope   = Rope.of_ustring utf8

let nl      = print_newline
let out  s  = Enum.print Char.print  stdout s; nl ()
let uout s  = Enum.print UChar.print stdout s; nl ()


let _ = 
  open Array in 
  begin
    String.print stdout "Array\n";
    print Char.print  stdout array; nl();
    out (enum array); 
    out (backwards array); 
    print Char.print  stdout (of_backwards (backwards array)); nl ()
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
