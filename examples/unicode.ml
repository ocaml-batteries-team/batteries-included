open Batteries
open Data.Text


let say x = System.IO.nwrite System.IO.stdout x; System.IO.write System.IO.stdout '\n'
let usay = System.IO.write_uline System.IO.stdout

let s1 = "Simple ASCII string"
and s2 = "Complex: á é í ó ú"

let u1 = UTF8.of_string s1 

let rope1 = Rope.of_ustring s1
and rope2 = Rope.of_latin1 s2

let rec exp_dup n r = if n <= 0 then r else exp_dup (n-1) (Rope.concat r r)

let r16 = exp_dup 4 rope2

let () = usay rope1; usay rope2
let () = usay r16 

let r3 = Rope.sub 15 36 r16

let () = say "Characters 15 to 41 of r16: "; usay r3

let c11 = Rope.get 11 rope2 
let () = 
  say "Character 11: "; 
  System.IO.write_uchar System.IO.stdout c11; say "\n"

let bad_rope = 
  try 
    usay (Rope.of_ustring (UTF8.of_string s2)) 
  with UTF8.Malformed_code -> say "Non-utf8 input -- caught Malformed_code\n (don't worry, that's part of the example)\n"

