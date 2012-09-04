
let say x = IO.nwrite IO.stdout x; IO.write IO.stdout '\n'
let usay = IO.write_uline IO.stdout

let s1 = "Simple ASCII string"
and s2 = "Complex: á é í ó ú"

let u1 = UTF8.of_string s1

let rope1 = Rope.of_ustring u1
and rope2 = Rope.of_latin1 s2

let rec exp_dup n r = if n <= 0 then r else exp_dup (n-1) (Rope.append r r)

let r16 = exp_dup 4 rope2

let () = usay rope1; usay rope2
let () = usay r16

let r3 = Rope.sub r16 15 36

let () = say "Characters 15 to 41 of r16: "; usay r3

let c11 = Rope.get rope2 11
let () =
  say "Character 11: ";
  IO.write_uchar IO.stdout c11; say "\n"

let bad_rope =
  try
    usay (Rope.of_ustring (UTF8.of_string s2))
  with UTF8.Malformed_code -> say "Non-utf8 input -- caught Malformed_code\n (don't worry, that's part of the example)\n"

