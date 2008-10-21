let string = "qwertyuiop\nasdfghjkl;\nzxcvbnm"
let array  = [|1;2;3;4;5;6;7;8;9|]

let (i,o) = System.IO.pipe ();;

Meta.Marshal.output o string;;
Printf.printf "String test:%S\n%S\n" string (Meta.Marshal.input i);;
Meta.Marshal.output o array;;
Printf.printf "Array test:%a\n%a\n" (Array.print Int.print) array (Array.print Int.print) (Meta.Marshal.input i);;
