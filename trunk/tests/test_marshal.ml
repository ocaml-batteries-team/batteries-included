open Data.Logical

let string = "qwertyuiop\nasdfghjkl;\nzxcvbnm"
let array  = [|1;2;3;4;5;6;7;8;9|]
let bitset = BitSet.create 8;;

BitSet.set bitset 0;;
BitSet.set bitset 2;;
BitSet.set bitset 4;;
BitSet.set bitset 6;;

let (i,o) = System.IO.pipe ();;

Meta.Marshal.output o string;;
Meta.Marshal.output o array;;
Meta.Marshal.output o bitset;;
Printf.printf "String test:\n%S\n%S\n" string (Meta.Marshal.input i);;
Printf.printf "Array test:\n%a\n%a\n" (Array.print Int.print) array (Array.print Int.print) (Meta.Marshal.input i);;
Printf.printf "BitSet test:\n%a\n%a\n" BitSet.print bitset BitSet.print (Meta.Marshal.input i);;
