open System.IO
open Data.Mutable

let test s =
  create_out
    ~write:ignore
    ~output:(fun _ _ _ -> 0)
    ~flush:(fun () -> Printf.printf "Flushed %s\n%!" s)
    ~close:ignore


let test_1 = test "1";;
let test_2 = test "2";;
let test_3 = test "3";;

flush_all ();;
