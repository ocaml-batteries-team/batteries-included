open BatPervasives
open BatSet

let of_list l = List.fold_left (flip add) empty l
let of_list2 l = List.fold_left (fun x y -> add y x) empty l
let of_list3 l = BatList.enum l |> BatSet.of_enum

let wrap f () = f [1;3;5;7;9;2;4;6;8;10; 2; 5; 8; 3; 1; 9; 6]

let () = Bench.bench ["flip", wrap of_list;
                      "fun", wrap of_list2;
                      "enum", wrap of_list3]
