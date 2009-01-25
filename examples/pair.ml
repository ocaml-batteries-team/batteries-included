Enum.iter2
  (fun x y -> Printf.printf "| %s -> %S\n" x y)
  (File.lines_of Sys.argv.(1))
  (File.lines_of Sys.argv.(2))
