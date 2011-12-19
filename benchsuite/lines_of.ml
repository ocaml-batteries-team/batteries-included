let readfile fn =
  let ic = open_in fn in
  let r = ref [] in
  (try while true do
     let l = input_line ic in
     r := l :: !r
   done with End_of_file -> ()) ;
  close_in ic ;
  List.rev !r

let readfile_batteries fn =
  let open Batteries in
  File.lines_of fn |> List.of_enum

let file_lines_of fn =
  let ic = open_in fn in
  BatEnum.suffix_action
    (fun () -> close_in ic)
    (BatEnum.from (fun () -> try input_line ic with End_of_file -> raise BatEnum.No_more_elements))

let rfb2 fn = 
   BatList.of_enum (file_lines_of fn)

let rfb3 fn = BatList.of_enum (BatIO.lines_of2 (BatFile.open_in fn))

let wrap f () = f "setup.ml"

let () = 
  Bench.config.Bench.samples <- 300;
  Bench.bench ["readfile", wrap readfile;
               "readfile_batteries", wrap readfile_batteries;
               "file_lines_of", wrap rfb2;
               "lines_of2", wrap rfb3;
              ]
