let buffer = String.create 4096

let _ =
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "-d" then begin
    (* decompress *)
    let ic = Bzip2.open_in_chan stdin in
    let rec decompress () =
      let n = Bzip2.input ic buffer 0 (String.length buffer) in
      if n = 0 then () else begin output stdout buffer 0 n; decompress() end
    in decompress(); Bzip2.dispose ic
  end else begin
    (* compress *)
    let oc = Bzip2.open_out_chan stdout in
    let rec compress () =
      let n = input stdin buffer 0 (String.length buffer) in
      if n = 0 then () else begin Bzip2.output oc buffer 0 n; compress() end
    in compress(); Bzip2.flush oc
  end
