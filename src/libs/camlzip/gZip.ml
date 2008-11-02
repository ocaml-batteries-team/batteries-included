
open Extlib

let uncompress input =
  let camlzip_in = ExtGzip.open_input input in
    IO.create_in
      ~read:(fun () -> ExtGzip.input_char camlzip_in)
      ~input:(ExtGzip.input camlzip_in)
      ~close:(fun () -> ExtGzip.close_in camlzip_in)

let compress output =
  let camlzip_out = ExtGzip.open_output output in
    IO.create_out
      ~write:(ExtGzip.output_char camlzip_out)
      ~output:(ExtGzip.output camlzip_out)
      ~flush:(fun () -> ExtGzip.flush camlzip_out)
      ~close:(fun () -> ExtGzip.close_out camlzip_out)

let open_in ?mode ?perm fname = uncompress (File.open_in ?mode ?perm fname)
let open_out ?mode ?perm fname = compress (File.open_out ?mode ?perm fname)

let gzip_compress ?(level = 6) =
  failwith "Not implemented: gzip_compress"
