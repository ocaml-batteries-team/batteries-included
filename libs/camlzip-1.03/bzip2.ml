
(* Module [Bzip2]: reading and writing to/from [bzip2] compressed files *)

exception Error of string

let buffer_size = 1024

type in_channel =
  { in_chan: Pervasives.in_channel;
    in_buffer: string;
    mutable in_pos: int;
    mutable in_avail: int;
    mutable in_eof: bool;
    in_stream: Bzlib.stream;
    mutable in_size: int32; }

let open_in_chan ic =
  { in_chan = ic;
    in_buffer = String.create buffer_size;
    in_pos = 0;
    in_avail = 0;
    in_eof = false;
    in_stream = Bzlib.decompress_init 0 false;
    in_size = Int32.zero }

let open_in filename =
  open_in_chan (Pervasives.open_in_bin filename)

let read_byte iz =
  if iz.in_avail = 0 then begin
    let n = Pervasives.input iz.in_chan iz.in_buffer 0
                             (String.length iz.in_buffer) in
    if n = 0 then raise End_of_file;
    iz.in_pos <- 0;
    iz.in_avail <- n
  end;
  let c = iz.in_buffer.[iz.in_pos] in
  iz.in_pos <- iz.in_pos + 1;
  iz.in_avail <- iz.in_avail - 1;
  Char.code c

let read_int32 iz =
  let b1 = read_byte iz in
  let b2 = read_byte iz in
  let b3 = read_byte iz in
  let b4 = read_byte iz in
  Int32.logor (Int32.of_int b1)
    (Int32.logor (Int32.shift_left (Int32.of_int b2) 8)
      (Int32.logor (Int32.shift_left (Int32.of_int b3) 16)
                   (Int32.shift_left (Int32.of_int b4) 24)))

let rec input iz buf pos len =
  if pos < 0 || len < 0 || pos + len > String.length buf then
    invalid_arg "Bzip2.input";
  if iz.in_eof then 0 else begin
    if iz.in_avail = 0 then begin
      let n = Pervasives.input iz.in_chan iz.in_buffer 0
                               (String.length iz.in_buffer) in
      if n = 0 then raise(Error("truncated file"));
      iz.in_pos <- 0;
      iz.in_avail <- n
    end;
    let (finished, used_in, used_out) =
      try
        Bzlib.decompress iz.in_stream iz.in_buffer iz.in_pos iz.in_avail
                                   buf pos len
      with Bzlib.Error(_, e) ->
        raise(Error(Bzlib.string_of_error e)) in
    iz.in_pos <- iz.in_pos + used_in;
    iz.in_avail <- iz.in_avail - used_in;
    iz.in_size <- Int32.add iz.in_size (Int32.of_int used_out);
    if finished then begin
      iz.in_eof <- true;
      used_out
    end else if used_out = 0 then
      input iz buf pos len
    else
      used_out
  end

let rec really_input iz buf pos len =
  if len <= 0 then () else begin
    let n = input iz buf pos len in
    if n = 0 then raise End_of_file;
    really_input iz buf (pos + n) (len - n)
  end

let char_buffer = String.create 1

let input_char iz =
  if input iz char_buffer 0 1 = 0 then raise End_of_file else char_buffer.[0]

let input_byte iz =
  Char.code (input_char iz)

let dispose iz =
  iz.in_eof <- true;
  Bzlib.decompress_end iz.in_stream

let close_in iz =
  dispose iz;
  Pervasives.close_in iz.in_chan

type out_channel =
  { out_chan: Pervasives.out_channel;
    out_buffer: string;
    mutable out_pos: int;
    mutable out_avail: int;
    out_stream: Bzlib.stream;
    mutable out_size: int32; }

let open_out_chan ?(level = 6) oc =
  if level < 1 || level > 9 then invalid_arg "Bzip2.open_out: bad level";
  { out_chan = oc;
    out_buffer = String.create buffer_size;
    out_pos = 0;
    out_avail = buffer_size;
    out_stream = Bzlib.compress_init level 0 0;
    out_size = Int32.zero }

let open_out ?(level = 6) filename =
  open_out_chan ~level (Pervasives.open_out_bin filename)

let rec output oz buf pos len =
  if pos < 0 || len < 0 || pos + len > String.length buf then
    invalid_arg "Bzlib2.output";
  (* If output buffer is full, flush it *)
  if oz.out_avail = 0 then begin
    Printf.printf "Flushing out_avail\n";
    Pervasives.output oz.out_chan oz.out_buffer 0 oz.out_pos;
    oz.out_pos <- 0;
    oz.out_avail <- String.length oz.out_buffer
  end;
  let (_, used_in, used_out) =
    try
      Bzlib.compress oz.out_stream buf pos len
                                 oz.out_buffer oz.out_pos oz.out_avail
                                 Bzlib.BZ_RUN
    with Bzlib.Error(f, e) ->
      raise (Error(Bzlib.string_of_error e)) in
  oz.out_pos <- oz.out_pos + used_out;
  oz.out_avail <- oz.out_avail - used_out;
  oz.out_size <- Int32.add oz.out_size (Int32.of_int used_in);
  if used_in < len then output oz buf (pos + used_in) (len - used_in)

let output_char oz c =
  char_buffer.[0] <- c;
  output oz char_buffer 0 1

let output_byte oz b =
  output_char oz (Char.unsafe_chr b)

let flush oz =
  let rec do_flush () =
    (* If output buffer is full, flush it *)
    if oz.out_avail = 0 then begin
      Pervasives.output oz.out_chan oz.out_buffer 0 oz.out_pos;
      oz.out_pos <- 0;
      oz.out_avail <- String.length oz.out_buffer
    end;
    let (finished, _, used_out) =
      Bzlib.compress oz.out_stream oz.out_buffer 0 0
                                 oz.out_buffer oz.out_pos oz.out_avail
                                 Bzlib.BZ_FINISH in
    oz.out_pos <- oz.out_pos + used_out;
    oz.out_avail <- oz.out_avail - used_out;
    if not finished then do_flush() in
  do_flush();
  (* Final data flush *)
  if oz.out_pos > 0 then
    Pervasives.output oz.out_chan oz.out_buffer 0 oz.out_pos;
  Bzlib.compress_end oz.out_stream

let close_out oz =
  flush oz;
  Pervasives.close_out oz.out_chan

