(* Batteries Included - (De)Compression modules
 * 
 * Copyright (C) 2001 Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *           (C) 2008 Stefano Zacchiroli <zack@upsilon.cc>
 * 
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of the
 * License, or (at your option) any later version, with the special
 * exception on linking described in file LICENSE.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA *)

open Common
open Extlib

(* The API of Camlzip does not allow to create (or otherwise work on)
   compressed chanells out of something else than legacy channels or
   file names. It is not easily generalizable either, because several
   channel functions are used, and also specific exceptions (e.g.,
   End_of_file) catched.

   Hence, this module mimic (read: it's shamelessly inspired from)
   Camlzip's [Gzip] module, but using Batteries' channels, functions,
   and exceptions.

   Future changes to Camlzip's [Gzip] might be needed to be ported
   here.
*)

open Compress

let buffer_size = 1024
  (** Internal I/O buffer. Specify how many bytes are read
      (resp. write) at a time from (to) the underlying channel and
      buffered before reading (writing) again. *)

type in_channel =
    { in_chan: IO.input;
      in_buffer: string;
      mutable in_pos: int;
      mutable in_avail: int;
      mutable in_eof: bool;
      in_stream: Zlib.stream;
      mutable in_size: int32;
      mutable in_crc: int32 }

let open_input ic =
  (* Superficial parsing of header *)
  begin try
    let id1 = IO.read_byte ic in
    let id2 = IO.read_byte ic in
    if id1 <> 0x1F || id2 <> 0x8B then 
      raise(Compression_error("bad magic number, not a gzip file", None));
    let cm = IO.read_byte ic in
    if cm <> 8 then
      raise(Compression_error("unknown compression method", None));
    let flags = IO.read_byte ic in
    if flags land 0xE0 <> 0 then
      raise(Compression_error("bad flags, not a gzip file", None));
    for i = 1 to 6 do ignore(IO.read_byte ic) done;
    if flags land 0x04 <> 0 then begin
      (* Skip extra data *)
      let len1 = IO.read_byte ic in
      let len2 = IO.read_byte ic in
      for i = 1 to len1 + len2 lsl 8 do ignore(IO.read_byte ic) done
    end;
    if flags land 0x08 <> 0 then begin
      (* Skip original file name *)
      while IO.read_byte ic <> 0 do () done
    end;
    if flags land 0x10 <> 0 then begin
      (* Skip comment *)
      while IO.read_byte ic <> 0 do () done
    end;
    if flags land 0x02 <> 0 then begin
      (* Skip header CRC *)
      ignore(IO.read_byte ic); ignore(IO.read_byte ic)
    end
  with IO.No_more_input | End_of_file ->
    raise(Compression_error("premature end of file, not a gzip file", None))
  end;
  { in_chan = ic;
    in_buffer = String.create buffer_size;
    in_pos = 0;
    in_avail = 0;
    in_eof = false;
    in_stream = Zlib.inflate_init false;
    in_size = Int32.zero;
    in_crc = Int32.zero }

let read_byte iz =
  if iz.in_avail = 0 then begin
    let n = IO.input iz.in_chan iz.in_buffer 0
                             (String.length iz.in_buffer) in
    if n = 0 then raise IO.No_more_input;
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
    invalid_arg "Gzip.input";
  if iz.in_eof then 0 else begin
    if iz.in_avail = 0 then begin
      let n = IO.input iz.in_chan iz.in_buffer 0
                               (String.length iz.in_buffer) in
      if n = 0 then raise(Compression_error("truncated file", None));
      iz.in_pos <- 0;
      iz.in_avail <- n
    end;
    let (finished, used_in, used_out) =
      try
        Zlib.inflate iz.in_stream iz.in_buffer iz.in_pos iz.in_avail
                                   buf pos len Zlib.Z_SYNC_FLUSH
      with Zlib.Error(_, _) as exn ->
        raise(Compression_error("error during decompression", Some exn)) in
    iz.in_pos <- iz.in_pos + used_in;
    iz.in_avail <- iz.in_avail - used_in;
    iz.in_crc <- Zlib.update_crc iz.in_crc buf pos used_out;
    iz.in_size <- Int32.add iz.in_size (Int32.of_int used_out);
    if finished then begin
      try
        let crc = read_int32 iz in
        let size = read_int32 iz in
        if iz.in_crc <> crc then 
          raise(Compression_error("CRC mismatch, data corrupted", None));
        if iz.in_size <> size then
          raise(Compression_error("size mismatch, data corrupted", None));
        iz.in_eof <- true;
        used_out
      with IO.No_more_input | End_of_file ->
        raise(Compression_error("truncated file", None))
    end
    else if used_out = 0 then
      input iz buf pos len
    else
      used_out
  end

let rec really_input iz buf pos len =
  if len <= 0 then () else begin
    let n = input iz buf pos len in
    if n = 0 then raise IO.No_more_input;
    really_input iz buf (pos + n) (len - n)
  end

let char_buffer = String.create 1

let input_char iz =
  if input iz char_buffer 0 1 = 0
  then raise IO.No_more_input
  else char_buffer.[0]

let input_byte iz =
  Char.code (input_char iz)

let dispose iz =
  iz.in_eof <- true;
  Zlib.inflate_end iz.in_stream

let close_in iz =
  dispose iz;
  IO.close_in iz.in_chan

type 'a out_channel =
  { out_chan: 'a IO.output;
    out_buffer: string;
    mutable out_pos: int;
    mutable out_avail: int;
    out_stream: Zlib.stream;
    mutable out_size: int32;
    mutable out_crc: int32 }

let open_output ?(level = 6) oc =
  if level < 1 || level > 9 then invalid_arg "Gzip.open_out: bad level";
  (* Write minimal header *)
  IO.write_byte oc 0x1F;                  (* ID1 *)
  IO.write_byte oc 0x8B;                  (* ID2 *)
  IO.write_byte oc 8;                     (* compression method *)
  IO.write_byte oc 0;                     (* flags *)
  for i = 1 to 4 do IO.write_byte oc 0 done; (* mtime *)
  IO.write_byte oc 0;                     (* xflags *)
  IO.write_byte oc 0xFF;                  (* OS (unknown) *)
  { out_chan = oc;
    out_buffer = String.create buffer_size;
    out_pos = 0;
    out_avail = buffer_size;
    out_stream = Zlib.deflate_init level false;
    out_size = Int32.zero;
    out_crc = Int32.zero }

let rec output oz buf pos len =
  if pos < 0 || len < 0 || pos + len > String.length buf then
    invalid_arg "Gzip.output";
  (* If output buffer is full, flush it *)
  if oz.out_avail = 0 then begin
    ignore(IO.really_output oz.out_chan oz.out_buffer 0 oz.out_pos);
    oz.out_pos <- 0;
    oz.out_avail <- String.length oz.out_buffer
  end;
  let (_, used_in, used_out) =
    try
      Zlib.deflate oz.out_stream buf pos len
                                 oz.out_buffer oz.out_pos oz.out_avail
                                 Zlib.Z_NO_FLUSH
    with Zlib.Error(_, _) as exn ->
      raise (Compression_error("error during compression", Some exn)) in
  oz.out_pos <- oz.out_pos + used_out;
  oz.out_avail <- oz.out_avail - used_out;
  oz.out_size <- Int32.add oz.out_size (Int32.of_int used_in);
  oz.out_crc <- Zlib.update_crc oz.out_crc buf pos used_in;
  if used_in < len then output oz buf (pos + used_in) (len - used_in)

let output oz buf pos len =
  output oz buf pos len;
  len

let output_char oz c =
  char_buffer.[0] <- c;
  ignore(output oz char_buffer 0 1)

let output_byte oz b =
  output_char oz (Char.unsafe_chr b)

let write_int32 oc n =
  let r = ref n in
  for i = 1 to 4 do
    IO.write_byte oc (Int32.to_int !r);
    r := Int32.shift_right_logical !r 8
  done

let flush oz =
  let rec do_flush () =
    (* If output buffer is full, flush it *)
    if oz.out_avail = 0 then begin
      ignore(IO.really_output oz.out_chan oz.out_buffer 0 oz.out_pos);
      oz.out_pos <- 0;
      oz.out_avail <- String.length oz.out_buffer
    end;
    let (finished, _, used_out) =
      Zlib.deflate oz.out_stream oz.out_buffer 0 0
                                 oz.out_buffer oz.out_pos oz.out_avail
                                 Zlib.Z_FINISH in
    oz.out_pos <- oz.out_pos + used_out;
    oz.out_avail <- oz.out_avail - used_out;
    if not finished then do_flush() in
  do_flush();
  (* Final data flush *)
  if oz.out_pos > 0 then
    ignore(IO.really_output oz.out_chan oz.out_buffer 0 oz.out_pos);
  (* Write CRC and size *)
  write_int32 oz.out_chan oz.out_crc;
  write_int32 oz.out_chan oz.out_size;
  (* Dispose of stream *)
  Zlib.deflate_end oz.out_stream

let close_out oz =
  flush oz;
  IO.close_out oz.out_chan
