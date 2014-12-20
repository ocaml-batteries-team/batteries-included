(*
 * BatIO - Abstract input/output
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (contributor)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

include BatInnerIO

let uid () = UID.uid ()

(** {6 Primitive operations}*)

external noop        : unit      -> unit        = "%ignore"
external cast_output : 'a output -> unit output = "%identity"
let lock = ref BatConcurrent.nolock


let outputs = OutputWeakSet.create 32
let outputs_add out =
  BatConcurrent.sync !lock (OutputWeakSet.add outputs) out

let outputs_remove out =
  BatConcurrent.sync !lock (OutputWeakSet.remove outputs) out


exception No_more_input
exception Input_closed
exception Output_closed




let post_incr r =
  let result = !r in
  incr r;
  result
let post r op =
  let result = !r in
  r := op !r;
  result


let on_close_out out f =
  BatConcurrent.sync !lock (fun () ->
    let do_close = out.out_close in
    out.out_close <- (fun () -> f out; do_close ())) ()

let on_close_in inp f =
  BatConcurrent.sync !lock (fun () ->
    let do_close = inp.in_close in
    inp.in_close <- (fun () -> f inp; do_close ())) ()

let close_in i =
  let f _ = raise Input_closed in
  i.in_close();
  i.in_read <- f;
  i.in_input <- f;
  i.in_close <- noop (*Double closing is not a problem*)


let wrap_in ~read ~input ~close ~underlying =
  let result =
    {
      in_read     = read;
      in_input    = input;
      in_close    = close;
      in_id       = uid ();
      in_upstream = InputWeakSet.create 2
    }
  in
  let add_upstream x = InputWeakSet.add x.in_upstream result in
  BatConcurrent.sync !lock (List.iter add_upstream) underlying;
  Gc.finalise close_in result;
  result

let inherit_in ?read ?input ?close inp =
  let read  = match read  with None -> inp.in_read | Some f -> f
  and input = match input with None -> inp.in_input| Some f -> f
  and close = match close with None -> ignore      | Some f -> f
  in  wrap_in ~read ~input ~close ~underlying:[inp]


let create_in ~read ~input ~close =
  wrap_in ~read ~input ~close ~underlying:[]

(*For recursively closing outputs, we need either polymorphic
  recursion or a hack. Well, a hack it is.*)

(*Close a [unit output] -- note that this works for any kind of output,
  thanks to [cast_output], but this can't return a proper result.*)
let rec close_unit (o:unit output) : unit =
  let forbidden _ = raise Output_closed in
  o.out_flush ();
  OutputWeakSet.iter close_unit o.out_upstream;
  let r = o.out_close() in
  o.out_write  <- forbidden;
  o.out_output <- forbidden;
  o.out_close  <- (fun _ -> r) (*Closing again is not a problem*);
  o.out_flush  <- noop         (*Flushing again is not a problem*);
  ()

(*Close a ['a output] -- first close it as a [unit output] then
  recover the result.*)
let close_out o =
  (*  Printf.eprintf "close_out\n%!";*)
  close_unit (cast_output o);
  o.out_close ()


let ignore_close_out out = ignore (close_out out)

let wrap_out ~write ~output ~flush ~close ~underlying  =
  let rec out =
    {
      out_write  = write;
      out_output = output;
      out_close  = (fun () ->
        outputs_remove (cast_output out);
        close ());
      out_flush  = flush;
      out_id     = uid ();
      out_upstream = OutputWeakSet.create 2
    }
  in
  let o = cast_output out in
  let add_upstream x = OutputWeakSet.add x.out_upstream o in
  BatConcurrent.sync !lock (List.iter add_upstream) underlying;
  outputs_add (cast_output out);
  Gc.finalise ignore_close_out out;
  out

let inherit_out ?write ?output ?flush ?close out =
  let write = match write  with None -> out.out_write | Some f -> f
  and output= match output with None -> out.out_output| Some f -> f
  and flush = match flush  with None -> out.out_flush | Some f -> f
  and close = match close  with None -> ignore        | Some f -> f
  in wrap_out ~write ~output ~flush ~close ~underlying:[out]

let create_out ~write ~output ~flush ~close =
  wrap_out ~write ~output ~flush ~close ~underlying:[]

let read i = i.in_read()

let nread i n =
  if n < 0 then invalid_arg "BatIO.nread";
  if n = 0 then
    ""
  else
    let s = String.create n in
    let l = ref n in
    let p = ref 0 in
    try
      while !l > 0 do
        let r = i.in_input s !p !l in
        if r = 0 then raise No_more_input;
        p := !p + r;
        l := !l - r;
      done;
      s
    with
      No_more_input as e ->
      if !p = 0 then raise e;
      String.sub s 0 !p

let really_output o s p l' =
  let sl = String.length s in
  if p + l' > sl || p < 0 || l' < 0 then invalid_arg "BatIO.really_output";
  let l = ref l' in
  let p = ref p in
  while !l > 0 do
    let w = o.out_output s !p !l in
    if w = 0 then raise Sys_blocked_io;
    p := !p + w;
    l := !l - w;
  done;
  l'

let input i s p l =
  let sl = String.length s in
  if p + l > sl || p < 0 || l < 0 then invalid_arg "BatIO.input";
  if l = 0 then
    0
  else
    i.in_input s p l

let really_input i s p l' =
  let sl = String.length s in
  if p + l' > sl || p < 0 || l' < 0 then invalid_arg "BatIO.really_input";
  let l = ref l' in
  let p = ref p in
  while !l > 0 do
    let r = i.in_input s !p !l in
    if r = 0 then raise Sys_blocked_io;
    p := !p + r;
    l := !l - r;
  done;
  l'

let really_nread i n =
  if n < 0 then invalid_arg "BatIO.really_nread";
  if n = 0 then ""
  else
    let s = String.create n
    in
    ignore(really_input i s 0 n);
    s

let output o s p l =
  let sl = String.length s in
  if p + l > sl || p < 0 || l < 0 then invalid_arg "BatIO.output";
  o.out_output s p l

let flush o = o.out_flush()

let flush_all () =
  BatConcurrent.sync !lock ( OutputWeakSet.iter (fun o -> try flush o with _ -> ())) outputs

let close_all () =
  let outs =
    BatConcurrent.sync !lock (OutputWeakSet.fold (fun o os -> o :: os) outputs) []
  in
  List.iter (fun o -> try close_out o with _ -> ()) outs

let read_all i =
  let maxlen = 1024 in
  let str    = ref [] in
  let pos    = ref 0 in
  let rec loop() =
    let s = nread i maxlen in
    str := (s,!pos) :: !str;
    pos := !pos + String.length s;
    loop()
  in
  try
    loop()
  with
    No_more_input
  | Input_closed ->
    let buf = String.create !pos in
    List.iter (fun (s,p) ->
      String.unsafe_blit s 0 buf p (String.length s)
    ) !str;
    buf

let input_string s =
  let pos = ref 0 in
  let len = String.length s in
  create_in
    ~read:(fun () ->
      if !pos >= len then raise No_more_input
      else String.unsafe_get s (post_incr pos))
    ~input:(fun sout p l ->
      if !pos >= len then raise No_more_input;
      let n = (if !pos + l > len then len - !pos else l) in
      String.unsafe_blit s (post pos ( (+) n ) ) sout p n;
      n
    )
    ~close:noop



(**
   {6 Standard BatIO}
*)




let default_buffer_size = 16 (*Arbitrary number. If you replace it, just
                               don't put something too small, i.e. anything
                               smaller than 10 is probably a bad idea.*)

let output_string() =
  let b = Buffer.create default_buffer_size in
  create_out
    ~write:  (fun c -> Buffer.add_char b c )
    ~output: (fun s p l -> Buffer.add_substring b s p l;  l  )
    ~close:  (fun () -> Buffer.contents b)
    ~flush:  noop


(** A placeholder used to allow recursive use of [self]
    in an [input_channel]*)
let placeholder_in =
  { in_read  = (fun () -> ' ');
    in_input = (fun _ _ _ -> 0);
    in_close = noop;
    in_id    = UID.placeholder;
    in_upstream= InputWeakSet.create 0 }
let input_channel ?(autoclose=true) ?(cleanup=false) ch =
  let me = ref placeholder_in (*placeholder*)
  in let result =
    create_in
      ~read:(fun () -> try input_char ch
      with End_of_file ->
        if autoclose then close_in !me;
        raise No_more_input)
      ~input:(fun s p l ->
        let n = Pervasives.input ch s p l in
        if n = 0 then
          begin
            if autoclose then close_in !me else ();
            raise No_more_input
          end
        else n)
      ~close:(if cleanup then fun () -> Pervasives.close_in ch else ignore)
  in
  me := result;
  result

let output_channel ?(cleanup=false) ch =
  create_out
    ~write: (fun c     -> output_char ch c)
    ~output:(fun s p l -> Pervasives.output ch s p l; l)
    ~close: (if cleanup then fun () ->
        begin
          (*           Printf.eprintf "Cleaning up\n%!";*)
          Pervasives.close_out ch
        end
      else fun () ->
        begin
          (*           Printf.eprintf "Not cleaning up\n%!";*)
          Pervasives.flush ch
        end)
    ~flush: (fun ()    -> Pervasives.flush ch)


let pipe() =
  let input = ref "" in
  let inpos = ref 0 in
  let output = Buffer.create default_buffer_size in
  let flush() =
    input := Buffer.contents output;
    inpos := 0;
    Buffer.reset output;
    if String.length !input = 0 then raise No_more_input
  in
  let read() =
    if !inpos = String.length !input then flush();
    String.unsafe_get !input (post_incr inpos)
  in
  let input s p l =
    if !inpos = String.length !input then flush();
    let r = (if !inpos + l > String.length !input then String.length !input - !inpos else l) in
    String.unsafe_blit !input !inpos s p r;
    inpos := !inpos + r;
    r
  in
  let write c =
    Buffer.add_char output c
  in
  let output s p l =
    Buffer.add_substring output s p l;
    l
  in
  let input  = create_in ~read ~input  ~close:noop
  and output = create_out ~write ~output ~close:noop ~flush:noop
  in
  input , output



(*let to_input_channel inp =
  let (fin, fout) = Unix.pipe () in
    let outp = out_channel fout  in
    (*connect [inp] to [outp]*)
    in_channel_of_descr fin*)
(**
   {6 Binary APIs}
*)


exception Overflow of string

let read_byte i = int_of_char (i.in_read())

let read_signed_byte i =
  let c = int_of_char (i.in_read()) in
  if c land 128 <> 0 then
    c - 256
  else
    c

let read_string i =
  let b = Buffer.create 8 in
  let rec loop() =
    let c = i.in_read() in
    if c <> '\000' then begin
      Buffer.add_char b c;
      loop();
    end;
  in
  loop();
  Buffer.contents b

let read_line i =
  let b = Buffer.create 80 in
  let cr = ref false in
  let rec loop() =
    match i.in_read() with
    | '\n' ->
      ()
    | '\r' when !cr ->
      Buffer.add_char b '\r';
      loop()
    | '\r' ->
      cr := true;
      loop()
    | c when !cr ->
      cr := false;
      Buffer.add_char b '\r';
      Buffer.add_char b c;
      loop();
    | c ->
      Buffer.add_char b c;
      loop()
  in
  try
    loop();
    Buffer.contents b
  with
    No_more_input ->
    if !cr then Buffer.add_char b '\r';
    if Buffer.length b > 0 then
      Buffer.contents b
    else
      raise No_more_input

(*$= read_line & ~cmp:BatString.equal ~printer:String.quote
  "abc" (read_line (BatIO.input_string "abc\ndef\n"))
  "abc" (read_line (BatIO.input_string "abc\r\ndef\n"))
  "abc\r" (read_line (BatIO.input_string "abc\r\r\ndef\n"))
  "abc" (read_line (BatIO.input_string "abc"))
  "abc\r" (read_line (BatIO.input_string "abc\r"))
  "kldsjf\r\r\rasdfa"  (read_line (BatIO.input_string "kldsjf\r\r\rasdfa\nsfdsagf\n"))
*)

let read_ui16 i =
  let ch1 = read_byte i in
  let ch2 = read_byte i in
  ch1 lor (ch2 lsl 8)

let read_i16 i =
  let ch1 = read_byte i in
  let ch2 = read_byte i in
  let n = ch1 lor (ch2 lsl 8) in
  if ch2 land 128 <> 0 then
    n - 65536
  else
    n

let fix = lnot 0x7FFFFFFF (* -:) *)

let read_i32 ch =
  let ch1 = read_byte ch in
  let ch2 = read_byte ch in
  let ch3 = read_byte ch in
  let ch4 = read_byte ch in
  if ch4 land 128 <> 0 then begin
    if ch4 land 64 = 0 then raise (Overflow "read_i32");
    (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor ((ch4 land 127) lsl 24)) lor fix (* FIX HERE *)
  end else begin
    if ch4 land 64 <> 0 then raise (Overflow "read_i32");
    ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)
  end

let read_real_i32 ch =
  let ch1 = read_byte ch in
  let ch2 = read_byte ch in
  let ch3 = read_byte ch in
  let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
  let big = Int32.shift_left (Int32.of_int (read_byte ch)) 24 in
  Int32.logor base big

let read_i64 ch =
  let ch1 = read_byte ch in
  let ch2 = read_byte ch in
  let ch3 = read_byte ch in
  let ch4 = read_byte ch in
  let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
  let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
  let big = Int64.of_int32 (read_real_i32 ch) in
  Int64.logor (Int64.shift_left big 32) small

let read_double ch =
  Int64.float_of_bits (read_i64 ch)

let read_float ch =
  Int32.float_of_bits (read_real_i32 ch)

let write_byte o n =
  (* doesn't test bounds of n in order to keep semantics of Pervasives.output_byte *)
  write o (Char.unsafe_chr (n land 0xFF))

let write_string o s =
  nwrite o s;
  write o '\000'

let write_line o s =
  nwrite o s;
  write o '\n'

let write_ui16 ch n =
  if n < 0 || n > 0xFFFF then raise (Overflow "write_ui16");
  write_byte ch n;
  write_byte ch (n lsr 8)

let write_i16 ch n =
  if n < -0x8000 || n > 0x7FFF then raise (Overflow "write_i16");
  if n < 0 then
    write_ui16 ch (65536 + n)
  else
    write_ui16 ch n

let write_i32 ch n =
  write_byte ch n;
  write_byte ch (n lsr 8);
  write_byte ch (n lsr 16);
  write_byte ch (n asr 24)

let write_real_i32 ch n =
  let base = Int32.to_int n in
  let big = Int32.to_int (Int32.shift_right_logical n 24) in
  write_byte ch base;
  write_byte ch (base lsr 8);
  write_byte ch (base lsr 16);
  write_byte ch big

let write_i64 ch n =
  write_real_i32 ch (Int64.to_int32 n);
  write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical n 32))

let write_double ch f =
  write_i64 ch (Int64.bits_of_float f)

let write_float ch f =
  write_real_i32 ch (Int32.bits_of_float f)

let stdin  = input_channel Pervasives.stdin
let stdout = output_channel Pervasives.stdout
let stderr = output_channel Pervasives.stderr
let stdnull= create_out
    ~write:ignore
    ~output:(fun _ _ l -> l)
    ~flush:ignore
    ~close:ignore

let get_output out = out.out_output
let get_flush  out = out.out_flush

let get_output_id out = (out.out_id :> int)
let get_input_id  inp = (inp.in_id :> int)

external noop :          unit -> unit = "%ignore"
external default_close : unit -> unit = "%ignore"

type ('a, 'b) printer = 'b output -> 'a -> unit

type 'a f_printer = Format.formatter -> 'a -> unit

let pos_in i =
  let p = ref 0 in
  (wrap_in
     ~read:(fun () ->
       let c = read i in
       incr p;
       c
     )
     ~input:(fun s sp l ->
       let n = input i s sp l in
       p := !p + n;
       n
     )
     ~close:noop
     ~underlying:[i]
  , (fun () -> !p))

let pos_out o =
  let p = ref 0 in
  (wrap_out
     ~write:(fun c ->
       write o c;
       incr p
     )
     ~output:(fun s sp l ->
       let n = output o s sp l in
       p := !p + n;
       n
     )
     ~close:noop
     ~flush:(fun () -> flush o)
     ~underlying:[o]
  , fun () -> !p)

let progress_in inp f =
  wrap_in ~read: (fun ()    -> let c = read inp in f(); c)
    ~input:(fun s i l -> let r = input inp s i l in f(); r)
    ~close:ignore
    ~underlying:[inp]

let progress_out out f =
  wrap_out ~write:(fun c -> write out c; f())
    ~output:(fun s i l -> let r = output out s i l in f(); r)
    ~close:ignore
    ~flush:(fun () -> flush out)
    ~underlying:[out]

(**
   {6 Support for enumerations}
*)

(*Function inlined here to avoid circular dependencies between [BatIO]
  and [ExtString].*)
let string_enum s =
  let l = String.length s in
  let rec make i =
    BatEnum.make
      ~next:(fun () ->
        if !i = l then
          raise BatEnum.No_more_elements
        else
          let p = !i in
          incr i;
          String.unsafe_get s p
      )
      ~count:(fun () -> l - !i)
      ~clone:(fun () -> make (ref !i))
  in
  make (ref 0)


let input_enum e =
  let pos = ref 0 in
  create_in
    ~read:(fun () ->
      match BatEnum.get e with
      | None -> raise No_more_input
      | Some c ->
        incr pos;
        c
    )
    ~input:(fun s p l ->
      let rec loop p l =
        if l = 0 then
          0
        else
          match BatEnum.get e with
          | None -> l
          | Some c ->
            String.unsafe_set s p c;
            loop (p + 1) (l - 1)
      in
      let k = loop p l in
      if k = l then raise No_more_input;
      l - k
    )
    ~close:default_close

let output_enum() =
  let b = Buffer.create default_buffer_size in
  create_out
    ~write:(fun x ->
      Buffer.add_char b x
    )
    ~output:(fun s p l ->
      Buffer.add_substring b s p l;
      l
    )
    ~close:(fun () ->
      let s = Buffer.contents b in
      string_enum s
    )
    ~flush:default_close


(** [apply_enum f x] applies [f] to [x] and converts exceptions
    [No_more_input] and [Input_closed] to [BatEnum.No_more_elements]*)
let apply_enum do_close f x =
  try f x
  with
  | No_more_input -> raise BatEnum.No_more_elements
  | Input_closed  -> do_close := false; raise BatEnum.No_more_elements

(** [close_at_end input e] returns an enumeration which behaves as [e]
    and has the secondary effect of closing [input] once everything has
    been read.*)
let close_at_end do_close (input:input) e =
  BatEnum.suffix_action (fun () -> if !do_close then close_in input) e

let make_enum f input =
  let do_close = ref true in
  close_at_end do_close input (BatEnum.from (fun () -> apply_enum do_close f input))


let combine (a,b) =
  wrap_out ~write:(fun c ->
    write a c;
    write b c)
    ~output:(fun s i j ->
      let _ = output a s i j in
      output b s i j)
    ~flush:(fun () ->
      flush a;
      flush b)
    ~close:(fun () ->
      (close_out a, close_out b))
    ~underlying:[cast_output a; cast_output b]


let write_enum f out enum =
  BatEnum.iter (f out) enum
(*;
  flush out*)

(**
   {6 Big Endians}
*)

module BigEndian = struct

  let read_ui16 i =
    let ch2 = read_byte i in
    let ch1 = read_byte i in
    ch1 lor (ch2 lsl 8)

  let read_i16 i =
    let ch2 = read_byte i in
    let ch1 = read_byte i in
    let n = ch1 lor (ch2 lsl 8) in
    if ch2 land 128 <> 0 then
      n - 65536
    else
      n

  let fix = lnot 0x7FFFFFFF (* -:) *)

  let read_i32 ch =
    let ch4 = read_byte ch in
    let ch3 = read_byte ch in
    let ch2 = read_byte ch in
    let ch1 = read_byte ch in
    if ch4 land 128 <> 0 then begin (* negative number *)
      if ch4 land 64 = 0 then raise (Overflow "read_i32");
      (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor ((ch4 land 127) lsl 24))
      lor fix (* FIX HERE *)
    end else begin (*positive number*)
      if ch4 land 64 <> 0 then raise (Overflow "read_i32");
      ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)
    end

  let read_real_i32 ch =
    let big = Int32.shift_left (Int32.of_int (read_byte ch)) 24 in
    let ch3 = read_byte ch in
    let ch2 = read_byte ch in
    let ch1 = read_byte ch in
    let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
    Int32.logor base big

  let read_i64 ch =
    let big = Int64.of_int32 (read_real_i32 ch) in
    let ch4 = read_byte ch in
    let ch3 = read_byte ch in
    let ch2 = read_byte ch in
    let ch1 = read_byte ch in
    let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
    let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
    Int64.logor (Int64.shift_left big 32) small

  let read_double ch =
    Int64.float_of_bits (read_i64 ch)

  let read_float ch =
    Int32.float_of_bits (read_real_i32 ch)

  let write_ui16 ch n =
    if n < 0 || n > 0xFFFF then raise (Overflow "write_ui16");
    write_byte ch (n lsr 8);
    write_byte ch n

  let write_i16 ch n =
    if n < -0x8000 || n > 0x7FFF then raise (Overflow "write_i16");
    if n < 0 then
      write_ui16 ch (65536 + n)
    else
      write_ui16 ch n

  let write_i32 ch n =
    write_byte ch (n asr 24);
    write_byte ch (n lsr 16);
    write_byte ch (n lsr 8);
    write_byte ch n

  let write_real_i32 ch n =
    let base = Int32.to_int n in
    let big = Int32.to_int (Int32.shift_right_logical n 24) in
    write_byte ch big;
    write_byte ch (base lsr 16);
    write_byte ch (base lsr 8);
    write_byte ch base

  let write_i64 ch n =
    write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical n 32));
    write_real_i32 ch (Int64.to_int32 n)

  let write_double ch f =
    write_i64 ch (Int64.bits_of_float f)

  let write_float ch f =
    write_real_i32 ch (Int32.bits_of_float f)

  let ui16s_of input     = make_enum read_ui16 input
  let i16s_of input      = make_enum read_i16 input
  let i32s_of input      = make_enum read_i32 input
  let real_i32s_of input = make_enum read_real_i32 input
  let i64s_of input      = make_enum read_i64 input
  let doubles_of input   = make_enum read_double input
  let floats_of input    = make_enum read_float input

end

(**
   {6 Bits API}
*)

type 'a bc = {
  ch : 'a;
  mutable nbits : int;
  mutable bits : int;
}

type in_bits = input bc
type out_bits = unit output bc

exception Bits_error

let input_bits ch =
  {
    ch = ch;
    nbits = 0;
    bits = 0;
  }

let output_bits ch =
  {
    ch = cast_output ch;
    nbits = 0;
    bits = 0;
  }

let rec read_bits b n =
  if b.nbits >= n then begin
    let c = b.nbits - n in
    let k = (b.bits asr c) land ((1 lsl n) - 1) in
    b.nbits <- c;
    k
  end else begin
    let k = read_byte b.ch in
    if b.nbits >= 24 then begin
      if n >= 31 then raise Bits_error;
      let c = 8 + b.nbits - n in
      let d = b.bits land ((1 lsl b.nbits) - 1) in
      let d = (d lsl (8 - c)) lor (k lsr c) in
      b.bits <- k;
      b.nbits <- c;
      d
    end else begin
      b.bits <- (b.bits lsl 8) lor k;
      b.nbits <- b.nbits + 8;
      read_bits b n;
    end
  end

let drop_bits b =
  b.nbits <- 0

let rec write_bits b ~nbits x =
  let n = nbits in
  if n + b.nbits >= 32 then begin
    if n > 31 then raise Bits_error;
    let n2 = 32 - b.nbits - 1 in
    let n3 = n - n2 in
    write_bits b ~nbits:n2 (x asr n3);
    write_bits b ~nbits:n3 (x land ((1 lsl n3) - 1));
  end else begin
    if n < 0 then raise Bits_error;
    if (x < 0 || x > (1 lsl n - 1)) && n <> 31 then raise Bits_error;
    b.bits <- (b.bits lsl n) lor x;
    b.nbits <- b.nbits + n;
    while b.nbits >= 8 do
      b.nbits <- b.nbits - 8;
      write_byte b.ch (b.bits asr b.nbits)
    done
  end

let flush_bits b =
  if b.nbits > 0 then write_bits b ~nbits:(8 - b.nbits) 0

(**
   {6 Generic BatIO}
*)


class in_channel ch =
  object
    method input s pos len = input ch s pos len
    method close_in() = close_in ch
  end

class out_channel ch =
  object
    method output s pos len = output ch s pos len
    method flush() = flush ch
    method close_out() = ignore(close_out ch)
  end

class in_chars ch =
  object
    method get() = try read ch with No_more_input -> raise End_of_file
    method close_in() = close_in ch
  end

class out_chars ch =
  object
    method put t = write ch t
    method flush() = flush ch
    method close_out() = ignore(close_out ch)
  end

let from_in_channel ch =
  let cbuf = String.create 1 in
  let read() =
    try
      if ch#input cbuf 0 1 = 0 then raise Sys_blocked_io;
      String.unsafe_get cbuf 0
    with
      End_of_file -> raise No_more_input
  in
  let input s p l =
    ch#input s p l
  in
  create_in
    ~read
    ~input
    ~close:ch#close_in

let from_out_channel ch =
  let cbuf = String.create 1 in
  let write c =
    String.unsafe_set cbuf 0 c;
    if ch#output cbuf 0 1 = 0 then raise Sys_blocked_io;
  in
  let output s p l =
    ch#output s p l
  in
  create_out
    ~write
    ~output
    ~flush:ch#flush
    ~close:ch#close_out

let from_in_chars ch =
  let input s p l =
    let i = ref 0 in
    try
      while !i < l do
        String.unsafe_set s (p + !i) (ch#get());
        incr i
      done;
      l
    with
      End_of_file when !i > 0 ->
      !i
  in
  create_in
    ~read:ch#get
    ~input
    ~close:ch#close_in

let from_out_chars ch =
  let output s p l =
    for i = p to p + l - 1 do
      ch#put (String.unsafe_get s i)
    done;
    l
  in
  create_out
    ~write:ch#put
    ~output
    ~flush:ch#flush
    ~close:ch#close_out

(**
   {6 Enumerations}
*)

let bytes_of        input = make_enum read_byte        input
let signed_bytes_of input = make_enum read_signed_byte input
let ui16s_of        input = make_enum read_ui16        input
let i16s_of         input = make_enum read_i16         input
let i32s_of         input = make_enum read_i32         input
let real_i32s_of    input = make_enum read_real_i32    input
let i64s_of         input = make_enum read_i64         input
let doubles_of      input = make_enum read_double      input
let floats_of       input = make_enum read_float       input
let strings_of      input = make_enum read_string      input
let lines_of        input = make_enum read_line        input
let chunks_of n     input = make_enum (fun ic -> nread ic n) input

(** The number of chars to read at once *)
let buffer_size = 1024 (*Arbitrary size.*)

(* make a bunch of char enums by reading buffer_size at a time and
   concat them all into into one big char enum *)
let chars_of input =
  let do_close = ref true in
  close_at_end do_close input (BatEnum.concat (BatEnum.from (fun () ->
        apply_enum do_close (fun source -> string_enum (nread source buffer_size)) input)))

let bits_of input =
  let do_close = ref true in
  close_at_end do_close input.ch (BatEnum.from (fun () -> apply_enum do_close read_bits input 1))

(** Buffered lines_of, for performance.  Ideas taken from ocaml stdlib *)
let lines_of2 ic =
  let buf = String.create buffer_size in
  let read_pos = ref 0 in (* next byte to read *)
  let end_pos = ref 0 in (* place to write new data *)
  let find_eol () =
    let rec find_loop pos =
      if pos >= !end_pos then !read_pos - pos
      else if buf.[pos] = '\n' then 1 + pos - !read_pos (* TODO: HANDLE CRLF *)
      else find_loop (pos+1)
    in
    find_loop !read_pos
  in
  let rec join_strings buf pos = function
    | [] -> buf
    | h::t ->
      let len = String.length h in
      String.blit h 0 buf (pos-len) len;
      join_strings buf (pos-len) t
  in
  let input_buf s o l =
    String.blit buf !read_pos s o l;
    read_pos := !read_pos + l;
    if !end_pos = !read_pos then
      try
        if !end_pos >= buffer_size then begin
          read_pos := 0;
          end_pos := input ic buf 0 buffer_size;
        end else begin
          let len_read = input ic buf 0 (buffer_size - !end_pos) in
          end_pos := !end_pos + len_read;
        end
      with No_more_input -> end_pos := !read_pos;
  in
  let get_line () =
    let rec get_pieces accu len =
      let n = find_eol () in
      if n = 0 then match accu with  (* EOF *)
        | [] -> close_in ic; raise BatEnum.No_more_elements
        | _ -> join_strings (String.create len) len accu
      else if n > 0 then (* newline found *)
        let res = String.create (n-1) in
        input_buf res 0 (n-1);
        input_buf " " 0 1; (* throw away EOL *)
        match accu with
        | [] -> res
        | _ -> let len = len + n-1 in
          join_strings (String.create len) len (res :: accu)
      else (* n < 0 ; no newline found *)
        let piece = String.create (-n) in
        input_buf piece 0 (-n);
        get_pieces (piece::accu) (len-n)
    in
    get_pieces [] 0
  in
  (* prime the buffer *)
  end_pos := input ic buf 0 buffer_size;
  BatEnum.from get_line


let write_bitss ~nbits output enum = write_enum (write_bits ~nbits) output enum

(**
   {6 Utilities}
*)
let is_newline = function '\010' | '\013' -> true | _ -> false

let tab_out ?(tab=' ') n out =
  let spaces   = String.make n tab in
  wrap_out
    ~write: (fun c ->
      write out c;
      if is_newline c then nwrite out spaces;
    )
    ~output:(fun s p l -> (*Replace each newline within the segment with newline^spaces*) (*FIXME?: performance - instead output each line and a newline between each char? *)
      let length = String.length s                 in
      let buffer = Buffer.create (String.length s) in
      for i = p to min (length - 1) l do
        let c = String.unsafe_get s i in
        Buffer.add_char buffer c;
        if is_newline c then
          Buffer.add_string buffer spaces
      done;
      let s' = Buffer.contents buffer                  in
      output out s' 0 (String.length s'))
    ~flush:noop
    ~close:noop
    ~underlying:[out]

(*
let lmargin n (p:_ output -> 'a -> unit) out x =
  p (tab_out n (cast_output out)) x
*)

let comb (a,b) =
  create_out ~write:(fun c ->
    write a c;
    write b c)
    ~output:(fun s i j ->
      let _ = output a s i j in
      output b s i j)
    ~flush:(fun () ->
      flush a;
      flush b)
    ~close:(fun () ->
      ignore (close_out a); close_out b)


(*let repeat n out =
  wrap_out
    ~underlying:[out]
    ~write:(fun c -> for i = 1 to n do write out c)
    ~output:(fun s p l -> for i = 1 to n do output out s p l)
    ~close:(fun () -> flush out)*)


(*let copy input output = write_chunks output (chunks_of default_buffer_size input)*)
(*let copy input output = write_chars output (chars_of input)*)

let copy ?(buffer=4096) inp out =
  let n   = buffer          in
  let buf = String.create n in
  try
    while true do
      let len = input inp buf 0 n in
      if len = 0 then raise No_more_input
      else            ignore (really_output out buf 0 len)
    done
  with No_more_input -> ()

(*let fast_chunks_of n inp  =
  let buffer = String.create n in
    make_enum (fun inp -> input inp buffer 0 n) input*)



(*
(** {6 Test} *)
let in_channel_of_input i =
  let (cin, cout) = Unix.pipe () in

  let latest_pos_in  = ref 0     in
  let rec aux ()  =
    let new_pos_in = pos_in  cin in
      if new_pos_in > !latest_pos_in then (*Something has been read, we can write a little bit more*)
    let size = new_pos_in - !latest_pos_in in
    let buf  = String.create size          in
      input i buf

(*    UnixLabels.select
      ~read:?
      ~write:*)

(*  let (fin, fout) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM in
  let cin         = open_in  fin
  and cout        = open_out fout in
  let rec aux () =
    let c = read i in
      Pervasives.output_char cout c;
      aux ()*)
*)

(**
   {6 Thread-safety}
*)

let lock_factory = ref (fun () -> BatConcurrent.nolock)


let synchronize_in ?(lock = !lock_factory ()) inp =
  wrap_in
    ~read:(BatConcurrent.sync lock (fun () -> read inp))
    ~input:(BatConcurrent.sync lock (fun s p l -> input inp s p l))
    ~close:noop
    ~underlying:[inp]

let synchronize_out ?(lock = !lock_factory ()) out =
  wrap_out
    ~write: (BatConcurrent.sync lock (fun c     -> write out c))
    ~output:(fun s p -> BatConcurrent.sync lock (fun l -> output out s p l))
    ~flush: (BatConcurrent.sync lock (fun ()    -> flush out))
    ~close: noop
    ~underlying:[out]

(**
   {6 Things that require temporary files}
*)

(**
   [to_input_channel inp] converts [inp] to an [in_channel].

   In the simplest case, [inp] maps to a file descriptor, which makes
   it possible to just reopen the same file descriptor as an
   [in_channel]. There is no flushing with which to screw up and
   this shouldn't interfere with [pos_in] et al. because [inp]
   maps {e directly} to a file descriptor, not through higher-level
   abstract streams.

   Otherwise, read everything, write it to a temporary file and
   read it back as an [in_channel].
   Yes, this is prohibitively expensive.
*)
let to_input_channel inp =
  try Unix.in_channel_of_descr (BatUnix.descr_of_input inp) (*Simple case*)
  with Invalid_argument "Unix.descr_of_in_channel" ->            (*Bad, bad case*)
    (* FIXME: this 'pipe' is never deleted *)
    let (name, cout) =
      Filename.open_temp_file ~mode:[Open_binary] "ocaml" "pipe" in
    let out          = output_channel cout                    in
    copy inp out;
    close_out out;
    Pervasives.open_in_bin name




(*(**
   Copy everything to a temporary file
 *)
  let out_channel_of_output out =
  let (name, cout) = Filename.open_temp_file "ocaml" "tmp" in
    create_out

    cout*)

let to_string print_x x =
  let out = output_string () in
  print_x out x;
  close_out out

let to_f_printer printer =
  fun fmt t -> Format.pp_print_string fmt (to_string printer t)

module Incubator = struct
  module Array = struct
    let pp ?(flush = false) ?(first = "[|") ?(last = "|]") ?(sep = "; ") ?(indent = String.length first) pp f a =
      let open Format in
      pp_open_box f indent;
      pp_print_cut f ();
      pp_print_string f first;
      pp_print_cut f ();

      for i = 0 to Array.length a - 2 do
        pp_open_box f indent;
        pp f a.(i);
        pp_print_string f sep;
        pp_close_box f ();
        pp_print_cut f ();
      done;

      if Array.length a > 0 then (
        (* Print the last element without a trailing separator *)
        pp_open_box f indent;
        pp f a.(Array.length a - 1);
        pp_close_box f ();
      );

      pp_print_cut f ();
      pp_print_string f last;
      pp_close_box f ();
      if flush then pp_print_flush f ()
  end

  module Enum = struct
    let pp ?(flush = false) ?(first = "") ?(last = "") ?(sep = " ") ?(indent = String.length first) pp f e =
      let open Format in
      pp_open_box f indent;
      pp_print_cut f ();
      pp_print_string f first;
      pp_print_cut f ();
      match BatEnum.get e with
      | None ->
        pp_print_string f last;
        pp_close_box f ();
        if flush then pp_print_flush f ()
      | Some x ->
        pp_open_box f indent;
        pp f x;
        let rec aux () =
          match BatEnum.get e with
          | None ->
            pp_close_box f ();
            pp_print_cut f ();
            pp_print_string f last;
            pp_close_box f ();
            if flush then pp_print_flush f ()
          | Some x ->
            pp_print_string f sep;
            pp_close_box f ();
            pp_print_cut f ();
            pp_open_box f indent;
            pp f x;
            aux ()
        in
        aux ()
  end

  module List = struct
    let pp ?(flush = false) ?(first = "[") ?(last = "]") ?(sep = "; ") ?(indent = String.length first) pp f l =
      let open Format in
      pp_open_box f indent;
      pp_print_cut f ();
      pp_print_string f first;
      pp_print_cut f ();
      match l with
      | [] ->
        pp_print_string f last;
        pp_close_box f ();
        if flush then pp_print_flush f ()
      | hd :: tl ->
        pp_open_box f indent;
        pp f hd;
        let rec aux rem =
          match rem with
          | [] ->
            pp_close_box f ();
            pp_print_cut f ();
            pp_print_string f last;
            pp_close_box f ();
            if flush then pp_print_flush f ()
          | hd :: tl ->
            pp_print_string f sep;
            pp_close_box f ();
            pp_print_cut f ();
            pp_open_box f indent;
            pp f hd;
            aux tl
        in
        aux tl
  end
end
