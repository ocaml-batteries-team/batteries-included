(*
 * BatInnerIO - Abstract input/output (inner module)
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 Philippe Strauss
 *               2008 David Teller
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


type 'a weak_set = ('a, unit) BatInnerWeaktbl.t
let weak_create size     = BatInnerWeaktbl.create size
let weak_add set element = BatInnerWeaktbl.add set element ()
let weak_iter f s        = BatInnerWeaktbl.iter (fun x _ -> f x) s

type input = {
  mutable in_read  : unit -> char;
  mutable in_input : string -> int -> int -> int;
  mutable in_close : unit -> unit;
  in_id: int;(**A unique identifier.*)
  in_upstream: input weak_set
}

type 'a output = {
  mutable out_write : char -> unit;
  mutable out_output: string -> int -> int -> int;
  mutable out_close : unit -> 'a;
  mutable out_flush : unit -> unit;
  out_id:    int;(**A unique identifier.*)
  out_upstream:unit output weak_set
    (** The set of outputs which have been created to write to this output.*)
}


module Input =
struct
  type t = input
  let compare x y = x.in_id - y.in_id
  let hash    x   = x.in_id
  let equal   x y = x.in_id = y.in_id
end

module Output =
struct
  type t = unit output
  let compare x y = x.out_id - y.out_id
  let hash    x   = x.out_id
  let equal   x y = x.out_id = y.out_id
end



(**All the currently opened outputs -- used to permit [flush_all] and [close_all].*)
(*module Inputs = Weaktbl.Make(Input)*)
module Outputs= Weak.Make(Output)



(** {6 Primitive operations}*)

external noop        : unit      -> unit        = "%ignore"
external cast_output : 'a output -> unit output = "%identity"
let lock = ref BatConcurrent.nolock


let outputs = Outputs.create 32
let outputs_add out =
  BatConcurrent.sync !lock (Outputs.add outputs) out

let outputs_remove out =
  BatConcurrent.sync !lock (Outputs.remove outputs) out


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

let uid = ref 0
let uid () = post_incr uid

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
      in_upstream = weak_create 2
    }
  in
  BatConcurrent.sync !lock (List.iter (fun x -> weak_add x.in_upstream result)) underlying;
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
  weak_iter close_unit o.out_upstream;
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
      out_upstream = weak_create 2
    }
  in
  let o = cast_output out in
  BatConcurrent.sync !lock (List.iter (fun x -> weak_add x.out_upstream o)) underlying;
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


let write o x = o.out_write x

let nwrite o s =
  let p = ref 0 in
  let l = ref (String.length s) in
  while !l > 0 do
    let w = o.out_output s !p !l in
    (* FIXME: unknown how many characters were already written *)
    if w = 0 then raise Sys_blocked_io;
    p := !p + w;
    l := !l - w;
  done

let output o s p l =
  let sl = String.length s in
  if p + l > sl || p < 0 || l < 0 then invalid_arg "BatIO.output";
  o.out_output s p l

let flush o = o.out_flush()

let flush_all () =
  BatConcurrent.sync !lock ( Outputs.iter (fun o -> try flush o with _ -> ())) outputs

let close_all () =
  let outs =
    BatConcurrent.sync !lock (Outputs.fold (fun o os -> o :: os) outputs) []
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
    in_id    = (-1);
    in_upstream= weak_create 0 }
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
          (*		   Printf.eprintf "Cleaning up\n%!";*)
          Pervasives.close_out ch
        end
      else fun () ->
        begin
          (*		   Printf.eprintf "Not cleaning up\n%!";*)
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

let get_output_id out = out.out_id
let get_input_id  inp = inp.in_id
