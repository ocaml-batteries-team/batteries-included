(*
  Requires batteries, benchmark, bitstring
  To compile:
  ocamlopt t_read_stub.c
  ocamlfind ocamlopt -linkpkg -thread -package batteries,threads,benchmark,bitstring t_read.ml -o t_read_stub.o t_read
*)

open Benchmark

(**** THIS CODE UNDER GPL LICENSE FROM CDK/extlib ****)
let resize s newlen =
  let len = String.length s in
  if len > newlen then String.sub s 0 newlen
  else
  let str = String.create newlen in
  String.blit s 0 str 0 len;
  str

let cdk_read buf_size name =
  let chan = open_in name in
  let buf = String.create buf_size in
  let rec iter buf nb_read =
    let buf_size = String.length buf in
    let tmp = input chan buf nb_read (buf_size - nb_read) in
    if tmp = 0 then
      String.sub buf 0 nb_read
    else
      let nb_read = nb_read + tmp in
      let buf =
	if nb_read = buf_size then
          resize buf (2 * buf_size)
	else buf
      in
      iter buf nb_read
  in
  let buf = iter buf 0 in
  close_in chan;
  buf
(**** END CDK/extlib code ****)


let varbuf_unix tmpsize fn =
  let fd = Unix.openfile fn [Unix.O_RDONLY] 0o600 in
  let buf = Buffer.create tmpsize in
  let tmp = String.create tmpsize in
  let rec loop () =
    let n = Unix.read fd tmp 0 tmpsize in
    if n > 0 then (
      Buffer.add_substring buf tmp 0 n;
      loop ()
    )
  in
  loop ();
  Buffer.contents buf

let varbuf_perv tmpsize fn =
  let fd = open_in fn in
  let buf = Buffer.create tmpsize in
  let tmp = String.create tmpsize in
  let rec loop () =
    let n = input fd tmp 0 tmpsize in
    if n > 0 then (
      Buffer.add_substring buf tmp 0 n;
      loop ()
    )
  in
  loop ();
  Buffer.contents buf

let read_file_as_str fn =
  let ic = Pervasives.open_in_bin fn in
  let len = (Pervasives.in_channel_length ic) in
  let old_gc = Gc.get() in
  Gc.set {old_gc with Gc.space_overhead = 0};
  let ret = String.create len in
  Gc.set old_gc;
  Pervasives.really_input ic ret 0 len;
  Pervasives.close_in ic;
  ret

open Bigarray

type bigstring_t = {
  bigarr : (char, int8_unsigned_elt, c_layout) Array1.t;
  data : string; length : int
}


let map_file fd ?pos ?(shared=false) len =
  let ba = Array1.map_file fd ?pos char c_layout shared len in
  let s = (Obj.magic (Obj.field (Obj.repr ba) 1) : string) in
    { bigarr = ba; data = s; length = Array1.dim ba }

let mmap_fn fn =
  let fd = Unix.openfile fn [Unix.O_RDONLY] 0o600 in
  let len = (Unix.stat fn).Unix.st_size in
  (map_file fd len).bigarr



type buffer = (char, int8_unsigned_elt, c_layout) Array1.t
external pread : Unix.file_descr -> buffer -> int64 -> int = "caml_maid_pread"

let pread_file fn =
  let fd = Unix.openfile fn [Unix.O_RDONLY] 0o600 in
  let len = (Unix.stat fn).Unix.st_size in
  let buf = Array1.create char c_layout len in
  ignore(pread fd buf 0L);
  buf


open Batteries

let batio_read fn = File.with_file_in fn BatIO.read_all

let check str = String.iter ignore str
let check_bs (str,_,_) = String.iter ignore str
let check_ba (ba: buffer) =
  for i = 0 to Array1.dim ba - 1 do
    Array1.unsafe_get ba i |> ignore
  done

let tests fn =
    [
      "mmap_fn", mmap_fn |- check_ba, fn;
      "pread", pread_file |- check_ba, fn;
      "batio", batio_read |- check, fn;
      "cdk_orig", cdk_read 1024 |- check, fn;
      "cdk2k", cdk_read 2048 |- check, fn;
      "cdk4k", cdk_read 4096 |- check, fn;
      "vbu1k", varbuf_unix 1024 |- check, fn;
      "vbu2k", varbuf_unix 2048 |- check, fn;
      "vbu4k", varbuf_unix 4096 |- check, fn;
      "vbp1k", varbuf_perv 1024 |- check, fn;
      "vbp2k", varbuf_perv 2048 |- check, fn;
      "vbp4k", varbuf_perv 4096 |- check, fn;
      "bitstring", Bitstring.bitstring_of_file |- check_bs, fn;
      "str_only", read_file_as_str |- check, fn;
    ]

let () =
  let fn = Sys.argv.(1) in
  latencyN 30L (tests fn) |> tabulate
