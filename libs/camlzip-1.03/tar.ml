
exception Error of string

(* Types of files recongized by tar *)
type file_type = REGULAR | LINK | SYMLINK | CHRSPEC | BLKSPEC | DIRECTORY | FIFO  | CONTIGIOUS | DUMPDIR (** GNU: A dir entry that contains the names of files that were in the dir at the time the dump was made. *) | LONGLINK (** GNU: Identifies the *next* file on the archive as having a long linkname *)| LONGNAME (** GNU: Identifies the *next* file on the tape as having a long name. *) | MULTIVOL (** GNU: The continuation of a file that began on another volume. *) | NAMES (** GNU: For storing filenames tha do not fit into the main header. *) | SPARSE (** GNU: Sparse file *) | VOLHDR (** GNU: This file is a tape/volume header. Ignore it on extraction. *)

type record_type = POSIX_FORMAT | GNU_FORMAT | OLDGNU_FORMAT | V7_FORMAT

(* The size of a header block *)
let blocksize = 512 

(* The metadata for a file in a tar archive *)
type header = {
  t_name: string;
  t_mode: int;
  t_uid: int;
  t_gid: int;
  t_size: int;
  t_mtime: int32;
  t_chksum: int;
  t_typeflag: file_type;
  t_linkname: string;
  t_format: record_type;
  t_uname: string;
  t_gname: string;
  t_devmajor: int;
  t_devminor: int;
  t_prefix: string;
  t_gnu: gnu_extras option
}
and gnu_extras = {
  t_atime: int32;
  t_ctime: int32;
  t_offset: int32;
  t_realsize: int32;
}

class in_chan i = object
  method really_input str pos len = 
    try Pervasives.really_input i str pos len with
	End_of_file -> raise (Error "Unexpected end of file")
  method input str pos len = 
    try Pervasives.input i str pos len with
	End_of_file -> raise (Error "Unexpected end of file")
  method dispose () = ()
  method close () = Pervasives.close_in i
end

class gzin_chan i = object
  method really_input str pos len =
    try Gzip.really_input i str pos len with
	End_of_file -> raise (Error "Unexpected end of file")
  method input str pos len =
    try Gzip.input i str pos len with
	End_of_file -> raise (Error "Unexpected end of file")
  method dispose () = Gzip.dispose i
  method close () = Gzip.close_in i
end

class bzin_chan i = object
  method really_input str pos len =
    try Bzip2.really_input i str pos len with
	End_of_file -> raise (Error "Unexpected end of file")
  method input str pos len =
    try Bzip2.input i str pos len with
	End_of_file -> raise (Error "Unexpected end of file")
  method dispose () = Bzip2.dispose i
  method close () = Bzip2.close_in i
end


let open_inchan comp chan = 
  match comp with
    | `Plain -> new in_chan chan
    | `Gzip -> new gzin_chan (Gzip.open_in_chan chan)
    | `Bzip2 -> new bzin_chan (Bzip2.open_in_chan chan)

let pick_comp_type filename = function
  | `Gzip -> `Gzip
  | `Bzip2 -> `Bzip2
  | `Plain -> `Plain
  | `Guess ->
      if Filename.check_suffix filename ".tar" then
	`Plain
      else if Filename.check_suffix filename ".bz2" then
	`Bzip2
      else if Filename.check_suffix filename ".gz"
	|| Filename.check_suffix filename ".Z"
	|| Filename.check_suffix filename ".tgz" then
	  `Gzip
      else
	`Plain

type t_in = {
  chan: in_chan;
  rawchan: in_channel;
  mutable last_header: header option;
}

let open_in_chan ?(compress=`Plain) chan =
  {
    chan = open_inchan compress chan;
    rawchan = chan;
    last_header = None;
  }

let open_in ?(compress=`Guess) filename =
  open_in_chan ~compress:(pick_comp_type filename compress) (open_in_bin filename)


let dispose t = t.chan#dispose ()
and close_in t = t.chan#close ()

(* Add Error Checking! *)
let c_string raw start = 
  let nul = String.index_from raw start '\000' in
    String.sub raw start (nul - start)

(* Numbers are /supposed/ to be 0 padded, octal, with a trailing "\000 ". About the only thing that's universal about this is octal. *)
let trim_spaces str pos len =
  let start = ref pos 
  and stop = ref (pos + len - 1) in
    while str.[!start] = ' ' do incr start done;
    while str.[!stop] = ' ' || str.[!stop] = '\000' do decr stop done;
    String.sub str !start (!stop - !start + 1)

let extract_num raw pos len = 
  if raw.[pos] = '\000' then 0
  else try
    int_of_string ("0o" ^ (trim_spaces raw pos len))
  with Failure x -> raise (Error "Invalid number in header")

let extract_int32 raw pos len = 
  if raw.[pos] = '\000' then 0l
  else try    
    Int32.of_string ("0o" ^ trim_spaces raw pos len)
  with Failure x ->  raise (Error "Invalid number in header")

let typeflag = function
  | '0' | '\000' -> REGULAR
  | '1' -> LINK
  | '2' -> SYMLINK
  | '3' -> CHRSPEC
  | '4' -> BLKSPEC
  | '5' -> DIRECTORY
  | '6' -> FIFO
  | '7' -> CONTIGIOUS
  | 'D' -> DUMPDIR
  | 'K' -> LONGLINK
  | 'L' -> LONGNAME
  | 'M' -> MULTIVOL
  | 'N' -> NAMES
  | 'S' -> raise (Error "Sparse files are not supported")
  | 'V' -> VOLHDR
  | _ -> raise (Error "Unknown file type")

let align_at_header t =
  match t.last_header with
    | None -> ()
    | Some h ->
	let entry_size = ((h.t_size/blocksize) + 1) * blocksize
	and buf = String.create blocksize
	and discarded = ref 0 in
	  while !discarded < entry_size do
	    let read = t.chan#input buf 0 blocksize in
	      discarded := !discarded + read
	  done;
	  t.last_header <- None

let empty_block = String.make blocksize '\000'

let compute_chksum buf = 
  let chksum = ref 256 in (* 256 is the sum of 8 ' ' characters for the chksum field *)
    for i = 0 to 147 do
      chksum := !chksum + Char.code buf.[i]
    done;
    for i = 156 to 511 do
      chksum := !chksum + Char.code buf.[i]
    done;
    !chksum

let read_magic header typec = 
  let magic = String.sub header 257 8 in
    match magic with
      | "ustar  \000" -> OLDGNU_FORMAT
      | "ustar\00000" -> begin match typec with
	  | '0' .. '7' -> POSIX_FORMAT | _ -> GNU_FORMAT
	end
      | _ -> V7_FORMAT

let read_oldgnu_header header =
  { t_atime = extract_int32 header 345 12;
    t_ctime = extract_int32 header 357 12;
    t_offset = extract_int32 header 369 12;
    t_realsize = extract_int32 header 483 12;
  }

let read_gnu_header t =
  let buf = String.create blocksize in
    t.chan#really_input buf 0 blocksize;
    { t_atime = extract_int32 buf 0 12;
      t_ctime = extract_int32 buf 12 12;
      t_offset = extract_int32 buf 24 12;
      t_realsize = extract_int32 buf 36 12;
    }

let read_header t =
  align_at_header t;
  let buf = String.create blocksize in
    t.chan#really_input buf 0 blocksize;
    if buf = empty_block then raise End_of_file;
    let head1 = { t_name = c_string buf 0;
		 t_mode = extract_num buf 100 8;
		 t_uid = extract_num buf 108 8;
		 t_gid = extract_num buf 116 8;
		 t_size = extract_num buf 124 12;
		 t_mtime = extract_int32 buf 136 12;
		 t_chksum = extract_num buf 148 8;
		 t_typeflag = typeflag buf.[156];
		 t_linkname = c_string buf 157;
		 t_format = read_magic buf buf.[156];
		 t_uname = c_string buf 265;
		 t_gname = c_string buf 297;
		 t_devmajor = extract_num buf 329 8;
		 t_devminor = extract_num buf 337 8;
		 t_prefix = String.sub buf 345 155;
		 t_gnu = None;
	       } in
    let chksum = compute_chksum buf in
      if chksum <> head1.t_chksum then
	raise (Error (Printf.sprintf "Invalid checksum in tar header. Calculated %d, expected %d" chksum head1.t_chksum));
      let head =
	if head1.t_format = OLDGNU_FORMAT then
	  {head1 with t_gnu = Some (read_oldgnu_header buf) }
	else if head1.t_format = GNU_FORMAT then
	  {head1 with t_gnu = Some (read_gnu_header t) }
	else
	  head1 in	
      t.last_header <- Some head;
      head

let align_at_body t =
  match t.last_header with
    | Some _ -> ()
    | None -> ignore (read_header t)

let get_header t =
  match t.last_header with
    | Some h -> h
    | None -> raise (Error "Missing tar header?")
    
let read_body t =
  align_at_body t;
  let header = get_header t in
    t.last_header <- None;
    if header.t_size = 0 then "" 
    else let buf = String.create header.t_size in
      t.chan#really_input buf 0 header.t_size;
      let align = blocksize - (header.t_size mod blocksize) in
	if align <> blocksize then begin
	  let leftover = String.create blocksize in	    
	    t.chan#really_input leftover 0 align
	end;
	buf

let read_entry t =
  let head = read_header t in
    head, read_body t

class out_chan o = object
  method output str pos len = Pervasives.output o str pos len
  method flush () = Pervasives.flush o
  method close () = Pervasives.close_out o
end

class gzout_chan o = object
  method output str pos len = Gzip.output o str pos len
  method flush () = Gzip.flush o
  method close () = Gzip.close_out o
end

class bzout_chan o = object
  method output str pos len = Bzip2.output o str pos len
  method flush () = Bzip2.flush o
  method close () = Bzip2.close_out o
end


let open_outchan comp chan = 
  match comp with
    | `Plain -> new out_chan chan
    | `Gzip -> new gzout_chan (Gzip.open_out_chan chan)
    | `Bzip2 -> new bzout_chan (Bzip2.open_out_chan chan)

type t_out = {
  ochan: out_chan;
  rawochan: out_channel;
}

let open_out_chan ?(compress=`Plain) chan =
  {
    ochan = open_outchan compress chan;
    rawochan = chan;
  }

let open_out ?(compress=`Plain) filename =
  open_out_chan ~compress (open_out_bin filename)

let write_str buf pos width str =
  let len = min (String.length str) (width - 1) in
    String.blit str 0 buf pos len

let write_num8 buf pos n =
  let as_str = Printf.sprintf "%07o" n in
    String.blit as_str 0 buf pos 7

let write_num12 buf pos n =
  let as_str = Printf.sprintf "%011o" n in
    String.blit as_str 0 buf pos 11

let write_int32 buf pos n =
  let as_str = Printf.sprintf "%011lo" n in
    String.blit as_str 0 buf pos 11

let write_padded_num buf pos n =
  let as_str = Printf.sprintf "%07o\000 " n in
    String.blit as_str 0 buf pos 8

let write_magic buf pos magic =
  let str = match magic with
    | POSIX_FORMAT | GNU_FORMAT -> "ustar\00000"
    | OLDGNU_FORMAT -> "ustar  \0000"
    | V7_FORMAT -> "      \000"
  in
    String.blit str 0 buf pos 8

let typeflag_to_char = function
  | REGULAR -> '0'
  | LINK -> '1'
  | SYMLINK -> '2'
  | CHRSPEC -> '3'
  | BLKSPEC -> '4'
  | DIRECTORY -> '5'
  | FIFO -> '6'
  | CONTIGIOUS -> '7'
  | DUMPDIR -> 'D'
  | LONGLINK -> 'K'
  | LONGNAME -> 'L'
  | MULTIVOL -> 'M'
  | NAMES -> 'N'
  | SPARSE -> raise (Error "Sparse files aren't supported for output")
  | VOLHDR -> 'V'

let isdigit = function '0' .. '9' -> true | _ -> false

let write_oldgnu_header t buf =
  let ext = match t.t_gnu with
    | Some e -> e
    | None -> raise (Error "OLDGNU_FORMAT record without t_gnu field set") in
    write_int32 buf 345 ext.t_atime;
    write_int32 buf 356 ext.t_ctime;
    write_int32 buf 369 ext.t_offset;
    write_int32 buf 483 ext.t_realsize

let write_gnu_header t buf =
  let ext = match t.t_gnu with
    | Some e -> e
    | None -> raise (Error "GNU_FORMAT record without t_gnu field set") in
    write_int32 buf 0 ext.t_atime;
    write_int32 buf 12 ext.t_ctime;
    write_int32 buf 24 ext.t_offset;
    write_int32 buf 36 ext.t_realsize
  
let output t head body =
  let size = String.length body in
  let buf = String.copy empty_block in
    write_str buf 0 100 head.t_name;
    write_num8 buf 100 head.t_mode;
    write_num8 buf 108 head.t_uid;
    write_num8 buf 116 head.t_gid;
    write_num12 buf 124 size;
    write_int32 buf 136 head.t_mtime;
    buf.[156] <- typeflag_to_char head.t_typeflag;
    write_str buf 157 100 head.t_linkname;
    write_magic buf 257 head.t_format;
    write_str buf 265 32 head.t_uname;
    write_str buf 297 32 head.t_gname;
    write_num8 buf 329 head.t_devmajor;
    write_num8 buf 337 head.t_devminor;
    write_str buf 345 155 head.t_prefix;
    if head.t_format = OLDGNU_FORMAT then
      write_oldgnu_header head buf;
    let chksum = compute_chksum buf in
      write_padded_num buf 148 chksum;
      t.ochan#output buf 0 blocksize;
      if head.t_format = GNU_FORMAT && isdigit buf.[156] then begin
	let buf2 = String.copy empty_block in
	  write_gnu_header head buf2;
	  t.ochan#output buf2 0 blocksize
      end;
      if size > 0 then begin
	let blocks = size / blocksize in
	  for n = 0 to blocks do
	    let pos = n * blocksize in
	    let len =
	      if size - pos >= blocksize then
		blocksize
	      else
		size - pos in
	      t.ochan#output body (n * 512) len;
	  done;	 
	  let align = blocksize - (size mod blocksize) in
	    if align > 0 && align < blocksize then 
	      t.ochan#output empty_block 0 align
      end
      
let flush t =
  t.ochan#output empty_block 0 blocksize;
  t.ochan#flush ()

let close_out t =
  flush t;
  close_out t.rawochan
