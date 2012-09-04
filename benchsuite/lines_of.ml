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

type 'a weak_set = ('a, unit) BatInnerWeaktbl.t
type input = {
  mutable in_read  : unit -> char;
  mutable in_input : string -> int -> int -> int;
  mutable in_close : unit -> unit;
  in_id: int;(**A unique identifier.*)
  in_upstream: input weak_set
}

let unread_string str pos len input =
  let limit = pos + len in
  let curr = ref pos in
  let restore =
    let old_read = input.in_read in
    let old_input = input.in_input in
    fun () ->
      input.in_read <- old_read;
      input.in_input <- old_input;
      ()
  in
  input.in_read <- (fun () ->
    if !curr = limit then begin
      restore ();
      input.in_read ()
    end
    else begin
      incr curr;
      str.[!curr-1]
    end);
  input.in_input <- (fun s p l ->
    let curr' = !curr + l in
    if curr' < limit then begin
      String.blit str !curr s p l;
      curr := curr';
      l
    end else begin
      let l1 = limit - !curr in
      String.blit str !curr s p l1;
      restore ();
      let l2 = input.in_input s (p + l1) (l - l1) in
      l1 + l2
    end);
  ()

let read_line2 =
  fun input ->
    let input = Obj.magic input in (* compensate for abstract input type *)
    let buff_len = 256 in
    let buff = String.create buff_len in
    let b = Buffer.create buff_len in
    let rec find_chunk () =
      let nread = input.in_input buff 0 buff_len in
      let rec loop i =
        if i = nread then None
	else
          if buff.[i] = '\n' then Some i
          else loop (i + 1) in
      match loop 0 with
        | Some i ->
          Buffer.add_substring b buff 0 i;
          (* 'i+1' because we skip the newline *)
          if i+1 < nread then
            unread_string buff (i+1) (nread - i - 1) input;
          Buffer.contents b
        | None ->
          Buffer.add_substring b buff 0 nread;
          if nread < buff_len then begin
            Buffer.contents b
          end else
            find_chunk ()
    in find_chunk ()

(** [apply_enum f x] applies [f] to [x] and converts exceptions
    [No_more_input] and [Input_closed] to [BatEnum.No_more_elements]*)
let apply_enum do_close f x =
  try f x
  with
    | BatIO.No_more_input -> raise BatEnum.No_more_elements
    | BatInnerIO.Input_closed  -> do_close := false; raise BatEnum.No_more_elements

(** [close_at_end input e] returns an enumeration which behaves as [e]
    and has the secondary effect of closing [input] once everything has
    been read.*)
let close_at_end do_close (input:BatIO.input) e =
  BatEnum.suffix_action (fun () -> if !do_close then BatIO.close_in input) e

let make_enum f input =
  let do_close = ref true in
  close_at_end do_close input (BatEnum.from (fun () -> apply_enum do_close f input))

let rfb4 fn = BatList.of_enum (make_enum read_line2 (BatFile.open_in fn))

let unread_string2 str pos len input =
  let limit = pos + len in
  let curr = ref pos in
  let restore =
    let old_read = input.in_read in
    let old_input = input.in_input in
    fun () ->
      input.in_read <- old_read;
      input.in_input <- old_input;
      ()
  in
  input.in_read <- (fun () ->
    if !curr = limit then begin
      restore ();
      input.in_read ()
    end
    else begin
      incr curr;
      str.[!curr-1]
    end);
  input.in_input <- (fun s p l ->
    let curr' = !curr + l in
    if curr' < limit then begin
      String.blit str !curr s p l;
      curr := curr';
      l
    end else begin
      let l1 = limit - !curr in
      String.blit str !curr s p l1;
      restore ();
      l1
    end);
  ()

let read_line3 =
  fun input ->
    let input = Obj.magic input in (* compensate for abstract input type *)
    let buff_len = 256 in
    let buff = String.create buff_len in
    let b = Buffer.create buff_len in
    let rec find_chunk () =
      let nread = input.in_input buff 0 buff_len in
      let rec loop i =
        if i = nread then None
	else
          if buff.[i] = '\n' then Some i
          else loop (i + 1) in
      match loop 0 with
        | Some i ->
          Buffer.add_substring b buff 0 i;
          (* 'i+1' because we skip the newline *)
          if i+1 < nread then
            unread_string2 buff (i+1) (nread - i - 1) input;
          Buffer.contents b
        | None ->
          Buffer.add_substring b buff 0 nread;
          if nread < buff_len then begin
            Buffer.contents b
          end else
            find_chunk ()
    in find_chunk ()

let rfb5 fn = BatList.of_enum (make_enum read_line3 (BatFile.open_in fn))

let () =
  Bench.config.Bench.samples <- 300;
  let funs = [ "readfile", readfile;
	       "readfile_batteries", readfile_batteries;
	       "file_lines_of", rfb2;
	       "lines_of2", rfb3;
	       "push_lines_of", rfb4;
	       "push_lines_of2", rfb5;
	     ] in
  let results = Bench.bench_funs funs "setup.ml" in
  print_endline "For reading setup.ml into a list, ";
  Bench.summarize results
