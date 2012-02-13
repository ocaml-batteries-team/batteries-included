

let width = 100000
let op_count = 1000
let set_poss = Array.init op_count (fun _ -> Random.int width)
let clear_poss = Array.init op_count (fun _ -> Random.int width)
let get_poss = Array.init op_count (fun _ -> Random.int width)

let farr n =
  let s = Array.create width false in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      s.(Array.unsafe_get clear_poss i) <- false;
      s.(Array.unsafe_get set_poss i) <- true;
    done;
    for _b = 1 to 100 do
      for i = 0 to op_count-1 do
        let _bool : bool =
          s.(Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done

let farr2 n =
  let s = Array.create width false in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      Array.unsafe_set s (Array.unsafe_get clear_poss i) false;
      Array.unsafe_set s (Array.unsafe_get set_poss i) true;
    done;
    for _b = 1 to 100 do
      for i = 0 to op_count-1 do
        let _bool : bool =
          Array.unsafe_get s (Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done


open Batteries

let fbs  n =
  let s = BitSet.create width in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      BitSet.unset s (Array.unsafe_get clear_poss i);
      BitSet.set s (Array.unsafe_get set_poss i);
    done;
    for _b = 1 to 100 do
      for i = 0 to op_count-1 do
        let _bool : bool =
          BitSet.mem s (Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done


module BitSet2 = struct
  (* Internal type for bit data - a string, but with a new type *)
  type intern

  let bcreate : int -> intern = Obj.magic String.create
  external fast_get : intern -> int -> int = "%string_unsafe_get"
  external fast_set : intern -> int -> int -> unit = "%string_unsafe_set"
  external fast_bool : int -> bool = "%identity"
  let fast_blit : intern -> int -> intern -> int -> int -> unit = Obj.magic String.blit
  let fast_fill : intern -> int -> int -> int (* char *) -> unit = Obj.magic String.fill
  let fast_length : intern -> int= Obj.magic String.length

  (* Safe access functions, still internal *)
  (* Get a byte of intern [s] at index [idx] *)
  let bget s ndx =
    fast_get s ndx

  (* Set a byte of intern [s] at index [ndx] to [v] *)
  let bset s ndx v =
    fast_set s ndx v

  (* Blit from one intern to another intern - sizes and offsets in bytes *)
  let bblit src srcoff dst dstoff len =
    fast_blit src srcoff dst dstoff len

  (* Fill a range of bits with a byte pattern [c] *)
  let bfill dst start len c =
    fast_fill dst start len c

  type t = {mutable data: intern} (* internal string of bits *)

  let int_size = 7 (* value used to round up index *)
  let log_int_size = 3 (* number of shifts *)

  let create n = (* n is in bits *)
    if n < 0 then invalid_arg "BitSet.create: negative size";
    let size = (n+int_size) lsr log_int_size in
    let b = bcreate size in
    bfill b 0 size 0;
    {data = b}

  let set t x =
    if x < 0 then invalid_arg "BitSet.set: negative index";
    let pos = x lsr log_int_size and delta = x land int_size in
    let size = fast_length t.data in
    if pos >= size then begin
      let b = bcreate (pos+1) in
      bblit t.data 0 b 0 size;
      bfill b size (pos - size + 1) 0;
      t.data <- b;
    end;
    bset t.data pos ((bget t.data pos) lor (1 lsl delta))

  let unset t x =
    if x < 0 then invalid_arg "BitSet.unset: negative index";
    let pos = x lsr log_int_size and delta = x land int_size in
    if pos < fast_length t.data then
      bset t.data pos ((bget t.data pos) land (0xFF lxor (1 lsl delta)))

  let mem t x =
    if x < 0 then invalid_arg "BitSet.mem";
    let pos = x lsr log_int_size and delta = x land int_size in
    let size = fast_length t.data in
    if pos < size then
      fast_bool (((bget t.data pos) lsr delta) land 1)
    else
      false

end

let fbs2 n =
  let s = BitSet2.create width in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      BitSet2.unset s (Array.unsafe_get clear_poss i);
      BitSet2.set s (Array.unsafe_get set_poss i);
    done;
    for _b = 1 to 100 do
      for i = 0 to op_count-1 do
        let _bool : bool =
          BitSet2.mem s (Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done

module BitSet3 = struct
  (* Internal type for bit data - a string, but with a new type *)
  type intern

  let bcreate : int -> intern = Obj.magic String.create
  external fast_get : intern -> int -> int = "%string_unsafe_get"
  external fast_set : intern -> int -> int -> unit = "%string_unsafe_set"
  external fast_bool : int -> bool = "%identity"
  let fast_blit : intern -> int -> intern -> int -> int -> unit = Obj.magic String.blit
  let fast_fill : intern -> int -> int -> int (* char *) -> unit = Obj.magic String.fill
  let fast_length : intern -> int= Obj.magic String.length

  type t = intern (* internal string of bits *)

  let int_size = 7 (* value used to round up index *)
  let log_int_size = 3 (* number of shifts *)

  let create n = (* n is in bits *)
    if n < 0 then invalid_arg "BitSet.create: negative size";
    let size = (n+int_size) lsr log_int_size in
    let b = bcreate size in
    fast_fill b 0 size 0;
    b

  let set t x =
    let size = fast_length t in
    let pos = x lsr log_int_size and delta = x land int_size in
    if x < 0 || pos >= size then invalid_arg "BitSet.set: invalid index";
    fast_set t pos ((fast_get t pos) lor (1 lsl delta))

  let unset t x =
    let size = fast_length t in
    let pos = x lsr log_int_size and delta = x land int_size in
    if x < 0 || pos >= size then invalid_arg "BitSet.unset: invalid index";
    fast_set t pos ((fast_get t pos) land (lnot (1 lsl delta)))

  let mem t x =
    let size = fast_length t in
    let pos = x lsr log_int_size and delta = x land int_size in
    if x < 0 || pos >= size then invalid_arg "BitSet.mem: invalid index";
    fast_bool (((fast_get t pos) lsr delta) land 1)

end

let fbs3 n =
  let module BitSet = BitSet3 in
  let s = BitSet.create width in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      BitSet.unset s (Array.unsafe_get clear_poss i);
      BitSet.set s (Array.unsafe_get set_poss i);
    done;
    for _b = 1 to 100 do
      for i = 0 to op_count-1 do
        let _bool : bool =
          BitSet.mem s (Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done

module BitSet4 = struct
  (* Internal type for bit data - a string, but with a new type *)
  type intern

  let bcreate : int -> intern = Obj.magic String.create
  external fast_get : intern -> int -> int = "%string_unsafe_get"
  external fast_set : intern -> int -> int -> unit = "%string_unsafe_set"
  external fast_bool : int -> bool = "%identity"
  let fast_blit : intern -> int -> intern -> int -> int -> unit = Obj.magic String.blit
  let fast_fill : intern -> int -> int -> int (* char *) -> unit = Obj.magic String.fill
  let fast_length : intern -> int= Obj.magic String.length

  type t = intern (* internal string of bits *)

  let int_size = 7 (* value used to round up index *)
  let log_int_size = 3 (* number of shifts *)

  let create n = (* n is in bits *)
    if n < 0 then invalid_arg "BitSet.create: negative size";
    let size = (n+int_size) lsr log_int_size in
    let b = bcreate size in
    fast_fill b 0 size 0;
    b

  let set t x =
    let pos = x lsr log_int_size and delta = x land int_size in
    fast_set t pos ((fast_get t pos) lor (1 lsl delta))

  let unset t x =
    let pos = x lsr log_int_size and delta = x land int_size in
    fast_set t pos ((fast_get t pos) land (lnot (1 lsl delta)))

  let mem t x =
    let pos = x lsr log_int_size and delta = x land int_size in
    fast_bool (((fast_get t pos) lsr delta) land 1)

end

let fbs4 n =
  let module BitSet = BitSet4 in
  let s = BitSet.create width in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      BitSet.unset s (Array.unsafe_get clear_poss i);
      BitSet.set s (Array.unsafe_get set_poss i);
    done;
    for _b = 1 to 100 do
      for i = 0 to op_count-1 do
        let _bool : bool =
          BitSet.mem s (Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done

let () =
  Bench.config.Bench.gc_between_tests <- true;
  Bench.bench_n ["batbitset", fbs;
                 "array", farr;
                 "bitset nolen", fbs2;
                 "bitset fixed", fbs3;
                 "bitset fixed unsafe", fbs4;
                 "array unsafe", farr;
                ]
  |> Bench.summarize ~alpha:0.05
