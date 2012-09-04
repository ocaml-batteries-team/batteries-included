type dynarray = {
  mutable len : int;
  mutable array : int array;
  (* int array to have cheap Array.get,
     like in batDynArray *)
}

let len = 1000
let d = {
  len;
  array = Array.make len 42;
}

let unsafe_iter f d =
  for i = 0 to d.len - 1 do
    f d.array.(i)
  done

let unsafe_iter2 f d =
  let a = d.array in
  for i = 0 to d.len - 1 do
    f a.(i)
  done

let iter f d =
  let a = d.array in
  let len = d.len in
  for i = 0 to len - 1 do
    f a.(i);
    if d.array != a || d.len <> len then failwith "whatever"
  done

let iter2 f d =
  let a = d.array in
  let i = ref 0 in
  let len = d.len in
  while !i < d.len && !i < len do
    f a.(!i);
    incr i
  done

let iter3 f d =
  let i = ref 0 in
  while !i < d.len do
    f d.array.(!i);
    incr i
  done

let test iter n =
  for i = 0 to n - 1 do
    ignore i;
    iter ignore d
  done

let for_ n =
  for i = 0 to n - 1 do
    ignore i;
    for i = 0 to d.len - 1 do
      ignore d.array.(i)
    done
  done

let for2 n =
  for i = 0 to n - 1 do
    ignore i;
    let a = d.array in
    for i = 0 to d.len - 1 do
      ignore a.(i)
    done
  done

let () =
  let readings =
    Bench.bench_n [
      "unsafe_iter", test unsafe_iter;
      "unsafe_iter2", test unsafe_iter2;
      "iter", test iter;
      "iter2", test iter2;
      "iter3", test iter3;
      "for_", for_;
      "for2", for2;
    ] in
  Bench.summarize readings
