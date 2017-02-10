
let array_shuffle ?state:(s = Random.State.make_self_init ()) a =
  for n = Array.length a - 1 downto 1 do
    let k = Random.State.int s (n + 1) in
    if k <> n then
      let buf = Array.unsafe_get a n in
      Array.unsafe_set a n (Array.unsafe_get a k);
      Array.unsafe_set a k buf
  done;
  a
