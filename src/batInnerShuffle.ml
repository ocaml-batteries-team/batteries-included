
let array_shuffle maybe_rand_state a =
  let state_incr = match maybe_rand_state with
    | None -> Random.int
    | Some s -> Random.State.int s in
  for n = Array.length a - 1 downto 1 do
    let k = state_incr (n + 1) in
    if k <> n then
      let buf = Array.unsafe_get a n in
      Array.unsafe_set a n (Array.unsafe_get a k);
      Array.unsafe_set a k buf
  done;
  a
