let array_shuffle rand_state a =
  (match rand_state with
   | Some s -> Random.set_state s
   | None -> ());
  for n = Array.length a - 1 downto 1 do
    let k = Random.int (n + 1) in
    if k <> n then
      let buf = Array.unsafe_get a n in
      Array.unsafe_set a n (Array.unsafe_get a k);
      Array.unsafe_set a k buf
  done;
  a

let list_shuffle rand_state l =
  let a = array_shuffle rand_state (Array.of_list l) in
  Array.to_list a
