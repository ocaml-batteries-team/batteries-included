
let array_shuffle ?state a =
  let random_int = match state with
    | None -> Random.int
    | Some s -> Random.State.int s in
  for n = Array.length a - 1 downto 1 do
    let k = random_int (n + 1) in
    if k <> n then begin
      let buf = Array.unsafe_get a n in
      Array.unsafe_set a n (Array.unsafe_get a k);
      Array.unsafe_set a k buf
    end
  done

(*$Q
  Q.(array_of_size Gen.(2--15) small_int) (fun a -> \
    let a' = Array.copy a in \
    array_shuffle a'; \
    (Array.to_list a' |> List.sort Pervasives.compare) = \
     (Array.to_list a |> List.sort Pervasives.compare))
*)

(*$R
  let rec fact = function 0 -> 1 | n -> n * fact (n - 1) in
  let length = 5 in
  let test = Array.init length (fun i -> i) in (* all elements must be distinct *)
  let permut_number = fact length in
  let histogram = Hashtbl.create permut_number in
  for i = 1 to 50_000 do
    let a = Array.copy test in
    array_shuffle a;
    Hashtbl.replace histogram a ();
  done;
  assert_bool "all permutations occur" (Hashtbl.length histogram = permut_number)
*)
