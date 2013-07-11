open BatLazyList

(* append *)

let test_append append n =
  for _i = 1 to n do
    iter ignore (BatList.fold_left append nil
                   (BatList.init 50 (fun len -> init len (fun i -> i))));
  done

let append_inlined l1 l2 =
  let rec aux list =  match next list with
    | Cons (x, (t : 'a t)) -> Cons (x, lazy (aux t))
    | _                    -> Lazy.force l2
  in lazy (aux l1)

let append_folding l1 l2 =
   lazy_fold_right (fun x xs -> Cons (x, xs)) l1 l2


(* concat *)

let test_concat concat n =
  for _i = 1 to n do
    iter ignore (concat (init 100 (fun len -> init len (fun j -> j))))
  done

let concat_inlined  (lol : ('a t) t) =
  let rec aux list = match next list with
    | Cons (li, t) -> Lazy.force (append li (lazy (aux t)))
    | Nil -> Nil
  in lazy (aux lol)

let concat_folding lol =
  lazy_fold_right (fun li rest -> Lazy.force (append li rest)) lol nil


(* exists *)

let test_exists exists n =
  let len = 10_000 in
  for _i = 1 to n do
    assert (exists (fun i -> i > len / 2) (init len (fun i -> i)));
  done

let exists_inlined f l =
  let rec aux rest = match next rest with
    | Cons (x, _) when f x -> true
    | Cons (_, t)          -> aux t
    | Nil                  -> false
  in aux l

let exists_folding p l =
  let test x rest = p x || Lazy.force rest in
  Lazy.force (lazy_fold_right test l (Lazy.lazy_from_val false))



let () =
  let append_benchs = Bench.bench_n [
    "append inlined", test_append append_inlined;
    "append folding", test_append append_folding;
  ] in
  let concat_benchs = Bench.bench_n [
    "concat inlined", test_concat concat_inlined;
    "concat folding", test_concat concat_folding;
  ] in
  let exists_benchs = Bench.bench_n [
    "exists inlined", test_exists exists_inlined;
    "exists folding", test_exists exists_folding;
  ] in
  List.iter Bench.summarize [ append_benchs; concat_benchs; exists_benchs ]

(* some approximate results:
  append inlined (2.82 ms) is 10.2% faster than
  append folding (3.14 ms)
  concat folding (1.38 ms) is probably (alpha=47.71%) same speed as
  concat inlined (1.39 ms)
  exists inlined (546.18 us) is 53.5% faster than
  exists folding (1.18 ms)
*)









