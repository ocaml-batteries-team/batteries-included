module Queue1 = (* not really amortized version *)
struct
  type 'a t = {
    len_front : int;
    front : 'a list; (* front = [] => rear = [] *)
    len_rear : int;
    rear : 'a list;
  }

  let empty = {
    len_front = 0;
    front = [];
    len_rear = 0;
    rear = []
  }

  let snoc t x =
    match t with
    | {front = []; rear = rear} ->
      assert (rear = []);
      {len_front = 1; front = [x]; len_rear = 0; rear = []}
    | {rear = rear; len_rear = len_rear} ->
      {t with rear = x :: rear; len_rear = len_rear + 1}

  let front t =
    match t with
    | {front = []; rear = rear} ->
      assert (rear = []);
      None
    | {front = [hd]; rear = rear; len_rear = len_rear} ->
      Some (hd, {len_front = len_rear; front = List.rev rear; rear = []; len_rear = 0})
    | {front = hd :: tl; len_front = len_front} ->
      Some (hd, {t with front = tl; len_front = len_front - 1})

end

type 'a lazy_list = 'a lazy_cell Lazy.t
and 'a lazy_cell =
  | Nil
  | Cons of 'a * 'a lazy_list
let nil = Lazy.lazy_from_val Nil
let rec append x y =
  lazy (
    match x with
    | lazy Nil -> Lazy.force y
    | lazy (Cons (hd, tl)) -> Cons (hd, append tl y)
  )
let rev x =
  let rec rev_append x acc =
    match x with
    | lazy Nil -> acc
    | lazy (Cons (hd, tl)) -> rev_append tl (Lazy.lazy_from_val (Cons (hd, acc))) in
  rev_append x nil

module Queue2 = (* really amortized version *)
struct
  type 'a t = {
    len_front : int;
    front : 'a lazy_list;
    len_rear : int; (* len_front >= len_rear *)
    rear : 'a lazy_list;
  }

  let empty = {
    len_front = 0;
    front = nil;
    len_rear = 0;
    rear = nil;
  }
  let snoc ({len_front = len_front; front = front; len_rear = len_rear; rear = rear} as t) x =
    if len_front >= len_rear + 1 then {
      t with
	rear = Lazy.lazy_from_val (Cons (x, rear));
	len_rear = len_rear + 1;
    }
    else {
      front = append front (rev (Lazy.lazy_from_val (Cons (x, rear))));
      len_front = len_front + len_rear + 1;
      rear = nil;
      len_rear = 0;
    }

  let front ({len_front = len_front; front = front; len_rear = len_rear; rear = rear} as t) =
    if len_front - 1 >= len_rear then (
      match front with
      | lazy Nil -> assert false
      | lazy (Cons (hd, tl)) ->
	Some (hd, {t with
	  len_front = len_front - 1;
	  front = tl;
	})
    ) else
      match front with
      | lazy Nil -> None
      | lazy (Cons (hd, tl)) ->
	  Some (hd, {
	    len_front = len_front - 1 + len_rear;
	    front = append tl (rev rear);
	    rear = nil;
	    len_rear = 0;
	  })
end

module type Queue = sig
  type 'a t
  val empty : 'a t
  val snoc : 'a t -> 'a -> 'a t
  val front : 'a t -> ('a * 'a t) option
end

let test q grow_size =
  let module Q = (val q:Queue) in
  fun n ->
    for i = 0 to n do
      let rec loop q = function
	| 0 -> q
	| j -> loop (Q.snoc q j) (j - 1) in
      let q = loop Q.empty grow_size in
      let rec loop q =
	match Q.front q with
	  | None -> ()
	  | Some (_, q) -> loop q in
      loop q
    done

let test_quadratic grow_size =
  let rec loop q = function
    | 0 -> q
    | j -> loop (BatDeque.cons j q) (j - 1) in
  let q = loop BatDeque.empty grow_size in
  let rec loop q =
    match BatDeque.rear q with
    | None -> ()
    | Some (q, _) ->
      match BatDeque.front q with
      | None -> ()
      | Some (_, q) -> loop q in
  loop q

let () =
  let readings =
    Bench.bench_n [
      "Not-really amortized Deque", test (module Queue1: Queue) 100;
      "Really amortized Deque", test (module Queue2: Queue) 100;
    ] in
  print_endline "Time to grow and deconstruct at the opposite end a deque of 10K elements";
  Bench.summarize readings;
  let sizes = [10; 20; 50; 100; 200; 400; 1000] in
  let readings = Bench.bench_throughput test_quadratic sizes in
  print_endline "Time (per element) to grow and then deconstruct a BatDeque at alternating ends by queue length";
  Bench.summarize readings
