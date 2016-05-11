(* Explanation from OCaml 4.02 source:

   A queue is a reference to either nothing or some cell of a cyclic
   list. By convention, that cell is to be viewed as the last cell in
   the queue. The first cell in the queue is then found in constant
   time: it is the next cell in the cyclic list. The queue's length is
   also recorded, so as to make [length] a constant-time operation.

   The [tail] field should really be of type ['a cell option], but
   then it would be [None] when [length] is 0 and [Some] otherwise,
   leading to redundant memory allocation and accesses. We avoid this
   overhead by filling [tail] with a dummy value when [length] is 0.
   Of course, this requires bending the type system's arm slightly,
   because it does not have dependent sums.
   The dummy value used by the stdlib is (Obj.magic None). *)

type 'a cell = {
  content: 'a;
  mutable next: 'a cell
}
and 'a t = {
  mutable length: int;
  mutable tail: 'a cell
}

external of_abstr : 'a Queue.t -> 'a t = "%identity"
external to_abstr : 'a t -> 'a Queue.t = "%identity"

let filter_inplace f ({tail} as queue) =
  if not (Queue.is_empty (to_abstr queue)) then
    let rec filter'
        ({ next = { content; next} as current } as prev)
      =
      if f content
      then
        (* Keep cell. Recursion to next cell unless we reached the tail *)
        (if current != tail then filter' current)
      else begin
        (* Remove cell. *)
        if current != tail
        then begin
          (* Easy case. We are not removing the tail cell. *)
          prev.next <- next;
          queue.length <- queue.length - 1;
          (* Recursion with the same cell,
           * because it is now pointing beyond current. *)
          filter' prev
        end
        else begin
          (* Removing the tail cell *)
          if prev == current
          (* Tail cell is the last cell. Just clear the queue. *)
          then begin
            Queue.clear (to_abstr queue)
          end
          else begin
            (* Tail cell is not the last cell.
             * prev is the new tail. *)
            prev.next <- next;
            queue.length <- queue.length - 1;
            queue.tail <- prev;
          end
        end
      end
    in
    filter' tail
