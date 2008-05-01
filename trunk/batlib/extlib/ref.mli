val pre : 'a ref -> ( 'a -> 'a ) -> 'a
  (** Perform an operation on a reference and return the
      previous value of that reference. 

      For instance, if [x] is a reference to [1],
      [pre x ( ( + ) 1) ] returns [1] and sets [x] to [2].*)

val post: 'a ref -> ('a -> 'a) -> 'a
  (** Perform an operation on a reference and return the
      new value of that reference. 

      For instance, if [x] is a reference to [1],
      [pre x ( ( + ) 1)] returns [2] and sets [x] to [2].*)

val pre_incr : int ref -> int

val pre_decr : int ref -> int

val post_incr: int ref -> int

val post_decr: int ref -> int
