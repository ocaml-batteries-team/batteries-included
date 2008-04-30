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

val pre_incr : 'a ref -> 'a

val pre_decr : 'a ref -> 'a

val post_incr: 'a ref -> 'a

val post_decr: 'a ref -> 'a
