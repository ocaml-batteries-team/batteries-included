type 'a t

external of_abstr : 'a Queue.t -> 'a t = "%identity"
external to_abstr : 'a t -> 'a Queue.t = "%identity"

val filter_inplace : ('a -> bool) -> 'a t -> unit
