module type ExceptionLess = sig

end

module type ExceptionBased = sig

  exception Rejected
    (** Exception to be raised when an element has been rejected
	by the parser.*)


  type ('a, 'b) t = 'a Enum.t -> 'b
    (** The type of a parser from elements of type ['a] to
	elements of type ['b], raising [Rejected] if the
	contents of ['a] have been rejected.*)

  val singleton : 'a -> ('a, 'a) t
    (** Recognize exactly one element.         *)

  val exact_sequence  : 'a Enum.t -> ('a, unit) t
    (** Recognize an exact sequence of elements.*)

  val maybe           : ('a, 'b) t -> ('a, 'b option) t
    (** Recognize 0 or 1 occurrence of a parser.*)

  val bind  : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
    (** Sequential composition.*)

  val either : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    (** Accept two possibilities.*)

  val zero_or_more : ('a, 'b) t -> ('a, 'b list) t
    (** Accept 0+ parsers. *)

  val one_or_more  : ('a, 'b) t -> ('a, 'b list) t
    (** Accept 1+ parsers. *)

  val times        : ('a, 'b) t -> int -> ('a, 'b list) t
    (** Accept a parser repeated n times *)
end



