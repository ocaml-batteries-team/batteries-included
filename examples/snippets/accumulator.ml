(*A problem I found some time ago on Paul Graham's website.

  "Revenge of the Nerds yielded a collection of canonical solutions to
  the same problem in a number of languages.

  The problem: Write a function foo that takes a number n and returns a
  function that takes a number i, and returns n incremented by i.

  Note: (a) that's number, not integer, (b) that's incremented by, not plus."

  Solutions in other languages are available at
  http://www.paulgraham.com/accgen.html
 *)


(** [adder t n] is an adder for elements of [numeric] typeclass [t],
    initialized with [n]*)
let adder t n i =
open Numeric in
  Ref.post r (t.add i)
  where r = ref n

(*Examples:*)
let adder_of_floats : float -> float =
  adder Float.operations 5.

let adder_of_ints   : int -> int =
  adder Int.operations 5

let adder_of_complexes: Complex.t -> Complex.t =
  adder Complex.operations Complex.i
