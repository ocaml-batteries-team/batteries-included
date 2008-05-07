module BaseComplex :
  sig
    type t = Complex.t = { re : float; im : float; }
    val zero : t
    val one : t
    val i : t
    val neg : t -> t
    val conj : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val inv : t -> t
    val div : t -> t -> t
    val sqrt : t -> t
    val norm2 : t -> float
    val norm : t -> float
    val arg : t -> float
    val polar : float -> float -> t
    val exp : t -> t
    val log : t -> t
    val pow : t -> t -> t
    val modulo : 'a -> 'b
    val to_string : t -> string
    val pred : t -> t
    val succ : t -> t
    val to_int : t -> int
    val of_int : int -> t
    val abs : t -> t
    val compare : 'a -> 'a -> int
    val of_string : string -> t
  end
module Complex :
  sig
    type t = BaseComplex.t
    val operations : t Number.numeric
    val zero : t
    val one : t
    val neg : t -> t
    val succ : t -> t
    val pred : t -> t
    val abs : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val modulo : t -> t -> t
    val pow : t -> t -> t
    val compare : t -> t -> int
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> t -> t
    val ( /. ) : t -> t -> t
    val ( ** ) : t -> t -> t
    val ( <>. ) : t -> t -> bool
    val ( >=. ) : t -> t -> bool
    val ( <=. ) : t -> t -> bool
    val ( >. ) : t -> t -> bool
    val ( <. ) : t -> t -> bool
    val ( =. ) : t -> t -> bool
  end
