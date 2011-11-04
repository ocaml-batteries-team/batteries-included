(*
 * Std - Additional functions
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** {6 Additional functions.}

    @author Nicolas Cannasse
    @author David Teller
    @author Zheng Li
*)

(**/**)
val input_lines : in_channel -> string BatEnum.t
(** Returns an enumeration over lines of an input channel, as read by the
 [input_line] function. *)

val input_chars : in_channel -> char BatEnum.t
(** Returns an enumeration over characters of an input channel. *)

val input_list : in_channel -> string list
(** Returns the list of lines read from an input channel. *)

val input_all : in_channel -> string
(** Return the whole contents of an input channel as a single
 string. *)
(**/**)

val print_bool : bool -> unit
(** Print a boolean to stdout. *)

val prerr_bool : bool -> unit
(** Print a boolean to stderr. *)

val input_file : ?bin:bool -> string -> string
(** returns the data of a given filename. *)

val output_file : filename:string -> text:string -> unit
(** creates a filename, write text into it and close it. *)

val string_of_char : char -> string
(** creates a string from a char. *)

external identity : 'a -> 'a = "%identity"
(** the identity function. *)

val unique : unit -> int
(** returns an unique identifier every time it is called.  This
    function is not thread-safe.  Use {!BatPervasives.unique}
    instead. *)

val dump : 'a -> string
(** represent a runtime value as a string. Since types are lost at compile
    time, the representation might not match your type. For example, None
    will be printed 0 since they share the same runtime representation. *)

val print : 'a -> unit
(** print the representation of a runtime value on stdout.
    See remarks for [dump]. *)

val finally : (unit -> unit) -> ('a -> 'b) -> 'a -> 'b 
  (** [finally fend f x] calls [f x] and then [fend()] even if [f x] raised
      an exception. *)

val with_dispose : dispose:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
(** [with_dispose dispose f x] invokes [f] on [x], calling [dispose x]
    when [f] terminates (either with a return value or an
    exception). *)

val forever : ('a -> 'b) -> 'a -> unit
(** [forever f x] invokes [f] on [x] repeatedly (until an exception occurs). *)

val ignore_exceptions : ('a -> 'b) -> 'a -> unit
(** [ignore_exceptions f x] invokes [f] on [x], ignoring both the returned value
    and the exceptions that may be raised. *)

val args : unit -> string BatEnum.t
(** An enumeration of the arguments passed to this program through the command line.

    [args ()] is given by the elements of [Sys.argv], minus the first element.*)

(**/**)
val invisible_args : int ref
(** The number of arguments which must never be returned by [args]

    Typically, [invisible_args] is [1], to drop the name of the executable. However,
    in some circumstances, it may be useful to pretend that some arguments need not
    be parsed.
*)
(**/**)

val exe  : string
  (** The name of the current executable.

      [exe] is given by the first argument of [Sys.argv]*)


(** {6 Operators}*)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** Function application. [x |> f] is equivalent to [f x]. 

    This operator is commonly used to write a function composition
    by order of evaluation means rather than by inverse order.
    For instance, [g (f x)] means "apply [f] to [x], then apply
    [g] to the result." In some circumstances, it may be more
    understandable to write this as [x |> f |> g], or
    "starting from [x], apply [f], then apply [g]."
    
    This operator may also be useful for composing sequences of
    function calls without too many parenthesis. *)

val ( <|  ) : ('a -> 'b) -> 'a -> 'b
  (** Function application. [f <| x] is equivalent to [f x]. 
      
      This operators may be useful for composing sequences of
      function calls without too many parenthesis.  *)

val ( |- ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Function composition. [f |- g] is [fun x -> g (f x)]. 
    This is also equivalent to applying [|>] twice.*)

val ( -| ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(** Function composition. [f -| g] is [fun x -> f (g x)]. Mathematically, this is
    operator o.*)

val flip : ( 'a -> 'b -> 'c ) -> 'b -> 'a -> 'c
  (** Argument flipping. 
      
      [flip f x y] is [f y x]. Don't abuse this function, it may shorten considerably
      your code but it also has the nasty habit of making it harder to read.*)
      

val ( *** ) : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
(** Function pairing.

    [f *** g] is [fun (x,y) -> (f x, g y)].*)

val ( &&& ) : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)
  (** Applying two functions to the same argument.

      [ f &&& g] is [fun x -> (f x, g x)]. *)

val first : ('a -> 'b * 'c) -> 'a -> 'b
(** Projection of a pair to its first element. *)

val second : ('a -> 'b * 'c) -> 'a -> 'c
(** Projection of a pair to its second element. *)



val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
  (** Turn a function that takes a pair into a function that takes its
      arguments one at a time. *)

val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
  (** Turn a function that takes its arguments one at a time into a
      function that takes a pair as argument. *)

val const : 'a -> (_ -> 'a)
  (** Ignore its second argument.

      Therefore, [const x] is the function which always returns [x].*)

val tap : ('a -> unit) -> 'a -> 'a
  (** 
      @since v1.1.0

      Apply the function to the given value, and return the original value as the result.  Useful as part of a pipeline of operations.  
  *)

(**
   {6 Results}
*)

type ('a, 'b) result =
  | Ok  of 'a
  | Bad of 'b
(** The result of a computation - either an [Ok] with the normal
result or a [Bad] with some value (often an exception) containing
failure information*)

val ignore_ok : ('a, exn) result -> unit
(** [ignore_ok (f x)] ignores the result of [f x] if it's ok, but
throws the exception contained if [Bad] is returned. *)

val ok : ('a, exn) result -> 'a
(** [f x |> ok] unwraps the [Ok] result of [f x] and returns it, or
throws the exception contained if [Bad] is returned. *)

val wrap : ('a -> 'b) -> 'a -> ('b, exn) result
(** [wrap f x] wraps a function that would normally throw an exception
on failure such that it now returns a result with either the [Ok]
return value or the [Bad] exception. *)

(**
   {6 Thread-safety internals}

   Unless you are attempting to adapt Batteries Included to a new model of
   concurrency, you probably won't need this.
*)

val lock: BatConcurrent.lock ref
(**
   A lock used to synchronize internal operations.

   By default, this is {!BatConcurrent.nolock}. However, if you're
   using a version of Batteries compiled in threaded mode, this uses
   {!BatMutex}. If you're attempting to use Batteries with another
   concurrency model, set the lock appropriately.
*)
