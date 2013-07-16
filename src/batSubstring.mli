(*TODO: What is this module? Is it meant for public use?*)

type t
(**
   [Substring.t] is the type of substrings of a basestring, an efficient
   representation of a piece of a string.

   A substring (s,i,n) is valid if 0 <= i <= i+n <= size s,
                  or equivalently, 0 <= i and 0 <= n and i+n <= size s.

   A valid substring (s, i, n) represents the string s[i...i+n-1].

   Invariant in the implementation: Any value of type [Substring.t] is valid.
*)

val empty : unit -> t

val to_string : t -> string
(**    [string sus] is the string s[i..i+n-1] represented by sus = (s, i, n). *)

val of_string : string -> t

val make : int -> char -> t

val create : int -> t

val equal : t -> t -> bool
(** Substring equality

    @since 2.1
*)

val of_input : BatIO.input -> t

val substring : string -> int -> int -> t
(** [substring s o l] returns a substring with base-string [s], offset
    [o] and length [l].  Arguments are checked for validity

    [substring s i n] creates the substring [(s, i, n)], consisting
    of the substring of s with length n starting at i.
    @raise Inavlid_argument if [i<0] or [n<0] or [i+n > size s].  Equivalent to
    [extract s i (Some n)].  *)

val unsafe_substring : string -> int -> int -> t
(** [unsafe_substring] behaves like [substring], but does not perform
    any sanity check on the position and length. *)

val extract : string -> int -> int option -> t
(** [extract s i None] creates the substring (s, i, size s-i)
    consisting of the tail of s starting at i.
    @raise Invalid_argument if [i<0] or [i > size s].

    [extract s i (Some n)] creates the substring (s, i, n), consisting
    of the substring of s with length n starting at i.
    @raise Invalid_argument if [i<0] or [n<0] or [i+n > size s].
*)

val all : string -> t
(** [all s] is the substring [(s, 0, size s)]. *)

val base : t -> string * int * int
(** [base sus] is the concrete triple [(s, i, n)], where [psus = (s, i,
    n)]. *)

val is_empty : t -> bool
(** [isEmpty (s, i, n)] true if the substring is empty (that is,
    [n = 0]). *)

val getc : t -> (char * t) option
(** [getc sus] returns [Some(c, rst)] where [c] is the first character and
    [rst] the remainder of [sus], if [sus] is non-empty; otherwise returns
    [None]. *)

val first : t -> char option
(** [first sus] returns [Some c] where [c] is the first character in
    [sus], if [sus] is non-empty; otherwise returns [None].  *)

val triml : int -> t -> t
(** [triml k sus] returns sus less its leftmost k characters; or the
    empty string at the end of sus if it has less than k characters.
    @raise Invalid_argument  if [k < 0], even in the partial application
    [triml k].
*)

val trimr : int -> t -> t
(** [trimr k sus] returns sus less its rightmost k characters; or the
    empty string at the beginning of sus if it has less than k
    characters.  @raise Invalid_argument if [k < 0], even in the partial
    application [trimr k].
*)

val get : t -> int -> char
(** [sub sus k] returns the k'th character of the substring; that
    is, s(i+k) where sus = (s, i, n).  @raise Invalid_argument if
    [k<0] or [k>=n].  *)

val size : t -> int
(** [size (s, i, n)] returns the size of the substring, that is, [n].
*)
val length: t -> int
(** Equivalent to {!size}. *)

val slice : t -> int -> int option -> t
(** [slice sus i' None] returns the substring [(s, i+i', n-i')],
    where [sus = (s, i, n)].
    @raise Invalid_argument if [i' < 0] or [i' > n].

    [slice sus i' (Some n')] returns the substring [(s, i+i', n')],
    where [sus] = [(s, i, n)].  @raise Invalid_argument if [i' < 0]
    or [n' < 0] or [i'+n' >= n].
*)
val concat : t list -> string
(** [concat suss] returns a string consisting of the concatenation of
    the substrings.  Equivalent to [String.concat (List.map to_string
    suss)].
*)
val explode : t -> char list
(** [explode sus] returns the list of characters of sus, that is,
    [s(i), s(i+1), ..., s(i+n-1)] where [sus = (s, i, n)].  Equivalent
    to [String.explode (to_string ss)].
*)
val is_prefix : string -> t -> bool
(** [is_prefix s1 s2] is true if [s1] is a prefix of [s2]. That is, if
    there exists a string [t] such that string [s1 ^ t = to_string s2].
*)

val compare : t -> t -> int
(** [compare sus1 sus2] performs lexicographic comparison, using the
    standard ordering Char.compare on the characters.p Equivalent to,
    but more efficient than, [String.compare (to_string sus1)
    (to_string sus2)].  *)

(* NOT IMPLEMENTED
   [collate cmp (sus1, sus2)] performs lexicographic comparison, using the
   given ordering cmp on characters.  Equivalent to, but more efficient
   than, String.collate cmp (string sus1, string sus2).
*)

val index : t -> char -> int
(** [index sus c] returns the index of the first occurence of [c] in [sus] or
    @raise Not_found otherwise. *)

val index_from : t -> int -> char -> int
(** [index_from sus i c] returns the index of the first occurence of [c] in
    [sus] after the index [i] or @raise Not_found otherwise. If [i] is beyond
    the range of [sus], @raise Invalid_argument. It is equivalent to [i + index (triml i sus) c]. *)

val rindex : t -> char -> int
(** [rindex sus c] returns the index of the last occurence of [c] in [sus] or
    @raise Not_found otherwise. *)

val rindex_from : t -> int -> char -> int
(** [index_from sus i c] returns the index of the last occurence of [c] in [sus]
    before the index [i] or @raise Not_found otherwise. If [i] is beyond the
    range of [sus], @raise Invalid_argument. It is equivalent to [rindex (trimr i sus) c]. *)

val contains : t -> char -> bool
(** [contains s c] tests if character [c] appears in the substring [s].

    @since 2.1
*)

val dropl : (char -> bool) -> t -> t
(** [dropl p sus] drops the longest prefix (left substring) of [sus]
    all of whose characters satisfy predicate [p].  If all
    characters do, it returns the empty substring [(s, i+n, 0)]
    where [sus = (s, i, n)].  *)

val dropr : (char -> bool) -> t -> t
(**
   [dropr p sus] drops the longest suffix (right substring) of sus all
   of whose characters satisfy predicate [p].  If all characters do, it
   returns the empty substring [(s, i, 0)] where [sus = (s, i, n)].
*)

val takel : (char -> bool) -> t -> t
(**
   [takel p sus] returns the longest prefix (left substring) of [sus]
   all of whose characters satisfy predicate [p].  That is, if the
   left-most character does not satisfy p, returns the empty [(s, i, 0)]
   where [sus = (s, i, n)].
*)

val taker : (char -> bool) -> t -> t
(**
   [taker p sus] returns the longest suffix (right substring) of [sus]
   all of whose characters satisfy predicate [p].  That is, if the
   right-most character satisfies [p], returns the empty [(s, i+n, 0)]
   where [sus = (s, i, n)].

   Let [p] be a predicate and xxxxfyyyyfzzzz a string where all
   characters in xxxx and zzzz satisfy [p], and f a character not
   satisfying [p].  Then

   sus = xxxxfyyyyfzzzz         sus = xxxxzzzz
   ------------------------------------------------------
   dropl p sus =     fyyyyfzzzz
   dropr p sus = xxxxfyyyyf
   takel p sus = xxxx                         xxxxzzzz
   taker p sus =           zzzz               xxxxzzzz

   It also holds that
   [concat (takel p sus) (dropl p sus) = string sus]
   [concat (dropr p sus) (taker p sus) = string sus]
*)

val splitl : (char -> bool) -> t -> t * t
(** [splitl p sus] splits [sus] into a pair [(sus1, sus2)] of
    substrings where [sus1] is the longest prefix (left substring) all
    of whose characters satisfy [p], and [sus2] is the rest.  That is,
    [sus2] begins with the leftmost character not satisfying [p].
    Disregarding sideeffects, we have: [splitl p sus = (takel p sus,
    dropl p sus)].
*)

val splitr : (char -> bool) -> t -> t * t
(** [splitr p sus] splits [sus] into a pair [(sus1, sus2)] of
    substrings where [sus2] is the longest suffix (right substring) all
    of whose characters satisfy [p], and [sus1] is the rest.  That is,
    [sus1] ends with the rightmost character not satisfying [p].
    Disregarding sideeffects, we have: [splitr p sus = (dropr p sus,
    taker p sus)]
*)

val split_at : int -> t -> t * t
(** [split_at sus k] returns the pair [(sus1, sus2)] of substrings,
    where [sus1] contains the first [k] characters of [sus], and
    [sus2] contains the rest.  @raise Invalid_argument if [k < 0] or
    [k > size sus].
*)

(* NOT IMPLEMENTED
   [position s (s',i,n)] splits the substring into a pair (pref, suff)
   of substrings, where suff is the longest suffix of (s', i, n) which
   has s as a prefix.  More precisely, let m = size s.  If there is a
   least index k in i..i+n-m for which s = s'[k..k+m-1],
   then the result is       pref = (s', i, k-i) and suff = (s', k, n-(k-i));
   otherwise the result is  pref = (s', i, n)   and suff = (s', i+n, 0).
*)

val span : t -> t -> t
(** [span sus1 sus2] returns a substring spanning from the start of
    [sus1] to the end of [sus2], provided this is well-defined: [sus1]
    and [sus2] must have the same underlying string, and the start of
    [sus1] must not be to the right of the end of [sus2]; otherwise
    @raise Invalid_argument.

    More precisely, if [base sus1 = (s,i,n)] and [base sus2 =
    (s',i',n')] and [s = s'] and [i <= i'+n'], then [base (span sus1
    sus2) = (s, i, i'+n'-i)].  This may be used to compute [span],
    [union], and [intersection].
*)

val translate : (char -> char) -> t -> string
(** [translate f sus] applies [f] to every character of [sus], from
    left to right, and returns the concatenation of the results.
    Equivalent to [String.of_list (List.map f (explode sus))].
*)

val tokens : (char -> bool) -> t -> t list
(** [tokens p sus] returns the list of tokens in [sus], from left to
    right, where a token is a non-empty maximal substring of [sus] not
    containing any delimiter, and a delimiter is a character satisfying
    [p].
*)

val fields : (char -> bool) -> t -> t list
(**
   [fields p sus] returns the list of fields in [sus], from left to right,
   where a field is a (possibly empty) maximal substring of [sus] not
   containing any delimiter, and a delimiter is a character satisfying [p].

   Two tokens may be separated by more than one delimiter, whereas two
   fields are separated by exactly one delimiter.  If the only delimiter
   is the character ['|'], then
   "abc||def" contains two tokens:   "abc" and "def"
   "abc||def" contains three fields: "abc" and "" and "def"
*)

val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a
(** [foldl f e sus] folds [f] over [sus] from left to right.  That is,
    evaluates [f s.[i+n-1] (f ... (f s.[i+1] (f s.[i] e)) ...)]
    tail-recursively, where [sus = (s, i, n)].  Equivalent to
    [List.fold_left f e (explode sus)].  *)

val fold_right : (char -> 'a -> 'a) -> t -> 'a -> 'a
(** [foldr f e sus] folds [f] over [sus] from right to left.  That is,
    evaluates [f s.[i] (f s.[i+1] (f ... (f s.[i+n-1] e) ...))]
    tail-recursively, where [sus = (s, i, n)].  Equivalent to
    [List.fold_right f e (explode sus)].
*)

val iter : (char -> unit) -> t -> unit
(** [iter f sus] applies [f] to all characters of [sus], from left to
    right.  Equivalent to [List.iter f (explode sus)].
*)

val iteri : (int -> char -> unit) -> t -> unit
(** Same as {!iter}, but the
    function is applied to the index of the element as first argument
    (counting from 0), and the character itself as second argument.

    @since 2.1
*)

val trim : t -> t
(** removes whitespace from left and right ends of input *)

val split_on_char : char -> t -> t list
(** [split_on_char c ss] returns substrings of input [ss] as divided
    by [c] *)

val split_on_pipe : t -> t list
val split_on_dot : t -> t list
val split_on_comma : t -> t list
val split_on_slash : t -> t list

val enum : t -> char BatEnum.t
(** [enum ss] returns an enumeration of the characters represented by ss. 
    It does no copying so beweare of mutating the original string.

    @since 2.1
*)

val print : 'a BatIO.output -> t -> unit
  (** [print oc ss] prints [ss] to the output channel [oc] *)
