(*
 * Path - Path and directory manipulation
 * Copyright (C) 2008 Dawid Toton
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

(** Filepath handling.



Paths can be used with different string implementations:
 - see {!Path.OfRope} to use paths built of validated UTF-8 strings ({!Rope.t})
 - see {!Path.OfString} to use paths based on primitive [string] type. Actual strings may use UTF-8 encoding.

    @author Dawid Toton
*)
 

(**
   {6 Functorized interface}
*)

(** This signature lists few basic operations provided by all string types. *)
module type StringType = sig

  (** 'Bytes' here may be replaced by anything as long as it is consitent in all functions taking/returning [int] (except [compare], of course). *)
 
  type t
  (** Type for strings. *)
  
  val length_bytes : t -> int
  (** Length in bytes *)
  
  val lift : string -> t
  (** Convert from string of primitive [string] type. *)
  
  val concat_with_separators : t -> t list -> t
  (** [concat_with_separators sep lst] catenates all {i n} elements of [lst] inserting {i (n-1)} copies of [sep] in between. *)
  
  val compare : t -> t -> int
  (** Usual comparison function. *)
  
  val sub : t -> int -> int -> t
  (** As {!String.sub}, byte-indexed *)
  
  module Regexp : sig
    type rt    
    (** As {!type:Str.regexp} *)

    val make : string -> rt (* From primitive string! *)
    (** As {!val:Str.regexp} *)

    val for_string : t -> rt 
    (** As {!Str.regexp_string} *)

    val full_match  : rt -> t -> bool 
    (** [full_match rx word] returns true if [word] belongs to the regular language of [rt]. *)
    
    val split_delim : rt -> t -> t list
    (** As {!Str.split_delim} *)

    val search_backward : rt -> t -> int -> int
    (** As {!Str.search_backward}, byte-indexed *)
  end
end

(** All implementations of [Path] functionality have this module type. *)
module type PathType = sig

type ustring
(** Type of strings used. In case of {!Path.OfRope} it is {!Rope.t} and in {!Path.OfString} module it is  [string].
 *)

(** Convenience operator for lifting primitive strings to [ustring] type. *)
module OperatorLift : sig

 val (!!) : string -> ustring 
 (** Prefix operator that converts primitive string to [ustring]. May raise some exceptions depending on actual strings implementation. 
 
  You might want to [open Path.OperatorLift] to improve readability of path construction using string literals. Example:
[Path.root/:!!"foo"/:!!"bar"] = [Path.root/:(S.lift "foo")/:(S.lift "bar")] (where [S.lift] converts to  [ustring] type) 
 *)

end

type t = ustring list
(** A type for storing paths. It is reversed list of names. In case of absolute path, the last element of the list is empty string ({e Windows:} empty or letter-colon; details below). Empty list represents empty relative path.

  Examples: [\["a";"b";"c"\]] is c/b/a (relative path); [\["d";"e";""\]] stays for /e/d (absolute path).

  All examples here and below are given for [ustring]=[string] case for clarity. To have the code working with other string types, one should prepend the [!!] operator ({!OperatorLift.(!!)}) to all string literals.
  
  There are two infix operators provided to allow to write expressions in natural order. For example, to build a path using {!PathType.Operators.(/:)} one can write:
  
  [base_dir/:"bar"] instead of ["bar"::base_dir]
  
  However it may be sometimes inevitable to write components in reverse, for example:
  
  [let whose_readme = function "README"::app::"doc"::"share"::_ -> Some app | _ -> None]
  
{e Windows:} Windows absolute paths start with "\\" or with drive letter. Use following representation:
 - [Path.root/:"."/:"pipe" = \["pipe";".";""\]]  for "\\.\pipe"
 - [\["C:"\]/:"foo" = \["foo";"C:"\]] for "C:\foo"
 
 In principle the first type of paths has broader range of allowed characters, but this implementation applies more strict rules to both ({!default_validator}). 
*)
(* If we wanted more safety, we'd have (making usage inconvenient):
type t = private P of ustring list 
*)

val is_relative : t -> bool
val is_absolute : t -> bool

(** {6 Construction} *)

val root : t
(** Root of the filesystem ([\[""\]]). It is minimal absolute path. Below it is called 'empty'. However it yields "/" or "\\" when converted to a string. 

{e Windows:} This path (root and nothing more) is meaningless, but for simplicity it is considered valid here. To create absolute path starting with drive letter, construct the list explicitly (as in [\["C:"\]/:"foo"]).
A path consisting of drive letter only is also called 'empty' here. 
*)
(* ocamldoc problem: try to get double_quot-backslash-double_quot in a docstring! *)

val append : t -> ustring -> t
(** Alternative name for {!Operators.(/:)} *)

val concat : t -> t -> t
(** Alternative name for {!Operators.(//@)} *)

module Operators : sig

 (** Infix operators for path construction. They are in separate module, so one can [open Path.Operators] to use them.
  *)

 val (/:) : t -> ustring -> t
 (** [path/:name] is a path of [name] located in a directory [path]. For example: 
 - {!PathType.root}[/:"var"/:"log"] builds absolute path "/var/log"
 - [\[user\]/:".ssh"] can be either:
 {ul {- absolute path "/.ssh" in case [user] is an empty string}
     {- relative path otherwise}}    
     
 {!PathType.default_validator} is applied to the argument. [name] must not contain path separator (causes Illegal_char exception).   
@raise Illegal_char (raised by validator on any bad character)
 *)
  
 val (//@) : t -> t -> t
(** [basepath//\@relpath] catenates two paths. 

{e Windows:} As a special exception it is possible to pass absolute path as [relpath], provided that [basepath] is simple absolute path (i.e. of the form [\[...; ""\]]) and [relpath] is not simple absolute path.

@raise Invalid_argument if the second argument is an absolute path ({e Windows:} see above). *)

end

exception Malformed_path

val normalize : t -> t
(**
  Consumes single dots and applies double dots where possible, e.g.:
  
  [normalize (\[".."\]/:"foo"/:"."/:"bar"/:"sub1"/:".."/:"sub2") = \[".."\]/:"foo"/:"bar"/:"sub2"]

{e Windows:} If single dot is next to root, it is preserved.    
@raise Malformed_path when absolute path is given that contains double dots that would be applied to the root.  
*)

val parent : t -> t
(** Returns parent path, i.e. immediate ancestor: [parent (foo/:bar) = foo]
@raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given
*)

val relative_to_any : t -> t -> t
(** [relative_to_any base sub] returns relative path [rel] such that 
[normalize (base/:rel) = normalize sub], i.e. common base is stripped and ".." are added if necessary.
Both arguments must be absolute paths or both relative. 

This function normalizes [base] and [sub] before calculation of the relative path.

{e Windows:} If [base] and [sub] are absolute, they must have the same root element: have the same drive letter or both starting with {!root} (i.e. [""] is the last element of the list). 
Exceptionally it is possible to get an absolute path as a result if drive letter is in [sub] but not as a root element (e .g. [base = root/:"bar"] and [sub = root/:bar//@(\["C:"\]/:"foo"]).

@see 'relative_to_parent' may be sometimes more suitable
@raise Invalid_argument if exactly one of given arguments is an absolute path
@raise Malformed_path if normalization fails (see {!PathType.normalize})
*)

exception Not_parent

val relative_to_parent : t -> t -> t
(** [relative_to_parent parent sub] returns relative path [rel] such that 
[(normalize parent)/:rel = normalize sub]. It is checked if [parent] is really a parent of [sub].
Both arguments must be absolute paths or both relative.

This function normalizes [base] and [sub] before calculation of the relative path.

{e Windows:} Exceptionally it is possible to get an absolute path as a result if drive letter is in [sub] but not as a root element (e .g. [base = root/:"bar"] and [sub = root/:bar//@(\["C:"\]/:"foo")]).

@raise Not_parent if [sub] is not descendant of [parent]
@raise Invalid_argument if exactly one of given arguments is absolute path
@raise Malformed_path if normalization fails (see {!PathType.normalize})
*)

(** {6 Validation} *)

exception Illegal_char
(** Raised by {!PathType.of_string}, {!PathType.append} and {!PathType.Operators.(/:)} when used validator finds illegal character. *)

type validator = ustring -> bool
(**
Validators should check if all characters of given string can be used in a name (path component). Return true if the name is valid. Return false if illegal character is found.

If a name should be rejected for some other reason, user defined validator may raise an exception.
*)
 
val default_validator : validator ref
(**
  Forward slash and code zero are considered invalid.
  
{e Windows:} Invalid characters are *?:\/<> and all with code <32. Exception: the function {!PathType.of_string} doesn't use validator against drive letter with colon. 
*)
(*TODO: Windows:
 On reserved names and ones ending with dot (except "." and "..") Illegal_name is raised.
*)

(** {6 Conversions} *)
   
val to_string : t -> ustring 
(** Convert to string. Empty relative path is converted to "." (single dot).

 {e Windows:} backslash is used as a separator and double backslash for root. If the path is only a drive letter (empty absolute path) trailing backslash is added (e.g. [to_string \["C:"\] = "C:\"]).
 
 "*)(* Dangling quote character because of ocamldoc lexer being apparently incompatible with OCaml. *)

val of_string : ustring -> t
(** Parse path in a given string. Any number of consecutive separators collapse ("a//b" becomes "a/b"). [Path.default_validator] is applied to each resulting name.

{e Windows:} both slashes '\' and '/' are accepted as separators. Paths of the 'semi-relative' form "C:foo\bar" are not recognized. For example "C:" string is parsed as [\["C:"\]] which has different meaning (see {!to_string}).
@raise Illegal_char when a character not allowed in paths is found.
*) 

(** {7 Convenience aliases} *)

val s : t -> ustring
(** = {!to_string} *)

val p : ustring -> t
(** = {!of_string} *)

(** {6 Name related functions} *)

val name : t -> ustring
(** Returns name of the object the pathname points to, i.e.
[name (foo/:bar) = bar]
@raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given
*)

val map_name : (ustring -> ustring) -> t -> t
(** [map_name fu path] returns [path] with the name replaced by [fu (]{!PathType.name}[ path)].

Example: [map_name (fun nn -> nn ^ ".backup") (["foo"]/:"bar") = ["foo"]/:"bar.backup"]

{!PathType.default_validator} is applied to new name.  
@raise Illegal_char (raised by validator if any bad character is found)
*)

val ext : t -> ustring option
(** Returns extension of the name of the object the pathname points to. Examples:

[ext ["aa.bb"] = Some "bb"]

[ext ["aa."] = Some ""]

[ext ["aa"] = None]

[ext [".hidden"] = Some "hidden"] {e (!)}

Extension begins where the rightmost dot in the name is found. If the name ends with a dot, the extension is empty and [Some ""] is returned. If there is no extension (no dot) the function returns [None].
@raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given
*)

val map_ext : (ustring option -> ustring option) -> t -> t
(** [map_ext fu path] returns [path] but with the name with extension given by [fu (]{!PathType.ext}[ path)]. If [fu] returns [Some _], the original extension may be replaced (when [Some ext] is passed to [fu]) or new added (when [fu] gets [None]). In case [fu] returns [None], the extension is removed (if exists). 

Example: [map_ext (function Some _ | None -> Some "png") (["foo"]/:"bar.jpeg") = ["foo"]/:"bar.png"]  

{!PathType.default_validator} is applied to the resulting name.  

{e Windows:} If [fu] returns [Some ""] (to make a name with trailing period) [map_ext] returns a path that shouldn't be passed to the operating system (is invalid).

@raise Illegal_char (raised by validator if any bad character is found)
*)

end

module Make : functor (S : StringType) -> PathType with type ustring = S.t
(** Constructs path handling module for string-like type and its operations given in [S].*)

module OfString : PathType with type ustring = string
(** This implementation can be used with UTF-8, but encoding of used strings is not verified.*)

(*
module OfRope : PathType with type ustring = Rope.t
(** In this implementation used strings are always valid UTF-8. *)
*)
