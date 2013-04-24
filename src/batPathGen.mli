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

(* the part copied to PathGen.ml starts here *)

(** This signature lists few basic operations provided by all string types. *)
module type StringType = sig

  (** The actual implementation may use any (coherent) scheme of indexing of strings. Below the term 'indexing unit' can stay either for byte or character (or whatever employed by the implementation).
      This determines meaning of all [int] arguments and results (excluding result of [compare]).
  *)

  type t
  (** Type for strings. *)

  val length : t -> int
  (** Length - number of indexing units *)

  type tchar
  (** Character type used by [t].*)

  val get : t -> int -> tchar
  (** Usual get function. *)

  val lift_char : char -> tchar
  (** Convert Latin-1 character to [tchar]. *)

  val lift : string -> t
  (** Convert from UTF-8 string of primitive [string] type. *)

  val to_string : t -> string
  (** Convert to primitive string with UTF-8 content. *)

  val concat_with_separators : t -> t list -> t
  (** [concat_with_separators sep lst] catenates all {i n} elements of [lst] inserting {i (n-1)} copies of [sep] in between. *)

  val compare : t -> t -> int
  (** Usual comparison function. *)

  val iter : (tchar -> unit) -> t -> unit
  val iteri : (int -> tchar -> unit) -> t -> unit

  val sub : t -> int -> int -> t
  (** As {!String.sub}, but indexed in specific way. *)

  val rindex : t -> char -> int

  module Parse : sig
    val source : t -> (tchar, BatCharParser.position) BatParserCo.Source.t
    val letter : (tchar, tchar, BatCharParser.position) BatParserCo.t
  end
end

(** All implementations of [Path] functionality have this module type. *)
module type PathType = sig

  type ustring
  (** Type of strings used. In case of {!Path.OfRope} it is {!Rope.t} and in {!Path.OfString} module it is  [string].
  *)

  type uchar
  (** Type of characters. It corresponds to [ustring] type. *)

  (** Convenience operator for lifting primitive strings to [ustring] type.

      @documents Future.Path.OperatorLift
  *)
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

  (**
     Infix operators for path construction. They are in separate module, so one can [open Path.Operators] to use them.

     @documents Future.Path.Operators
  *)
  module Operators : sig

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

  (**
     As other Operators modules in batteries are named "Infix" we provide Infix as well.
     This is a mere copy of Operators.
  *)
  module Infix : sig

    val (/:) : t -> ustring -> t
    val (//@) : t -> t -> t

  end

  exception Malformed_path

  val normalize_filepath : t -> t
  (**
     Consumes single dots where possible, e.g.:

     [normalize (\[".."\]/:"foo"/:"."/:"bar"/:"sub1"/:".."/:"sub2") = \[".."\]/:"foo"/:"bar"/:"sub1"/:".."/:"sub2"]

     When a directory structure contains links, it can be not pefectly pure tree. Then meaing of the ".." symbol depends on the real nature of parent of what is denoted by the name that preceded the ".." symbol. This symbol cannot be resolved for a graph traversal case when dealing with abstract paths only.

     {e Windows:} If single dot is next to root, it is preserved.
  *)
  val normalize_in_graph : t -> t
  (** Another name for [normalize_filepath]. *)

  val normalize_in_tree : t -> t
  (**
     Consumes single dots and applies double dots where possible, e.g.:

     [normalize (\[".."\]/:"foo"/:"."/:"bar"/:"sub1"/:".."/:"sub2") = \[".."\]/:"foo"/:"bar"/:"sub2"]

     This normalization is useful when dealing with paths that describe locations in a tree and the ".." symbol always points to the only parent of what precedes this symbol.

     {e Windows:} If single dot is next to root, it is preserved.
     @raise Malformed_path when absolute path is given that contains double dots that would be applied to the root.
  *)

  val normalize : t -> t
  (** Deprecated name for [normalize_in_tree] *)

  val parent : t -> t
  (** Returns parent path, i.e. immediate ancestor: [parent (foo/:bar) = foo]
      @raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given
  *)

  val belongs : t -> t -> bool
  (** [belongs base sub] is [true] when [sub] descends from [base], i.e. [base] is a prefix of [sub]. If [base]=[sub] the function returns [true]. It is otherwise [false].
      Both arguments must be absolute paths or both relative.

      If both arguments have a root portion with drive letter and these letters are different, [belongs base sub] returns false.

      @raise Invalid_argument if exactly one of given arguments is absolute path
  *) (* Should this function normalize its arguments? *)

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
      [(normalize parent)/:rel = normalize sub]. It is checked if [sub] is really a descendant of [parent].
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

  val to_ustring : t -> ustring
  (** Convert to the chosen [ustring] type. Empty relative path is converted to "." (single dot).

      {e Windows:} backslash is used as a separator and double backslash for root. If the path is only a drive letter (empty absolute path) trailing backslash is added (e.g. [to_string \["C:"\] = "C:\"]).

      @see 'to_string' is likely to bo more useful
      "*)(* Dangling quote character because of ocamldoc lexer being apparently incompatible with OCaml. *)

  val to_string : t -> string
  (** Convert to type primitive string with UTF-8 content. The string is built in the same way as by [to_ustring] function. *)

  val of_string : ustring -> t
  (** Parse path in a given string. Any number of consecutive separators collapse ("a//b" becomes "a/b"). [Path.default_validator] is applied to each resulting name.

      {e Windows:} both slashes '\' and '/' are accepted as separators. Paths of the 'semi-relative' form "C:foo\bar" are not recognized. For example "C:" string is parsed as [\["C:"\]] which has different meaning (see {!to_string}).
      @raise Illegal_char when a character not allowed in paths is found.
  *)

  (** {7 Convenience aliases} *)

  val s : t -> string
  (** = {!to_string} *)

  val p : ustring -> t
  (** = {!of_string} *)

  (** {6 Name related functions}
      These funtions do not accept empty paths, i.e. [\[\]], [\[""\]] or [\["C:"\]].
  *)

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

      @example "Count unfinished music downloads (files ending with '.ogg.part')."
      {[
        let count_music_parts download_dir =
          let files = Directory.files download_dir in
          let check file =
            match Path.ext file with
            | Some "part" -> ((Path.ext (Path.name_core file)) = "ogg")
            | _ -> false
          in
          let music_parts = List.filter check files in
          List.length music_parts
      ]}

      @raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given
  *)

  val map_ext : (ustring option -> ustring option) -> t -> t
  (** [map_ext fu path] returns [path] but with the name with extension given by [fu (]{!PathType.ext}[ path)]. If [fu] returns [Some _], the original extension may be replaced (when [Some ext] is passed to [fu]) or new added (when [fu] gets [None]). In case [fu] returns [None], the extension is removed (if exists).

      @example "A name for file being encoded in a new format."
      {[
        let pngname file = map_ext (function Some _ | None -> Some "png") file
        let new_bar = pngname (["foo"]/:"bar.jpeg") (* = ["foo"]/:"bar.png" *)
      ]}

      {!PathType.default_validator} is applied to the resulting name.

      The replacement string returned by the mapping function [fu] can contain dots. Consequently, this string doesn't need to be an extension as defined by the {!ext} function. Consider for example:
      {[
        let before = foo/:"bar.mli"
        let replacement = "mli.off"
        let ext_before = Path.ext before (* = Some "mli" *)
        let after = Path.map_ext (fun _ -> Some replacement) before (* = foo/:"bar.mli.off" *)
        let ext_after = Path.ext after (* = Some "off" *)
      ]}
      Note the difference between [replacement] and [ext_after]!
      [(map_ext fu)] is idempotent only if [fu] always returns [Some _]. Otherwise it can remove the extension, possibly exposing part of the name that becomes the new extension.

      {e Windows:} If [fu] returns [Some ""] (to make a name with trailing period) [map_ext] returns a path that shouldn't be passed to the operating system (it is invalid).

      @raise Illegal_char (raised by validator if any bad character is found)
      @raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given
  *)

  val name_core : t -> ustring
  (**
     Returns part of the name to the left of rightmost dot. Returns empty string if the name starts with a dot.

     @example "Label for a piece of GUI in which a file is edited."
     {[
       let tab_label modified file =
         let text = (if modified then "*" else "") ^ (Path.name_core file) in
         GMisc.label ~text ()
     ]}

     @raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given
  *)

  type components = t * ustring * ustring option
  (** A [path] can be represented by the following triple:
      [(Path.parent path, Path.name_core path, Path.ext path)]
  *)

  val split : t -> components
  (** Dissect the path to its components (parent path, core part of name and possibly an extension).

      Resulting [name_core] string can be empty. For example,
      [Path.split (Path.root/:"home"/:"user"/:".bashrc")] equals [(Path.root/:"home"/:"user", "", Some "bashrc")].

      @raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given
  *)

  val join : components -> t
  (** Create a path from given components.

      @raise Illegal_char (raised by validator on any bad character)

      @example "Creating paths for a series of numbered images."
      {[
        let get_animation_frames working_dir count =
          let frame_file num = Path.join
              (working_dir/:"rendering"
              ,"frame"^(stirng_of_int num)
              ,Some "png"
              )
          in
          BatEnum.map frame_file (1 -- count)
      ]}
  *)

  val map : (components -> components) -> t -> t
  (** Map a path through a function that operates on separate components.

      @raise Illegal_char (raised by validator on any bad character)
      @raise Invalid_argument if empty path (relative [\[\]] or absolute [\[""\]]) is given

      @example "Insert a string just before file extension."
      {[
        let extract_first_page file =
          let insert (parent, name_core, ext) = (parent, name_core ^ "_page1", ext) in
          let result_file = Path.map insert file in
          let code = Sys.command
              (String.concat ' '
                 ["psselect -p1 <"; P.s file
                 ;" >"; P.s result_file
                 ]
              )
          in
          if code = 0 then result_file else failwith "psselect"
      ]}
  *)

  (** {6 Supplementary functions} *)

  val drive_letter : t -> uchar option
    (**
       Return drive letter of the given absolute path.

       {e Windows:} [drive_letter abs] returns [None] if [abs] is simple absolute path (i.e. begins with a separator), otherwise the root element of [abs] consists of a letter [ch] with a colon - in this case [Some ch] is returned.

       {e Other systems:} Returns [None] on all absolute paths.

       @example "(Windows only) Are the locations on the same partition?"
       {[let can_move_quickly ~path_from ~path_to =
         (drive_letter path_from) = (drive_letter path_to)
       ]}

       @raise Invalid_argument if relative path is given
    *)

end

(* end of the part that is copied to PathGen.ml *)


module Make : functor (S : StringType) -> PathType with type ustring = S.t and type uchar = S.tchar
(** Constructs path handling module for string-like type and its operations given in [S].

    @documents Future.Path.Make
*)

module OfString : PathType with type ustring = string and type uchar = char
(** This implementation can be used with UTF-8, but encoding of used strings is not verified.

    @documents Future.Path.OfString
*)

(*
module OfRope : PathType with type ustring = Rope.t
(** In this implementation used strings are always valid UTF-8.

    @documents Future.Path.OfRope
*)

*)
