(*
 * Path - Path and directory manipulation
 * Copyright (C) 2008 Dawid Towon
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
TYPE_CONV_PATH "" (*For Sexplib, Bin-prot...*)

(*
Path handling module rewritten for inclusion into Batteries. :-)

I wanted to keep it with no dependecies for a while. So this version uses mockup of Rope.t using primitive string. RopeAdapter waits for real implementation.

StringType module type is not specific to Path and I believe it should belong to Data.Text hierarchy and be generalized in a clean way. The same for its implementations, of course.

 Windows-specific behaviour not tested.

Dawid Toton
*)


(* TODO 
 - implement RopeAdapter
 - test on Windows!
 - decide about platform-dependent val compare : t -> t -> int (useless?)
 - adopt from legacy Filename: is_implicit, check_suffix, chop_suffix, chop_extension, quote
 - should we make an exception in [Path.ext] for .hidden files?
 - consider adoption of legacy Filename.current_dir_name, Filename.parent_dir_name
 
In related modules: 
 - Windows: read directories using Unicode functions
 - Windows: ensure files are open using Unicode functions
 - factor out do_with_resource
 - Directory.exists, Directory.make, etc.
*)


(* ----------------------- Copy of (most of) Path.mli 
  How to avoid having the copy here?
*)
(** This signature lists few basic operations provided by all string types. *)
module type StringType = sig
 
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
  
  However it may be sometimes useful to write components in reverse, for example:
  
  [let whose_readme = function "README"::app::"doc"::"share"::_ -> Some app | _ -> None]
  
{e Windows:} Windows absolute paths start with "\\" or with drive letter. Use following representation:
 - [Path.root/:"."/:"pipe" = \["pipe";".";""\]]  for "\\.\pipe"
 - [\["C:"\]/:"foo" = \["foo";"C:"\]] for "C:\foo"
 
 In principle the first type of paths has broader range of allowed characters, but this implementation ({!default_validator}) applies more strict rules to both. 
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

 {e Windows:} backslash is used as a separator and double backslash for root.
 *)

val of_string : ustring -> t
(** Parse path in a given string. Any number of consecutive separators collapse ("a//b" becomes "a/b"). [Path.default_validator] is applied to each resulting name.

{e Windows:} both slashes '\' and '/' are accepted as separators
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
(* End of the copy *)





module Make = functor (S : StringType) -> struct

  exception Not_parent
  exception Illegal_char
  exception Malformed_path

  let windows = match Sys.os_type with
      | "Win32" -> true
      | _ -> false

  type ustring = S.t

  let strequal s1 s2 = (S.compare s1 s2) = 0

  let lift ss = S.lift ss
  
  module OperatorLift = struct
    let (!!) = lift
  end
  
  open OperatorLift

  (* Returns true if windows and the arugment is letter-colon, false otherwise *)  
  let is_win_disk_letter =
    if windows then
      let regexp = S.Regexp.make "[A-Za-z]:" in
      (fun name -> S.Regexp.full_match regexp name)  
     else (fun _ -> false)

  let isnul ss = strequal !!"" ss
  let isroot ss = (isnul ss) || (is_win_disk_letter ss)
  let isdot ss = strequal !!"." ss
  let isdotdot ss = strequal !!".." ss

  type t = ustring list

  let is_relative path =
    match List.rev path with
     | nm :: _ when isroot nm -> false
     | _ -> true

  let is_absolute path = not (is_relative path)

  let root = [!!""]

  type validator = ustring -> bool

  let validator_simple =
    let regexp = S.Regexp.make "[^/\000]+" in
    (fun name -> S.Regexp.full_match regexp name)  
    
  let validator_windows = 
    let regexp = S.Regexp.make "[^/\\\\*\\?<>:\000-\031]+" in (* TODO: complete the list of excluded chars *)
    (*TODO: implement more rules *)
    (fun name -> S.Regexp.full_match regexp name)  
    
  let default_validator = ref (if windows then validator_windows else validator_simple)

  let apply_default_validator name =
    if not (!default_validator name) then raise Illegal_char else name

  let append path name = 
    (apply_default_validator name) :: path

  let concat basepath relpath =
    let simple_concat () =  
      if is_relative relpath then relpath @ basepath
       else raise (Invalid_argument "Path.concat")
     in  
    if windows then
     begin 
       match basepath with
        | nm :: _ when isnul nm ->
          (* special rules *)
          begin
            match relpath with
             | nm :: _ when isnul nm -> raise (Invalid_argument "Path.concat")
             | _ -> relpath @ basepath (* allow drive-letter inside the path *) 
          end
        | _ -> simple_concat ()  
     end else simple_concat ()

  module Operators = struct

    let (/:) = append
    
    let (//@) = concat
    
  end

  let normalize path =
    let rec doit cback path =
      match cback, path with
       | 0,  [] -> []
       | nn, [] -> !!".." :: (doit (nn - 1) []) 
       | 0,  [rt] when isroot rt -> path 
       | nn, [rt] when isroot rt -> raise Malformed_path
       | _,  dotdot :: rest  when isdotdot dotdot -> doit (cback + 1) rest
       | 0,  [dot;nu] when windows && (isdot dot) && (isnul nu) -> path
       | nn, [dot;nu] when windows && (isdot dot) && (isnul nu) -> raise Malformed_path
       | _,  dot :: rest when isdot dot -> doit cback rest
       | 0,  name :: rest -> name :: (doit 0 rest)
       | nn, name :: rest -> doit (nn - 1) rest
     in
    doit 0 path
     
  let parent path =   
    match path with
     | [] -> raise (Invalid_argument "Path.parent")
     | [rt] when isroot rt -> raise (Invalid_argument "Path.parent")
     | _ :: par -> par
     
  let gen_relative_to parent_only base sub =
    let base = normalize base in
    let sub = normalize sub in
    let rec fold rbase rsub =
      match rbase, rsub with
       | bname::brest, sname::srest when bname = sname -> fold brest srest
       | _::brest, _ -> if parent_only then raise Not_parent 
                         else fold brest (!!".." :: rsub)
       | [], _ -> rsub
     in
    let rbase = List.rev base in
    let rsub = List.rev sub in
    let rrel = match rbase, rsub with
     | hb::_, hs::_ when hb = hs -> fold rbase rsub
     | rt::_, _ when isroot rt -> raise (Invalid_argument "Path.relative_to_*")
     | _, rt::_ when isroot rt -> raise (Invalid_argument "Path.relative_to_*")
     | _, _ -> fold rbase rsub
     in
    List.rev rrel 
   
  let relative_to_any base sub = gen_relative_to false base sub

  let relative_to_parent base sub = gen_relative_to true base sub

  let to_string path =
    let separator = if windows then !!"\\" else !!"/" in  
    match List.rev path with 
     | [] -> !!"."
     | nl :: abs when isnul nl -> 
       let root = if windows then !!"\\\\" else !!"/" in
       S.concat_with_separators !!""
         [root; S.concat_with_separators separator abs]
     | rel -> S.concat_with_separators separator rel (* also absolute but with drive letter *)

  let separator_regexp = S.Regexp.make (if windows then "[\\/]+" else "/+")
      
  let of_string str =
    let parts = S.Regexp.split_delim separator_regexp str in
    (* Special rules apply to the first separator or leading win-disk-letter *)
    let head, relparts = match parts with
      | nm :: rest when isroot nm -> Some nm, rest
      | other -> None, other
     in 
    (* Filter out redundant separator (at most one is removed here since separator_regexp is built with + operator) *)
    let filtered_relparts = List.filter
      (function nm when isnul nm -> false | _ -> true)
      relparts
     in
    let path = List.rev 
      (match head with
        | Some nm -> nm :: filtered_relparts
        | None -> filtered_relparts
      )
     in 
    (* Validation excluding [head] contents: win-disk-letter or an empty string is omitted *) 
    List.iter
      (fun name -> ignore (apply_default_validator name))
      filtered_relparts; 
    path 
     
  let s = to_string
  let p = of_string

  let name path = 
    match path with   
     | [] -> raise (Invalid_argument "Path.parent")
     | [rt] when isroot rt -> raise (Invalid_argument "Path.parent")
     | name :: _ -> name
     
  let map_name fu path = 
    match path with   
     | [] -> raise (Invalid_argument "Path.map_name")
     | [rt] when isroot rt -> raise (Invalid_argument "Path.map_name")
     | name :: rest -> (apply_default_validator (fu name)) :: rest
     
  let split_on_last_dot =
    let regexp = S.Regexp.for_string !!"." in
    (fun name ->
      let len = S.length_bytes name in
      try
        let dot_index = S.Regexp.search_backward regexp name (len - 1) in 
        let len_ext = len - (dot_index + 1) in
        let ext = S.sub name (dot_index + 1) len_ext in
        (S.sub name 0 dot_index, (* excluding the dot *)
         Some ext (* possibly empty extension *)
        )
      with Not_found ->
        (name, None)   
    )
     
  let ext path =
    let name = name path in
    snd (split_on_last_dot name)   
        
  let map_ext fu path = 
    match path with   
     | [] -> raise (Invalid_argument "Path.map_ext")
     | [rt] when isroot rt -> raise (Invalid_argument "Path.map_ext")
     | name :: rest -> 
       let part1, part2 = split_on_last_dot name in
       match fu part2 with
        | Some new_ext -> 
           (apply_default_validator 
             (S.concat_with_separators !!"." [part1; new_ext])
           ) :: rest
        | None -> part1 :: rest
end
      
module StringAdapter = struct
 
  type t = string
  
  let length_bytes = String.length
  
  let lift ss = ss
  
  let concat_with_separators sep lst = String.concat sep lst
    
  let compare (r1 : string) (r2 : string) = compare r1 r2 
  
  let sub = String.sub
  
  module Regexp = struct
  
    type rt = Str.regexp
  
    let make = Str.regexp
    
    let for_string = Str.regexp_string
    
    let full_match rx str =
      let pref_match = Str.string_match rx str 0 in
      pref_match && (Str.match_end () = String.length str)
 
    let split_delim = Str.split_delim
    
    let search_backward = Str.search_backward
    
  end
end

module OfString = Make (StringAdapter)
            
      
(*module RopeAdapter = struct
  (* temporary stub *)

  module R = Rope
  type t = R.t
  
  let length_bytes = Rope.length
  
  let lift = R.of_latin1
  
  (*
  let concat_with_separators sep lst =
    let rec insert_sep = function
      | [] -> []
      | [one] -> [one]
      | hd :: tl -> hd::sep::(insert_sep tl)
     in
    R.concat (insert_sep lst)
  *)  
  let concat_with_separators sep lst = Rope.concat sep lst

  let compare r1 r2 = R.compare r1 r2 

  let sub = Rope.sub
  module Regexp = struct
    type rt = Str.regexp
    let make = Str.regexp
    let for_string = Str.regexp_string
    let full_match rx str =
      let pref_match = Str.string_match rx str 0 in
      pref_match && (Str.match_end () = String.length str)
    let split_delim = Str.split_delim
    let search_backward = Str.search_backward
  end
end

module OfRope = Make (RopeAdapter)*)
      
