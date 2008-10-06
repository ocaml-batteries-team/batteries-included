(*
 * Camlp4_Sig -- Parsing with Camlp4
 * Copyright (C) Daniel de Rauglaudre
 *               2006 Nicolas Pouillard
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

(** Camlp4 signatures.

    @documents Sig*)

(** {6 Basic signatures} *)

(** Signature with just a type. *)
module type Type = sig type t
                        end
  
(** Signature for errors modules, an Error modules can be registred with
    the {!ErrorHandler.Register} functor in order to be well printed. *)
module type Error =
  sig
    type t
    
    exception E of t
      
    val to_string : t -> string
      
    val print : Format.formatter -> t -> unit
      
  end
  
(** A signature for extensions identifiers. *)
module type Id =
  sig
    (** The name of the extension, typically the module name. *)
    val name : string
      
    (** The version of the extension, typically $Id$ with a versionning system. *)
    val version : string
      
  end
  
(** A signature for warnings abstract from locations. *)
module Warning (Loc : Type) =
  struct
    module type S =
      sig
        type warning = Loc.t -> string -> unit
        
        val default_warning : warning
          
        val current_warning : warning ref
          
        val print_warning : warning
          
      end
      
  end
  
(** {6 Advanced signatures} *)
(** A signature for locations. *)
module type Loc =
  sig
    type t
    
    (** Return a start location for the given file name.
      This location starts at the begining of the file. *)
    val mk : string -> t
      
    (** The [ghost] location can be used when no location
      information is available. *)
    val ghost : t
      
    (** {6 Conversion functions} *)
    (** Return a location where both positions are set the given position. *)
    val of_lexing_position : Lexing.position -> t
      
    (** Return an OCaml location. *)
    val to_ocaml_location : t -> Camlp4_import.Location.t
      
    (** Return a location from an OCaml location. *)
    val of_ocaml_location : Camlp4_import.Location.t -> t
      
    (** Return a location from ocamllex buffer. *)
    val of_lexbuf : Lexing.lexbuf -> t
      
    (** Return a location from [(file_name, start_line, start_bol, start_off,
      stop_line,  stop_bol,  stop_off, ghost)]. *)
    val of_tuple : (string * int * int * int * int * int * int * bool) -> t
      
    (** Return [(file_name, start_line, start_bol, start_off,
      stop_line,  stop_bol,  stop_off, ghost)]. *)
    val to_tuple : t -> (string * int * int * int * int * int * int * bool)
      
    (** [merge loc1 loc2] Return a location that starts at [loc1] and end at [loc2]. *)
    val merge : t -> t -> t
      
    (** The stop pos becomes equal to the start pos. *)
    val join : t -> t
      
    (** [move selector n loc]
      Return the location where positions are moved.
      Affected positions are chosen with [selector].
      Returned positions have their character offset plus [n]. *)
    val move : [ | `start | `stop | `both ] -> int -> t -> t
      
    (** [shift n loc] Return the location where the new start position is the old
      stop position, and where the new stop position character offset is the
      old one plus [n]. *)
    val shift : int -> t -> t
      
    (** [move_line n loc] Return the location with the old line count plus [n].
      The "begin of line" of both positions become the current offset. *)
    val move_line : int -> t -> t
      
    (** {6 Accessors} *)
    (** Return the file name *)
    val file_name : t -> string
      
    (** Return the line number of the begining of this location. *)
    val start_line : t -> int
      
    (** Return the line number of the ending of this location. *)
    val stop_line : t -> int
      
    (** Returns the number of characters from the begining of the file
      to the begining of the line of location's begining. *)
    val start_bol : t -> int
      
    (** Returns the number of characters from the begining of the file
      to the begining of the line of location's ending. *)
    val stop_bol : t -> int
      
    (** Returns the number of characters from the begining of the file
      of the begining of this location. *)
    val start_off : t -> int
      
    (** Return the number of characters from the begining of the file
      of the ending of this location. *)
    val stop_off : t -> int
      
    (** Return the start position as a Lexing.position. *)
    val start_pos : t -> Lexing.position
      
    (** Return the stop position as a Lexing.position. *)
    val stop_pos : t -> Lexing.position
      
    (** Generally, return true if this location does not come
      from an input stream. *)
    val is_ghost : t -> bool
      
    (** Return the associated ghost location. *)
    val ghostify : t -> t
      
    (** Return the location with the give file name *)
    val set_file_name : string -> t -> t
      
    (** [strictly_before loc1 loc2] True if the stop position of [loc1] is
      strictly_before the start position of [loc2]. *)
    val strictly_before : t -> t -> bool
      
    (** Return the location with an absolute file name. *)
    val make_absolute : t -> t
      
    (** Print the location into the formatter in a format suitable for error
      reporting. *)
    val print : Format.formatter -> t -> unit
      
    (** Print the location in a short format useful for debugging. *)
    val dump : Format.formatter -> t -> unit
      
    (** Same as {!print} but return a string instead of printting it. *)
    val to_string : t -> string
      
    (** [Exc_located loc e] is an encapsulation of the exception [e] with
      the input location [loc]. To be used in quotation expanders
      and in grammars to specify some input location for an error.
      Do not raise this exception directly: rather use the following
      function [Loc.raise]. *)
    exception Exc_located of t * exn
      
    (** [raise loc e], if [e] is already an [Exc_located] exception,
      re-raise it, else raise the exception [Exc_located loc e]. *)
    val raise : t -> exn -> 'a
      
    (** The name of the location variable used in grammars and in
      the predefined quotations for OCaml syntax trees. Default: [_loc]. *)
    val name : string ref
      
  end
  
(** Abstract syntax tree minimal signature.
    Types of this signature are abstract.
    See the {!Camlp4Ast} signature for a concrete definition. *)
module type Ast =
  sig
    (** {6 Syntactic categories as abstract types} *)
    type loc
    
    type meta_bool
    
    type 'a meta_option
    
    type 'a meta_list
    
    type ctyp
    
    type patt
    
    type expr
    
    type module_type
    
    type sig_item
    
    type with_constr
    
    type module_expr
    
    type str_item
    
    type class_type
    
    type class_sig_item
    
    type class_expr
    
    type class_str_item
    
    type match_case
    
    type ident
    
    type binding
    
    type rec_binding
    
    type module_binding
    
    (** {6 Location accessors} *)
    val loc_of_ctyp : ctyp -> loc
      
    val loc_of_patt : patt -> loc
      
    val loc_of_expr : expr -> loc
      
    val loc_of_module_type : module_type -> loc
      
    val loc_of_module_expr : module_expr -> loc
      
    val loc_of_sig_item : sig_item -> loc
      
    val loc_of_str_item : str_item -> loc
      
    val loc_of_class_type : class_type -> loc
      
    val loc_of_class_sig_item : class_sig_item -> loc
      
    val loc_of_class_expr : class_expr -> loc
      
    val loc_of_class_str_item : class_str_item -> loc
      
    val loc_of_with_constr : with_constr -> loc
      
    val loc_of_binding : binding -> loc
      
    val loc_of_rec_binding : rec_binding -> loc
      
    val loc_of_module_binding : module_binding -> loc
      
    val loc_of_match_case : match_case -> loc
      
    val loc_of_ident : ident -> loc
      
    (** {6 Traversals} *)
    (** This class is the base class for map traversal on the Ast.
      To make a custom traversal class one just extend it like that:
      
      This example swap pairs expression contents:
      open Camlp4.PreCast;
      [class swap = object
        inherit Ast.map as super;
        method expr e =
          match super#expr e with
          \[ <:expr\@_loc< ($e1$, $e2$) >> -> <:expr< ($e2$, $e1$) >>
          | e -> e \];
      end;
      value _loc = Loc.ghost;
      value map = (new swap)#expr;
      assert (map <:expr< fun x -> (x, 42) >> = <:expr< fun x -> (42, x) >>);]
  *)
    class map :
      object ('self_type)
        method string : string -> string
          
        method list : 'a 'b. ('self_type -> 'a -> 'b) -> 'a list -> 'b list
          
        method meta_bool : meta_bool -> meta_bool
          
        method meta_option :
          'a 'b. ('self_type -> 'a -> 'b) -> 'a meta_option -> 'b meta_option
          
        method meta_list :
          'a 'b. ('self_type -> 'a -> 'b) -> 'a meta_list -> 'b meta_list
          
        method loc : loc -> loc
          
        method expr : expr -> expr
          
        method patt : patt -> patt
          
        method ctyp : ctyp -> ctyp
          
        method str_item : str_item -> str_item
          
        method sig_item : sig_item -> sig_item
          
        method module_expr : module_expr -> module_expr
          
        method module_type : module_type -> module_type
          
        method class_expr : class_expr -> class_expr
          
        method class_type : class_type -> class_type
          
        method class_sig_item : class_sig_item -> class_sig_item
          
        method class_str_item : class_str_item -> class_str_item
          
        method with_constr : with_constr -> with_constr
          
        method binding : binding -> binding
          
        method rec_binding : rec_binding -> rec_binding
          
        method module_binding : module_binding -> module_binding
          
        method match_case : match_case -> match_case
          
        method ident : ident -> ident
          
        method unknown : 'a. 'a -> 'a
          
      end
      
    (** Fold style traversal *)
    class fold :
      object ('self_type)
        method string : string -> 'self_type
          
        method list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type
          
        method meta_bool : meta_bool -> 'self_type
          
        method meta_option :
          'a.
            ('self_type -> 'a -> 'self_type) -> 'a meta_option -> 'self_type
          
        method meta_list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a meta_list -> 'self_type
          
        method loc : loc -> 'self_type
          
        method expr : expr -> 'self_type
          
        method patt : patt -> 'self_type
          
        method ctyp : ctyp -> 'self_type
          
        method str_item : str_item -> 'self_type
          
        method sig_item : sig_item -> 'self_type
          
        method module_expr : module_expr -> 'self_type
          
        method module_type : module_type -> 'self_type
          
        method class_expr : class_expr -> 'self_type
          
        method class_type : class_type -> 'self_type
          
        method class_sig_item : class_sig_item -> 'self_type
          
        method class_str_item : class_str_item -> 'self_type
          
        method with_constr : with_constr -> 'self_type
          
        method binding : binding -> 'self_type
          
        method rec_binding : rec_binding -> 'self_type
          
        method module_binding : module_binding -> 'self_type
          
        method match_case : match_case -> 'self_type
          
        method ident : ident -> 'self_type
          
        method unknown : 'a. 'a -> 'self_type
          
      end
      
  end
  
(** Signature for OCaml syntax trees. *)
(*
    This signature is an extension of {!Ast}
    It provides:
      - Types for all kinds of structure.
      - Map: A base class for map traversals.
      - Map classes and functions for common kinds.

    == Core language ==
    ctyp               :: Representaion of types
    patt               :: The type of patterns
    expr               :: The type of expressions
    match_case         :: The type of cases for match/function/try constructions
    ident              :: The type of identifiers (including path like Foo(X).Bar.y)
    binding            :: The type of let bindings
    rec_binding        :: The type of record definitions

    == Modules ==
    module_type        :: The type of module types
    sig_item           :: The type of signature items
    str_item           :: The type of structure items
    module_expr        :: The type of module expressions
    module_binding     :: The type of recursive module definitions
    with_constr        :: The type of `with' constraints

    == Classes ==
    class_type         :: The type of class types
    class_sig_item     :: The type of class signature items
    class_expr         :: The type of class expressions
    class_str_item     :: The type of class structure items
 *)
module type Camlp4Ast =
  sig
    (** The inner module for locations *)
    module Loc : Loc
      
    type (* i . i *)
      (* i i *)
      (* foo *)
      (* Bar *)
      (* $s$ *)
      (* t as t *)
      (* list 'a as 'a *)
      (* _ *)
      (* t t *)
      (* list 'a *)
      (* t -> t *)
      (* int -> string *)
      (* #i *)
      (* #point *)
      (* ~s:t *)
      (* i *)
      (* Lazy.t *)
      (* t == t *)
      (* type t = [ A | B ] == Foo.t *)
      (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
      (* < (t)? (..)? > *)
      (* < move : int -> 'a .. > as 'a  *)
      (* ?s:t *)
      (* ! t . t *)
      (* ! 'a . list 'a -> 'a *)
      (* 's *)
      (* +'s *)
      (* -'s *)
      (* `s *)
      (* { t } *)
      (* { foo : int ; bar : mutable string } *)
      (* t : t *)
      (* t; t *)
      (* t, t *)
      (* [ t ] *)
      (* [ A of int and string | B ] *)
      (* t of t *)
      (* A of int *)
      (* t and t *)
      (* t | t *)
      (* private t *)
      (* mutable t *)
      (* ( t ) *)
      (* (int * string) *)
      (* t * t *)
      (* [ = t ] *)
      (* [ > t ] *)
      (* [ < t ] *)
      (* [ < t > t ] *)
      (* t & t *)
      (* t of & t *)
      (* $s$ *)
      (* i *)
      (* p as p *)
      (* (Node x y as n) *)
      (* $s$ *)
      (* _ *)
      (* p p *)
      (* fun x y -> *)
      (* [| p |] *)
      (* p, p *)
      (* p; p *)
      (* c *)
      (* 'x' *)
      (* ~s or ~s:(p) *)
      (* ?s or ?s:(p) *)
      (* ?s:(p = e) or ?(p = e) *)
      (* p | p *)
      (* p .. p *)
      (* { p } *)
      (* i = p *)
      (* s *)
      (* ( p ) *)
      (* (p : t) *)
      (* #i *)
      (* `s *)
      (* i *)
      (* e.e *)
      (* $s$ *)
      (* e e *)
      (* e.(e) *)
      (* [| e |] *)
      (* e; e *)
      (* assert False *)
      (* assert e *)
      (* e := e *)
      (* 'c' *)
      (* (e : t) or (e : t :> t) *)
      (* 3.14 *)
      (* for s = e to/downto e do { e } *)
      (* fun [ mc ] *)
      (* if e then e else e *)
      (* 42 *)
      (* ~s or ~s:e *)
      (* lazy e *)
      (* let b in e or let rec b in e *)
      (* let module s = me in e *)
      (* match e with [ mc ] *)
      (* new i *)
      (* object ((p))? (cst)? end *)
      (* ?s or ?s:e *)
      (* {< rb >} *)
      (* { rb } or { (e) with rb } *)
      (* do { e } *)
      (* e#s *)
      (* e.[e] *)
      (* s *)
      (* "foo" *)
      (* try e with [ mc ] *)
      (* (e) *)
      (* e, e *)
      (* (e : t) *)
      (* `s *)
      (* while e do { e } *)
      (* i *)
      (* A.B.C *)
      (* functor (s : mt) -> mt *)
      (* 's *)
      (* sig sg end *)
      (* mt with wc *)
      (* $s$ *)
      (* class cict *)
      (* class type cict *)
      (* sg ; sg *)
      (* # s or # s e *)
      (* exception t *)
      (* external s : t = s ... s *)
      (* include mt *)
      (* module s : mt *)
      (* module rec mb *)
      (* module type s = mt *)
      (* open i *)
      (* type t *)
      (* value s : t *)
      (* $s$ *)
      (* type t = t *)
      (* module i = i *)
      (* wc and wc *)
      (* $s$ *)
      (* bi and bi *)
      (* let a = 42 and c = 43 *)
      (* p = e *)
      (* let patt = expr *)
      (* $s$ *)
      (* rb ; rb *)
      (* i = e *)
      (* $s$ *)
      (* mb and mb *)
      (* module rec (s : mt) = me and (s : mt) = me *)
      (* s : mt = me *)
      (* s : mt *)
      (* $s$ *)
      (* a | a *)
      (* p (when e)? -> e *)
      (* $s$ *)
      (* i *)
      (* me me *)
      (* functor (s : mt) -> me *)
      (* struct st end *)
      (* (me : mt) *)
      (* $s$ *)
      (* class cice *)
      (* class type cict *)
      (* st ; st *)
      (* # s or # s e *)
      (* exception t or exception t = i *)
      (*FIXME*)
      (* e *)
      (* external s : t = s ... s *)
      (* include me *)
      (* module s = me *)
      (* module rec mb *)
      (* module type s = mt *)
      (* open i *)
      (* type t *)
      (* value (rec)? bi *)
      (* $s$ *)
      (* (virtual)? i ([ t ])? *)
      (* [t] -> ct *)
      (* object ((t))? (csg)? end *)
      (* ct and ct *)
      (* ct : ct *)
      (* ct = ct *)
      (* $s$ *)
      (* type t = t *)
      (* csg ; csg *)
      (* inherit ct *)
      (* method s : t or method private s : t *)
      (* value (virtual)? (mutable)? s : t *)
      (* method virtual (mutable)? s : t *)
      (* $s$ *)
      (* ce e *)
      (* (virtual)? i ([ t ])? *)
      (* fun p -> ce *)
      (* let (rec)? bi in ce *)
      (* object ((p))? (cst)? end *)
      (* ce : ct *)
      (* ce and ce *)
      (* ce = ce *)
      (* $s$ *)
      loc =
      Loc.
      t
      and meta_bool =
      | BTrue | BFalse | BAnt of string
      and 'a meta_option =
      | ONone | OSome of 'a | OAnt of string
      and 'a meta_list =
      | LNil | LCons of 'a * 'a meta_list | LAnt of string
      and ident =
      | IdAcc of loc * ident * ident
      | IdApp of loc * ident * ident
      | IdLid of loc * string
      | IdUid of loc * string
      | IdAnt of loc * string
      and ctyp =
      | TyNil of loc
      | TyAli of loc * ctyp * ctyp
      | TyAny of loc
      | TyApp of loc * ctyp * ctyp
      | TyArr of loc * ctyp * ctyp
      | TyCls of loc * ident
      | TyLab of loc * string * ctyp
      | TyId of loc * ident
      | TyMan of loc * ctyp * ctyp
      | TyDcl of loc * string * ctyp list * ctyp * (ctyp * ctyp) list
      | TyObj of loc * ctyp * meta_bool
      | TyOlb of loc * string * ctyp
      | TyPol of loc * ctyp * ctyp
      | TyQuo of loc * string
      | TyQuP of loc * string
      | TyQuM of loc * string
      | TyVrn of loc * string
      | TyRec of loc * ctyp
      | TyCol of loc * ctyp * ctyp
      | TySem of loc * ctyp * ctyp
      | TyCom of loc * ctyp * ctyp
      | TySum of loc * ctyp
      | TyOf of loc * ctyp * ctyp
      | TyAnd of loc * ctyp * ctyp
      | TyOr of loc * ctyp * ctyp
      | TyPrv of loc * ctyp
      | TyMut of loc * ctyp
      | TyTup of loc * ctyp
      | TySta of loc * ctyp * ctyp
      | TyVrnEq of loc * ctyp
      | TyVrnSup of loc * ctyp
      | TyVrnInf of loc * ctyp
      | TyVrnInfSup of loc * ctyp * ctyp
      | TyAmp of loc * ctyp * ctyp
      | TyOfAmp of loc * ctyp * ctyp
      | TyAnt of loc * string
      and patt =
      | PaNil of loc
      | PaId of loc * ident
      | PaAli of loc * patt * patt
      | PaAnt of loc * string
      | PaAny of loc
      | PaApp of loc * patt * patt
      | PaArr of loc * patt
      | PaCom of loc * patt * patt
      | PaSem of loc * patt * patt
      | PaChr of loc * string
      | PaInt of loc * string
      | PaInt32 of loc * string
      | PaInt64 of loc * string
      | PaNativeInt of loc * string
      | PaFlo of loc * string
      | PaLab of loc * string * patt
      | PaOlb of loc * string * patt
      | PaOlbi of loc * string * patt * expr
      | PaOrp of loc * patt * patt
      | PaRng of loc * patt * patt
      | PaRec of loc * patt
      | PaEq of loc * ident * patt
      | PaStr of loc * string
      | PaTup of loc * patt
      | PaTyc of loc * patt * ctyp
      | PaTyp of loc * ident
      | PaVrn of loc * string
      and expr =
      | ExNil of loc
      | ExId of loc * ident
      | ExAcc of loc * expr * expr
      | ExAnt of loc * string
      | ExApp of loc * expr * expr
      | ExAre of loc * expr * expr
      | ExArr of loc * expr
      | ExSem of loc * expr * expr
      | ExAsf of loc
      | ExAsr of loc * expr
      | ExAss of loc * expr * expr
      | ExChr of loc * string
      | ExCoe of loc * expr * ctyp * ctyp
      | ExFlo of loc * string
      | ExFor of loc * string * expr * expr * meta_bool * expr
      | ExFun of loc * match_case
      | ExIfe of loc * expr * expr * expr
      | ExInt of loc * string
      | ExInt32 of loc * string
      | ExInt64 of loc * string
      | ExNativeInt of loc * string
      | ExLab of loc * string * expr
      | ExLaz of loc * expr
      | ExLet of loc * meta_bool * binding * expr
      | ExLmd of loc * string * module_expr * expr
      | ExMat of loc * expr * match_case
      | ExNew of loc * ident
      | ExObj of loc * patt * class_str_item
      | ExOlb of loc * string * expr
      | ExOvr of loc * rec_binding
      | ExRec of loc * rec_binding * expr
      | ExSeq of loc * expr
      | ExSnd of loc * expr * string
      | ExSte of loc * expr * expr
      | ExStr of loc * string
      | ExTry of loc * expr * match_case
      | ExTup of loc * expr
      | ExCom of loc * expr * expr
      | ExTyc of loc * expr * ctyp
      | ExVrn of loc * string
      | ExWhi of loc * expr * expr
      and module_type =
      | MtNil of loc
      | MtId of loc * ident
      | MtFun of loc * string * module_type * module_type
      | MtQuo of loc * string
      | MtSig of loc * sig_item
      | MtWit of loc * module_type * with_constr
      | MtAnt of loc * string
      and sig_item =
      | SgNil of loc
      | SgCls of loc * class_type
      | SgClt of loc * class_type
      | SgSem of loc * sig_item * sig_item
      | SgDir of loc * string * expr
      | SgExc of loc * ctyp
      | SgExt of loc * string * ctyp * string meta_list
      | SgInc of loc * module_type
      | SgMod of loc * string * module_type
      | SgRecMod of loc * module_binding
      | SgMty of loc * string * module_type
      | SgOpn of loc * ident
      | SgTyp of loc * ctyp
      | SgVal of loc * string * ctyp
      | SgAnt of loc * string
      and with_constr =
      | WcNil of loc
      | WcTyp of loc * ctyp * ctyp
      | WcMod of loc * ident * ident
      | WcAnd of loc * with_constr * with_constr
      | WcAnt of loc * string
      and binding =
      | BiNil of loc
      | BiAnd of loc * binding * binding
      | BiEq of loc * patt * expr
      | BiAnt of loc * string
      and rec_binding =
      | RbNil of loc
      | RbSem of loc * rec_binding * rec_binding
      | RbEq of loc * ident * expr
      | RbAnt of loc * string
      and module_binding =
      | MbNil of loc
      | MbAnd of loc * module_binding * module_binding
      | MbColEq of loc * string * module_type * module_expr
      | MbCol of loc * string * module_type
      | MbAnt of loc * string
      and match_case =
      | McNil of loc
      | McOr of loc * match_case * match_case
      | McArr of loc * patt * expr * expr
      | McAnt of loc * string
      and module_expr =
      | MeNil of loc
      | MeId of loc * ident
      | MeApp of loc * module_expr * module_expr
      | MeFun of loc * string * module_type * module_expr
      | MeStr of loc * str_item
      | MeTyc of loc * module_expr * module_type
      | MeAnt of loc * string
      and str_item =
      | StNil of loc
      | StCls of loc * class_expr
      | StClt of loc * class_type
      | StSem of loc * str_item * str_item
      | StDir of loc * string * expr
      | StExc of loc * ctyp * ident meta_option
      | StExp of loc * expr
      | StExt of loc * string * ctyp * string meta_list
      | StInc of loc * module_expr
      | StMod of loc * string * module_expr
      | StRecMod of loc * module_binding
      | StMty of loc * string * module_type
      | StOpn of loc * ident
      | StTyp of loc * ctyp
      | StVal of loc * meta_bool * binding
      | StAnt of loc * string
      and class_type =
      | CtNil of loc
      | CtCon of loc * meta_bool * ident * ctyp
      | CtFun of loc * ctyp * class_type
      | CtSig of loc * ctyp * class_sig_item
      | CtAnd of loc * class_type * class_type
      | CtCol of loc * class_type * class_type
      | CtEq of loc * class_type * class_type
      | CtAnt of loc * string
      and class_sig_item =
      | CgNil of loc
      | CgCtr of loc * ctyp * ctyp
      | CgSem of loc * class_sig_item * class_sig_item
      | CgInh of loc * class_type
      | CgMth of loc * string * meta_bool * ctyp
      | CgVal of loc * string * meta_bool * meta_bool * ctyp
      | CgVir of loc * string * meta_bool * ctyp
      | CgAnt of loc * string
      and class_expr =
      | CeNil of loc
      | CeApp of loc * class_expr * expr
      | CeCon of loc * meta_bool * ident * ctyp
      | CeFun of loc * patt * class_expr
      | CeLet of loc * meta_bool * binding * class_expr
      | CeStr of loc * patt * class_str_item
      | CeTyc of loc * class_expr * class_type
      | CeAnd of loc * class_expr * class_expr
      | CeEq of loc * class_expr * class_expr
      | CeAnt of loc * string
      and class_str_item =
      | CrNil of loc
      | (* cst ; cst *)
      CrSem of loc * class_str_item * class_str_item
      | (* type t = t *)
      CrCtr of loc * ctyp * ctyp
      | (* inherit ce or inherit ce as s *)
      CrInh of loc * class_expr * string
      | (* initializer e *)
      CrIni of loc * expr
      | (* method (private)? s : t = e or method (private)? s = e *)
      CrMth of loc * string * meta_bool * expr * ctyp
      | (* value (mutable)? s = e *)
      CrVal of loc * string * meta_bool * expr
      | (* method virtual (private)? s : t *)
      CrVir of loc * string * meta_bool * ctyp
      | (* value virtual (private)? s : t *)
      CrVvr of loc * string * meta_bool * ctyp
      | CrAnt of loc * string
    
    val loc_of_ctyp : ctyp -> loc
      
    val loc_of_patt : patt -> loc
      
    val loc_of_expr : expr -> loc
      
    val loc_of_module_type : module_type -> loc
      
    val loc_of_module_expr : module_expr -> loc
      
    val loc_of_sig_item : sig_item -> loc
      
    val loc_of_str_item : str_item -> loc
      
    val loc_of_class_type : class_type -> loc
      
    val loc_of_class_sig_item : class_sig_item -> loc
      
    val loc_of_class_expr : class_expr -> loc
      
    val loc_of_class_str_item : class_str_item -> loc
      
    val loc_of_with_constr : with_constr -> loc
      
    val loc_of_binding : binding -> loc
      
    val loc_of_rec_binding : rec_binding -> loc
      
    val loc_of_module_binding : module_binding -> loc
      
    val loc_of_match_case : match_case -> loc
      
    val loc_of_ident : ident -> loc
      
    module Meta :
      sig
        module type META_LOC =
          sig
            val meta_loc_patt : loc -> loc -> patt
              
            val meta_loc_expr : loc -> loc -> expr
              
          end
          
        module MetaLoc :
          sig
            val meta_loc_patt : loc -> loc -> patt
              
            val meta_loc_expr : loc -> loc -> expr
              
          end
          
        module MetaGhostLoc :
          sig
            val meta_loc_patt : loc -> 'a -> patt
              
            val meta_loc_expr : loc -> 'a -> expr
              
          end
          
        module MetaLocVar :
          sig
            val meta_loc_patt : loc -> 'a -> patt
              
            val meta_loc_expr : loc -> 'a -> expr
              
          end
          
        module Make (MetaLoc : META_LOC) :
          sig
            module Expr :
              sig
                val meta_string : loc -> string -> expr
                  
                val meta_int : loc -> string -> expr
                  
                val meta_float : loc -> string -> expr
                  
                val meta_char : loc -> string -> expr
                  
                val meta_bool : loc -> bool -> expr
                  
                val meta_list : (loc -> 'a -> expr) -> loc -> 'a list -> expr
                  
                val meta_binding : loc -> binding -> expr
                  
                val meta_rec_binding : loc -> rec_binding -> expr
                  
                val meta_class_expr : loc -> class_expr -> expr
                  
                val meta_class_sig_item : loc -> class_sig_item -> expr
                  
                val meta_class_str_item : loc -> class_str_item -> expr
                  
                val meta_class_type : loc -> class_type -> expr
                  
                val meta_ctyp : loc -> ctyp -> expr
                  
                val meta_expr : loc -> expr -> expr
                  
                val meta_ident : loc -> ident -> expr
                  
                val meta_match_case : loc -> match_case -> expr
                  
                val meta_module_binding : loc -> module_binding -> expr
                  
                val meta_module_expr : loc -> module_expr -> expr
                  
                val meta_module_type : loc -> module_type -> expr
                  
                val meta_patt : loc -> patt -> expr
                  
                val meta_sig_item : loc -> sig_item -> expr
                  
                val meta_str_item : loc -> str_item -> expr
                  
                val meta_with_constr : loc -> with_constr -> expr
                  
              end
              
            module Patt :
              sig
                val meta_string : loc -> string -> patt
                  
                val meta_int : loc -> string -> patt
                  
                val meta_float : loc -> string -> patt
                  
                val meta_char : loc -> string -> patt
                  
                val meta_bool : loc -> bool -> patt
                  
                val meta_list : (loc -> 'a -> patt) -> loc -> 'a list -> patt
                  
                val meta_binding : loc -> binding -> patt
                  
                val meta_rec_binding : loc -> rec_binding -> patt
                  
                val meta_class_expr : loc -> class_expr -> patt
                  
                val meta_class_sig_item : loc -> class_sig_item -> patt
                  
                val meta_class_str_item : loc -> class_str_item -> patt
                  
                val meta_class_type : loc -> class_type -> patt
                  
                val meta_ctyp : loc -> ctyp -> patt
                  
                val meta_expr : loc -> expr -> patt
                  
                val meta_ident : loc -> ident -> patt
                  
                val meta_match_case : loc -> match_case -> patt
                  
                val meta_module_binding : loc -> module_binding -> patt
                  
                val meta_module_expr : loc -> module_expr -> patt
                  
                val meta_module_type : loc -> module_type -> patt
                  
                val meta_patt : loc -> patt -> patt
                  
                val meta_sig_item : loc -> sig_item -> patt
                  
                val meta_str_item : loc -> str_item -> patt
                  
                val meta_with_constr : loc -> with_constr -> patt
                  
              end
              
          end
          
      end
      
    class map :
      object ('self_type)
        method string : string -> string
          
        method list : 'a 'b. ('self_type -> 'a -> 'b) -> 'a list -> 'b list
          
        method meta_bool : meta_bool -> meta_bool
          
        method meta_option :
          'a 'b. ('self_type -> 'a -> 'b) -> 'a meta_option -> 'b meta_option
          
        method meta_list :
          'a 'b. ('self_type -> 'a -> 'b) -> 'a meta_list -> 'b meta_list
          
        method loc : loc -> loc
          
        method expr : expr -> expr
          
        method patt : patt -> patt
          
        method ctyp : ctyp -> ctyp
          
        method str_item : str_item -> str_item
          
        method sig_item : sig_item -> sig_item
          
        method module_expr : module_expr -> module_expr
          
        method module_type : module_type -> module_type
          
        method class_expr : class_expr -> class_expr
          
        method class_type : class_type -> class_type
          
        method class_sig_item : class_sig_item -> class_sig_item
          
        method class_str_item : class_str_item -> class_str_item
          
        method with_constr : with_constr -> with_constr
          
        method binding : binding -> binding
          
        method rec_binding : rec_binding -> rec_binding
          
        method module_binding : module_binding -> module_binding
          
        method match_case : match_case -> match_case
          
        method ident : ident -> ident
          
        method unknown : 'a. 'a -> 'a
          
      end
      
    class fold :
      object ('self_type)
        method string : string -> 'self_type
          
        method list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type
          
        method meta_bool : meta_bool -> 'self_type
          
        method meta_option :
          'a.
            ('self_type -> 'a -> 'self_type) -> 'a meta_option -> 'self_type
          
        method meta_list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a meta_list -> 'self_type
          
        method loc : loc -> 'self_type
          
        method expr : expr -> 'self_type
          
        method patt : patt -> 'self_type
          
        method ctyp : ctyp -> 'self_type
          
        method str_item : str_item -> 'self_type
          
        method sig_item : sig_item -> 'self_type
          
        method module_expr : module_expr -> 'self_type
          
        method module_type : module_type -> 'self_type
          
        method class_expr : class_expr -> 'self_type
          
        method class_type : class_type -> 'self_type
          
        method class_sig_item : class_sig_item -> 'self_type
          
        method class_str_item : class_str_item -> 'self_type
          
        method with_constr : with_constr -> 'self_type
          
        method binding : binding -> 'self_type
          
        method rec_binding : rec_binding -> 'self_type
          
        method module_binding : module_binding -> 'self_type
          
        method match_case : match_case -> 'self_type
          
        method ident : ident -> 'self_type
          
        method unknown : 'a. 'a -> 'self_type
          
      end
      
    val map_expr : (expr -> expr) -> map
      
    val map_patt : (patt -> patt) -> map
      
    val map_ctyp : (ctyp -> ctyp) -> map
      
    val map_str_item : (str_item -> str_item) -> map
      
    val map_sig_item : (sig_item -> sig_item) -> map
      
    val map_loc : (loc -> loc) -> map
      
    val ident_of_expr : expr -> ident
      
    val ident_of_patt : patt -> ident
      
    val ident_of_ctyp : ctyp -> ident
      
    val biAnd_of_list : binding list -> binding
      
    val rbSem_of_list : rec_binding list -> rec_binding
      
    val paSem_of_list : patt list -> patt
      
    val paCom_of_list : patt list -> patt
      
    val tyOr_of_list : ctyp list -> ctyp
      
    val tyAnd_of_list : ctyp list -> ctyp
      
    val tyAmp_of_list : ctyp list -> ctyp
      
    val tySem_of_list : ctyp list -> ctyp
      
    val tyCom_of_list : ctyp list -> ctyp
      
    val tySta_of_list : ctyp list -> ctyp
      
    val stSem_of_list : str_item list -> str_item
      
    val sgSem_of_list : sig_item list -> sig_item
      
    val crSem_of_list : class_str_item list -> class_str_item
      
    val cgSem_of_list : class_sig_item list -> class_sig_item
      
    val ctAnd_of_list : class_type list -> class_type
      
    val ceAnd_of_list : class_expr list -> class_expr
      
    val wcAnd_of_list : with_constr list -> with_constr
      
    val meApp_of_list : module_expr list -> module_expr
      
    val mbAnd_of_list : module_binding list -> module_binding
      
    val mcOr_of_list : match_case list -> match_case
      
    val idAcc_of_list : ident list -> ident
      
    val idApp_of_list : ident list -> ident
      
    val exSem_of_list : expr list -> expr
      
    val exCom_of_list : expr list -> expr
      
    val list_of_ctyp : ctyp -> ctyp list -> ctyp list
      
    val list_of_binding : binding -> binding list -> binding list
      
    val list_of_rec_binding :
      rec_binding -> rec_binding list -> rec_binding list
      
    val list_of_with_constr :
      with_constr -> with_constr list -> with_constr list
      
    val list_of_patt : patt -> patt list -> patt list
      
    val list_of_expr : expr -> expr list -> expr list
      
    val list_of_str_item : str_item -> str_item list -> str_item list
      
    val list_of_sig_item : sig_item -> sig_item list -> sig_item list
      
    val list_of_class_sig_item :
      class_sig_item -> class_sig_item list -> class_sig_item list
      
    val list_of_class_str_item :
      class_str_item -> class_str_item list -> class_str_item list
      
    val list_of_class_type : class_type -> class_type list -> class_type list
      
    val list_of_class_expr : class_expr -> class_expr list -> class_expr list
      
    val list_of_module_expr :
      module_expr -> module_expr list -> module_expr list
      
    val list_of_module_binding :
      module_binding -> module_binding list -> module_binding list
      
    val list_of_match_case : match_case -> match_case list -> match_case list
      
    val list_of_ident : ident -> ident list -> ident list
      
    val safe_string_escaped : string -> string
      
    val is_irrefut_patt : patt -> bool
      
    val is_constructor : ident -> bool
      
    val is_patt_constructor : patt -> bool
      
    val is_expr_constructor : expr -> bool
      
    val ty_of_stl : (Loc.t * string * (ctyp list)) -> ctyp
      
    val ty_of_sbt : (Loc.t * string * bool * ctyp) -> ctyp
      
    val bi_of_pe : (patt * expr) -> binding
      
    val pel_of_binding : binding -> (patt * expr) list
      
    val binding_of_pel : (patt * expr) list -> binding
      
    val sum_type_of_list : (Loc.t * string * (ctyp list)) list -> ctyp
      
    val record_type_of_list : (Loc.t * string * bool * ctyp) list -> ctyp
      
  end
  
module Camlp4AstToAst (M : Camlp4Ast) : Ast with type loc = M.loc
  and type meta_bool = M.meta_bool and type 'a meta_option = 'a M.meta_option
  and type 'a meta_list = 'a M.meta_list and type ctyp = M.ctyp
  and type patt = M.patt and type expr = M.expr
  and type module_type = M.module_type and type sig_item = M.sig_item
  and type with_constr = M.with_constr and type module_expr = M.module_expr
  and type str_item = M.str_item and type class_type = M.class_type
  and type class_sig_item = M.class_sig_item
  and type class_expr = M.class_expr
  and type class_str_item = M.class_str_item and type binding = M.binding
  and type rec_binding = M.rec_binding
  and type module_binding = M.module_binding
  and type match_case = M.match_case and type ident = M.ident = M
  
module MakeCamlp4Ast (Loc : Type) =
  struct
    type loc =
      Loc.
      t
      and meta_bool =
      | BTrue | BFalse | BAnt of string
      and 'a meta_option =
      | ONone | OSome of 'a | OAnt of string
      and 'a meta_list =
      | LNil | LCons of 'a * 'a meta_list | LAnt of string
      and ident =
      | IdAcc of loc * ident * ident
      | IdApp of loc * ident * ident
      | IdLid of loc * string
      | IdUid of loc * string
      | IdAnt of loc * string
      and ctyp =
      | TyNil of loc
      | TyAli of loc * ctyp * ctyp
      | TyAny of loc
      | TyApp of loc * ctyp * ctyp
      | TyArr of loc * ctyp * ctyp
      | TyCls of loc * ident
      | TyLab of loc * string * ctyp
      | TyId of loc * ident
      | TyMan of loc * ctyp * ctyp
      | TyDcl of loc * string * ctyp list * ctyp * (ctyp * ctyp) list
      | TyObj of loc * ctyp * meta_bool
      | TyOlb of loc * string * ctyp
      | TyPol of loc * ctyp * ctyp
      | TyQuo of loc * string
      | TyQuP of loc * string
      | TyQuM of loc * string
      | TyVrn of loc * string
      | TyRec of loc * ctyp
      | TyCol of loc * ctyp * ctyp
      | TySem of loc * ctyp * ctyp
      | TyCom of loc * ctyp * ctyp
      | TySum of loc * ctyp
      | TyOf of loc * ctyp * ctyp
      | TyAnd of loc * ctyp * ctyp
      | TyOr of loc * ctyp * ctyp
      | TyPrv of loc * ctyp
      | TyMut of loc * ctyp
      | TyTup of loc * ctyp
      | TySta of loc * ctyp * ctyp
      | TyVrnEq of loc * ctyp
      | TyVrnSup of loc * ctyp
      | TyVrnInf of loc * ctyp
      | TyVrnInfSup of loc * ctyp * ctyp
      | TyAmp of loc * ctyp * ctyp
      | TyOfAmp of loc * ctyp * ctyp
      | TyAnt of loc * string
      and patt =
      | PaNil of loc
      | PaId of loc * ident
      | PaAli of loc * patt * patt
      | PaAnt of loc * string
      | PaAny of loc
      | PaApp of loc * patt * patt
      | PaArr of loc * patt
      | PaCom of loc * patt * patt
      | PaSem of loc * patt * patt
      | PaChr of loc * string
      | PaInt of loc * string
      | PaInt32 of loc * string
      | PaInt64 of loc * string
      | PaNativeInt of loc * string
      | PaFlo of loc * string
      | PaLab of loc * string * patt
      | PaOlb of loc * string * patt
      | PaOlbi of loc * string * patt * expr
      | PaOrp of loc * patt * patt
      | PaRng of loc * patt * patt
      | PaRec of loc * patt
      | PaEq of loc * ident * patt
      | PaStr of loc * string
      | PaTup of loc * patt
      | PaTyc of loc * patt * ctyp
      | PaTyp of loc * ident
      | PaVrn of loc * string
      and expr =
      | ExNil of loc
      | ExId of loc * ident
      | ExAcc of loc * expr * expr
      | ExAnt of loc * string
      | ExApp of loc * expr * expr
      | ExAre of loc * expr * expr
      | ExArr of loc * expr
      | ExSem of loc * expr * expr
      | ExAsf of loc
      | ExAsr of loc * expr
      | ExAss of loc * expr * expr
      | ExChr of loc * string
      | ExCoe of loc * expr * ctyp * ctyp
      | ExFlo of loc * string
      | ExFor of loc * string * expr * expr * meta_bool * expr
      | ExFun of loc * match_case
      | ExIfe of loc * expr * expr * expr
      | ExInt of loc * string
      | ExInt32 of loc * string
      | ExInt64 of loc * string
      | ExNativeInt of loc * string
      | ExLab of loc * string * expr
      | ExLaz of loc * expr
      | ExLet of loc * meta_bool * binding * expr
      | ExLmd of loc * string * module_expr * expr
      | ExMat of loc * expr * match_case
      | ExNew of loc * ident
      | ExObj of loc * patt * class_str_item
      | ExOlb of loc * string * expr
      | ExOvr of loc * rec_binding
      | ExRec of loc * rec_binding * expr
      | ExSeq of loc * expr
      | ExSnd of loc * expr * string
      | ExSte of loc * expr * expr
      | ExStr of loc * string
      | ExTry of loc * expr * match_case
      | ExTup of loc * expr
      | ExCom of loc * expr * expr
      | ExTyc of loc * expr * ctyp
      | ExVrn of loc * string
      | ExWhi of loc * expr * expr
      and module_type =
      | MtNil of loc
      | MtId of loc * ident
      | MtFun of loc * string * module_type * module_type
      | MtQuo of loc * string
      | MtSig of loc * sig_item
      | MtWit of loc * module_type * with_constr
      | MtAnt of loc * string
      and sig_item =
      | SgNil of loc
      | SgCls of loc * class_type
      | SgClt of loc * class_type
      | SgSem of loc * sig_item * sig_item
      | SgDir of loc * string * expr
      | SgExc of loc * ctyp
      | SgExt of loc * string * ctyp * string meta_list
      | SgInc of loc * module_type
      | SgMod of loc * string * module_type
      | SgRecMod of loc * module_binding
      | SgMty of loc * string * module_type
      | SgOpn of loc * ident
      | SgTyp of loc * ctyp
      | SgVal of loc * string * ctyp
      | SgAnt of loc * string
      and with_constr =
      | WcNil of loc
      | WcTyp of loc * ctyp * ctyp
      | WcMod of loc * ident * ident
      | WcAnd of loc * with_constr * with_constr
      | WcAnt of loc * string
      and binding =
      | BiNil of loc
      | BiAnd of loc * binding * binding
      | BiEq of loc * patt * expr
      | BiAnt of loc * string
      and rec_binding =
      | RbNil of loc
      | RbSem of loc * rec_binding * rec_binding
      | RbEq of loc * ident * expr
      | RbAnt of loc * string
      and module_binding =
      | MbNil of loc
      | MbAnd of loc * module_binding * module_binding
      | MbColEq of loc * string * module_type * module_expr
      | MbCol of loc * string * module_type
      | MbAnt of loc * string
      and match_case =
      | McNil of loc
      | McOr of loc * match_case * match_case
      | McArr of loc * patt * expr * expr
      | McAnt of loc * string
      and module_expr =
      | MeNil of loc
      | MeId of loc * ident
      | MeApp of loc * module_expr * module_expr
      | MeFun of loc * string * module_type * module_expr
      | MeStr of loc * str_item
      | MeTyc of loc * module_expr * module_type
      | MeAnt of loc * string
      and str_item =
      | StNil of loc
      | StCls of loc * class_expr
      | StClt of loc * class_type
      | StSem of loc * str_item * str_item
      | StDir of loc * string * expr
      | StExc of loc * ctyp * ident meta_option
      | StExp of loc * expr
      | StExt of loc * string * ctyp * string meta_list
      | StInc of loc * module_expr
      | StMod of loc * string * module_expr
      | StRecMod of loc * module_binding
      | StMty of loc * string * module_type
      | StOpn of loc * ident
      | StTyp of loc * ctyp
      | StVal of loc * meta_bool * binding
      | StAnt of loc * string
      and class_type =
      | CtNil of loc
      | CtCon of loc * meta_bool * ident * ctyp
      | CtFun of loc * ctyp * class_type
      | CtSig of loc * ctyp * class_sig_item
      | CtAnd of loc * class_type * class_type
      | CtCol of loc * class_type * class_type
      | CtEq of loc * class_type * class_type
      | CtAnt of loc * string
      and class_sig_item =
      | CgNil of loc
      | CgCtr of loc * ctyp * ctyp
      | CgSem of loc * class_sig_item * class_sig_item
      | CgInh of loc * class_type
      | CgMth of loc * string * meta_bool * ctyp
      | CgVal of loc * string * meta_bool * meta_bool * ctyp
      | CgVir of loc * string * meta_bool * ctyp
      | CgAnt of loc * string
      and class_expr =
      | CeNil of loc
      | CeApp of loc * class_expr * expr
      | CeCon of loc * meta_bool * ident * ctyp
      | CeFun of loc * patt * class_expr
      | CeLet of loc * meta_bool * binding * class_expr
      | CeStr of loc * patt * class_str_item
      | CeTyc of loc * class_expr * class_type
      | CeAnd of loc * class_expr * class_expr
      | CeEq of loc * class_expr * class_expr
      | CeAnt of loc * string
      and class_str_item =
      | CrNil of loc
      | CrSem of loc * class_str_item * class_str_item
      | CrCtr of loc * ctyp * ctyp
      | CrInh of loc * class_expr * string
      | CrIni of loc * expr
      | CrMth of loc * string * meta_bool * expr * ctyp
      | CrVal of loc * string * meta_bool * expr
      | CrVir of loc * string * meta_bool * ctyp
      | CrVvr of loc * string * meta_bool * ctyp
      | CrAnt of loc * string
    
  end
  
type ('a, 'loc) stream_filter = ('a * 'loc) Stream.t -> ('a * 'loc) Stream.t

module type AstFilters =
  sig
    module Ast : Camlp4Ast
      
    type 'a filter = 'a -> 'a
    
    val register_sig_item_filter : Ast.sig_item filter -> unit
      
    val register_str_item_filter : Ast.str_item filter -> unit
      
    val fold_interf_filters : ('a -> Ast.sig_item filter -> 'a) -> 'a -> 'a
      
    val fold_implem_filters : ('a -> Ast.str_item filter -> 'a) -> 'a -> 'a
      
  end
  
module type DynAst =
  sig
    module Ast : Ast
      
    type 'a tag
    
    val ctyp_tag : Ast.ctyp tag
      
    val patt_tag : Ast.patt tag
      
    val expr_tag : Ast.expr tag
      
    val module_type_tag : Ast.module_type tag
      
    val sig_item_tag : Ast.sig_item tag
      
    val with_constr_tag : Ast.with_constr tag
      
    val module_expr_tag : Ast.module_expr tag
      
    val str_item_tag : Ast.str_item tag
      
    val class_type_tag : Ast.class_type tag
      
    val class_sig_item_tag : Ast.class_sig_item tag
      
    val class_expr_tag : Ast.class_expr tag
      
    val class_str_item_tag : Ast.class_str_item tag
      
    val match_case_tag : Ast.match_case tag
      
    val ident_tag : Ast.ident tag
      
    val binding_tag : Ast.binding tag
      
    val rec_binding_tag : Ast.rec_binding tag
      
    val module_binding_tag : Ast.module_binding tag
      
    val string_of_tag : 'a tag -> string
      
    module Pack (X : sig type 'a t
                          end) :
      sig
        type pack
        
        val pack : 'a tag -> 'a X.t -> pack
          
        val unpack : 'a tag -> pack -> 'a X.t
          
        val print_tag : Format.formatter -> pack -> unit
          
      end
      
  end
  
type quotation =
  { q_name : string; q_loc : string; q_shift : int; q_contents : string
  }

module type Quotation =
  sig
    module Ast : Ast
      
    module DynAst : DynAst with module Ast = Ast
      
    open Ast
      
    type 'a expand_fun = loc -> string option -> string -> 'a
    
    val add : string -> 'a DynAst.tag -> 'a expand_fun -> unit
      
    val find : string -> 'a DynAst.tag -> 'a expand_fun
      
    val default : string ref
      
    val parse_quotation_result :
      (loc -> string -> 'a) -> loc -> quotation -> string -> string -> 'a
      
    val translate : (string -> string) ref
      
    val expand : loc -> quotation -> 'a DynAst.tag -> 'a
      
    val dump_file : (string option) ref
      
    module Error : Error
      
  end
  
module type Token =
  sig
    module Loc : Loc
      
    type t
    
    val to_string : t -> string
      
    val print : Format.formatter -> t -> unit
      
    val match_keyword : string -> t -> bool
      
    val extract_string : t -> string
      
    module Filter :
      sig
        type token_filter = (t, Loc.t) stream_filter
        
        type t
        
        val mk : (string -> bool) -> t
          
        val define_filter : t -> (token_filter -> token_filter) -> unit
          
        val filter : t -> token_filter
          
        val keyword_added : t -> string -> bool -> unit
          
        val keyword_removed : t -> string -> unit
          
      end
      
    module Error : Error
      
  end
  
type camlp4_token =
  | KEYWORD of string
  | SYMBOL of string
  | LIDENT of string
  | UIDENT of string
  | ESCAPED_IDENT of string
  | INT of int * string
  | INT32 of int32 * string
  | INT64 of int64 * string
  | NATIVEINT of nativeint * string
  | FLOAT of float * string
  | CHAR of char * string
  | STRING of string * string
  | LABEL of string
  | OPTLABEL of string
  | QUOTATION of quotation
  | ANTIQUOT of string * string
  | COMMENT of string
  | BLANKS of string
  | NEWLINE
  | LINE_DIRECTIVE of int * string option
  | EOI

module type Camlp4Token = Token with type t = camlp4_token
  
module type DynLoader =
  sig
    type t
    
    exception Error of string * string
      
    val mk : ?ocaml_stdlib: bool -> ?camlp4_stdlib: bool -> unit -> t
      
    val fold_load_path : t -> (string -> 'a -> 'a) -> 'a -> 'a
      
    val load : t -> string -> unit
      
    val include_dir : t -> string -> unit
      
    val find_in_path : t -> string -> string
      
  end
  
module Grammar =
  struct
    module type Action =
      sig
        type t
        
        val mk : 'a -> t
          
        val get : t -> 'a
          
        val getf : t -> 'a -> 'b
          
        val getf2 : t -> 'a -> 'b -> 'c
          
      end
      
    type assoc = | NonA | RightA | LeftA
    
    type position =
      | First | Last | Before of string | After of string | Level of string
    
    module type Structure =
      sig
        module Loc : Loc
          
        module Action : Action
          
        module Token : Token with module Loc = Loc
          
        type gram
        
        type internal_entry
        
        type tree
        
        type token_pattern = ((Token.t -> bool) * string)
        
        type symbol =
          | Smeta of string * symbol list * Action.t
          | Snterm of internal_entry
          | Snterml of internal_entry * string
          | Slist0 of symbol
          | Slist0sep of symbol * symbol
          | Slist1 of symbol
          | Slist1sep of symbol * symbol
          | Sopt of symbol
          | Sself
          | Snext
          | Stoken of token_pattern
          | Skeyword of string
          | Stree of tree
        
        type production_rule = ((symbol list) * Action.t)
        
        type single_extend_statment =
          ((string option) * (assoc option) * (production_rule list))
        
        type extend_statment =
          ((position option) * (single_extend_statment list))
        
        type delete_statment = symbol list
        
        type ('a, 'b, 'c) fold =
          internal_entry ->
            symbol list -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'c
        
        type ('a, 'b, 'c) foldsep =
          internal_entry ->
            symbol list ->
              ('a Stream.t -> 'b) ->
                ('a Stream.t -> unit) -> 'a Stream.t -> 'c
        
      end
      
    module type Dynamic =
      sig
        include Structure
          
        val mk : unit -> gram
          
        module Entry :
          sig
            type 'a t
            
            val mk : gram -> string -> 'a t
              
            val of_parser :
              gram -> string -> ((Token.t * Loc.t) Stream.t -> 'a) -> 'a t
              
            val setup_parser :
              'a t -> ((Token.t * Loc.t) Stream.t -> 'a) -> unit
              
            val name : 'a t -> string
              
            val print : Format.formatter -> 'a t -> unit
              
            val dump : Format.formatter -> 'a t -> unit
              
            val obj : 'a t -> internal_entry
              
            val clear : 'a t -> unit
              
          end
          
        val get_filter : gram -> Token.Filter.t
          
        type 'a not_filtered
        
        val extend : 'a Entry.t -> extend_statment -> unit
          
        val delete_rule : 'a Entry.t -> delete_statment -> unit
          
        val srules : 'a Entry.t -> ((symbol list) * Action.t) list -> symbol
          
        val sfold0 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) fold
          
        val sfold1 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) fold
          
        val sfold0sep : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) foldsep
          
        val lex :
          gram ->
            Loc.t ->
              char Stream.t -> ((Token.t * Loc.t) Stream.t) not_filtered
          
        val lex_string :
          gram ->
            Loc.t -> string -> ((Token.t * Loc.t) Stream.t) not_filtered
          
        val filter :
          gram ->
            ((Token.t * Loc.t) Stream.t) not_filtered ->
              (Token.t * Loc.t) Stream.t
          
        val parse : 'a Entry.t -> Loc.t -> char Stream.t -> 'a
          
        val parse_string : 'a Entry.t -> Loc.t -> string -> 'a
          
        val parse_tokens_before_filter :
          'a Entry.t -> ((Token.t * Loc.t) Stream.t) not_filtered -> 'a
          
        val parse_tokens_after_filter :
          'a Entry.t -> (Token.t * Loc.t) Stream.t -> 'a
          
      end
      
    module type Static =
      sig
        include Structure
          
        module Entry :
          sig
            type 'a t
            
            val mk : string -> 'a t
              
            val of_parser :
              string -> ((Token.t * Loc.t) Stream.t -> 'a) -> 'a t
              
            val setup_parser :
              'a t -> ((Token.t * Loc.t) Stream.t -> 'a) -> unit
              
            val name : 'a t -> string
              
            val print : Format.formatter -> 'a t -> unit
              
            val dump : Format.formatter -> 'a t -> unit
              
            val obj : 'a t -> internal_entry
              
            val clear : 'a t -> unit
              
          end
          
        val get_filter : unit -> Token.Filter.t
          
        type 'a not_filtered
        
        val extend : 'a Entry.t -> extend_statment -> unit
          
        val delete_rule : 'a Entry.t -> delete_statment -> unit
          
        val srules : 'a Entry.t -> ((symbol list) * Action.t) list -> symbol
          
        val sfold0 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) fold
          
        val sfold1 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) fold
          
        val sfold0sep : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) foldsep
          
        val lex :
          Loc.t -> char Stream.t -> ((Token.t * Loc.t) Stream.t) not_filtered
          
        val lex_string :
          Loc.t -> string -> ((Token.t * Loc.t) Stream.t) not_filtered
          
        val filter :
          ((Token.t * Loc.t) Stream.t) not_filtered ->
            (Token.t * Loc.t) Stream.t
          
        val parse : 'a Entry.t -> Loc.t -> char Stream.t -> 'a
          
        val parse_string : 'a Entry.t -> Loc.t -> string -> 'a
          
        val parse_tokens_before_filter :
          'a Entry.t -> ((Token.t * Loc.t) Stream.t) not_filtered -> 'a
          
        val parse_tokens_after_filter :
          'a Entry.t -> (Token.t * Loc.t) Stream.t -> 'a
          
      end
      
  end
  
module type Lexer =
  sig
    module Loc : Loc
      
    module Token : Token with module Loc = Loc
      
    module Error : Error
      
    val mk : unit -> Loc.t -> char Stream.t -> (Token.t * Loc.t) Stream.t
      
  end
  
module Parser (Ast : Ast) =
  struct
    module type SIMPLE =
      sig
        val parse_expr : Ast.loc -> string -> Ast.expr
          
        val parse_patt : Ast.loc -> string -> Ast.patt
          
      end
      
    module type S =
      sig
        val parse_implem :
          ?directive_handler: (Ast.str_item -> Ast.str_item option) ->
            Ast.loc -> char Stream.t -> Ast.str_item
          
        val parse_interf :
          ?directive_handler: (Ast.sig_item -> Ast.sig_item option) ->
            Ast.loc -> char Stream.t -> Ast.sig_item
          
      end
      
  end
  
module Printer (Ast : Ast) =
  struct
    module type S =
      sig
        val print_interf :
          ?input_file: string -> ?output_file: string -> Ast.sig_item -> unit
          
        val print_implem :
          ?input_file: string -> ?output_file: string -> Ast.str_item -> unit
          
      end
      
  end
  
module type Syntax =
  sig
    module Loc : Loc
      
    module Ast : Ast with type loc = Loc.t
      
    module Token : Token with module Loc = Loc
      
    module Gram : Grammar.Static with module Loc = Loc
      and module Token = Token
      
    module Quotation : Quotation with module Ast = Ast
      
    module AntiquotSyntax : Parser(Ast).SIMPLE
      
    include Warning(Loc).S
      
    include Parser(Ast).S
      
    include Printer(Ast).S
      
  end
  
module type Camlp4Syntax =
  sig
    module Loc : Loc
      
    module Ast : Camlp4Ast with module Loc = Loc
      
    module Token : Camlp4Token with module Loc = Loc
      
    module Gram : Grammar.Static with module Loc = Loc
      and module Token = Token
      
    module Quotation : Quotation with module Ast = Camlp4AstToAst(Ast)
      
    module AntiquotSyntax : Parser(Ast).SIMPLE
      
    include Warning(Loc).S
      
    include Parser(Ast).S
      
    include Printer(Ast).S
      
    val interf : ((Ast.sig_item list) * (Loc.t option)) Gram.Entry.t
      
    val implem : ((Ast.str_item list) * (Loc.t option)) Gram.Entry.t
      
    val top_phrase : (Ast.str_item option) Gram.Entry.t
      
    val use_file : ((Ast.str_item list) * (Loc.t option)) Gram.Entry.t
      
    val a_CHAR : string Gram.Entry.t
      
    val a_FLOAT : string Gram.Entry.t
      
    val a_INT : string Gram.Entry.t
      
    val a_INT32 : string Gram.Entry.t
      
    val a_INT64 : string Gram.Entry.t
      
    val a_LABEL : string Gram.Entry.t
      
    val a_LIDENT : string Gram.Entry.t
      
    val a_NATIVEINT : string Gram.Entry.t
      
    val a_OPTLABEL : string Gram.Entry.t
      
    val a_STRING : string Gram.Entry.t
      
    val a_UIDENT : string Gram.Entry.t
      
    val a_ident : string Gram.Entry.t
      
    val amp_ctyp : Ast.ctyp Gram.Entry.t
      
    val and_ctyp : Ast.ctyp Gram.Entry.t
      
    val match_case : Ast.match_case Gram.Entry.t
      
    val match_case0 : Ast.match_case Gram.Entry.t
      
    val match_case_quot : Ast.match_case Gram.Entry.t
      
    val binding : Ast.binding Gram.Entry.t
      
    val binding_quot : Ast.binding Gram.Entry.t
      
    val rec_binding_quot : Ast.rec_binding Gram.Entry.t
      
    val class_declaration : Ast.class_expr Gram.Entry.t
      
    val class_description : Ast.class_type Gram.Entry.t
      
    val class_expr : Ast.class_expr Gram.Entry.t
      
    val class_expr_quot : Ast.class_expr Gram.Entry.t
      
    val class_fun_binding : Ast.class_expr Gram.Entry.t
      
    val class_fun_def : Ast.class_expr Gram.Entry.t
      
    val class_info_for_class_expr : Ast.class_expr Gram.Entry.t
      
    val class_info_for_class_type : Ast.class_type Gram.Entry.t
      
    val class_longident : Ast.ident Gram.Entry.t
      
    val class_longident_and_param : Ast.class_expr Gram.Entry.t
      
    val class_name_and_param : (string * Ast.ctyp) Gram.Entry.t
      
    val class_sig_item : Ast.class_sig_item Gram.Entry.t
      
    val class_sig_item_quot : Ast.class_sig_item Gram.Entry.t
      
    val class_signature : Ast.class_sig_item Gram.Entry.t
      
    val class_str_item : Ast.class_str_item Gram.Entry.t
      
    val class_str_item_quot : Ast.class_str_item Gram.Entry.t
      
    val class_structure : Ast.class_str_item Gram.Entry.t
      
    val class_type : Ast.class_type Gram.Entry.t
      
    val class_type_declaration : Ast.class_type Gram.Entry.t
      
    val class_type_longident : Ast.ident Gram.Entry.t
      
    val class_type_longident_and_param : Ast.class_type Gram.Entry.t
      
    val class_type_plus : Ast.class_type Gram.Entry.t
      
    val class_type_quot : Ast.class_type Gram.Entry.t
      
    val comma_ctyp : Ast.ctyp Gram.Entry.t
      
    val comma_expr : Ast.expr Gram.Entry.t
      
    val comma_ipatt : Ast.patt Gram.Entry.t
      
    val comma_patt : Ast.patt Gram.Entry.t
      
    val comma_type_parameter : Ast.ctyp Gram.Entry.t
      
    val constrain : (Ast.ctyp * Ast.ctyp) Gram.Entry.t
      
    val constructor_arg_list : Ast.ctyp Gram.Entry.t
      
    val constructor_declaration : Ast.ctyp Gram.Entry.t
      
    val constructor_declarations : Ast.ctyp Gram.Entry.t
      
    val ctyp : Ast.ctyp Gram.Entry.t
      
    val ctyp_quot : Ast.ctyp Gram.Entry.t
      
    val cvalue_binding : Ast.expr Gram.Entry.t
      
    val direction_flag : Ast.meta_bool Gram.Entry.t
      
    val dummy : unit Gram.Entry.t
      
    val eq_expr : (string -> Ast.patt -> Ast.patt) Gram.Entry.t
      
    val expr : Ast.expr Gram.Entry.t
      
    val expr_eoi : Ast.expr Gram.Entry.t
      
    val expr_quot : Ast.expr Gram.Entry.t
      
    val field_expr : Ast.rec_binding Gram.Entry.t
      
    val fun_binding : Ast.expr Gram.Entry.t
      
    val fun_def : Ast.expr Gram.Entry.t
      
    val ident : Ast.ident Gram.Entry.t
      
    val ident_quot : Ast.ident Gram.Entry.t
      
    val ipatt : Ast.patt Gram.Entry.t
      
    val ipatt_tcon : Ast.patt Gram.Entry.t
      
    val label : string Gram.Entry.t
      
    val label_declaration : Ast.ctyp Gram.Entry.t
      
    val label_expr : Ast.rec_binding Gram.Entry.t
      
    val label_ipatt : Ast.patt Gram.Entry.t
      
    val label_longident : Ast.ident Gram.Entry.t
      
    val label_patt : Ast.patt Gram.Entry.t
      
    val labeled_ipatt : Ast.patt Gram.Entry.t
      
    val let_binding : Ast.binding Gram.Entry.t
      
    val meth_list : Ast.ctyp Gram.Entry.t
      
    val module_binding : Ast.module_binding Gram.Entry.t
      
    val module_binding0 : Ast.module_expr Gram.Entry.t
      
    val module_binding_quot : Ast.module_binding Gram.Entry.t
      
    val module_declaration : Ast.module_type Gram.Entry.t
      
    val module_expr : Ast.module_expr Gram.Entry.t
      
    val module_expr_quot : Ast.module_expr Gram.Entry.t
      
    val module_longident : Ast.ident Gram.Entry.t
      
    val module_longident_with_app : Ast.ident Gram.Entry.t
      
    val module_rec_declaration : Ast.module_binding Gram.Entry.t
      
    val module_type : Ast.module_type Gram.Entry.t
      
    val module_type_quot : Ast.module_type Gram.Entry.t
      
    val more_ctyp : Ast.ctyp Gram.Entry.t
      
    val name_tags : Ast.ctyp Gram.Entry.t
      
    val opt_as_lident : string Gram.Entry.t
      
    val opt_class_self_patt : Ast.patt Gram.Entry.t
      
    val opt_class_self_type : Ast.ctyp Gram.Entry.t
      
    val opt_comma_ctyp : Ast.ctyp Gram.Entry.t
      
    val opt_dot_dot : Ast.meta_bool Gram.Entry.t
      
    val opt_eq_ctyp : Ast.ctyp Gram.Entry.t
      
    val opt_expr : Ast.expr Gram.Entry.t
      
    val opt_meth_list : Ast.ctyp Gram.Entry.t
      
    val opt_mutable : Ast.meta_bool Gram.Entry.t
      
    val opt_polyt : Ast.ctyp Gram.Entry.t
      
    val opt_private : Ast.meta_bool Gram.Entry.t
      
    val opt_rec : Ast.meta_bool Gram.Entry.t
      
    val opt_virtual : Ast.meta_bool Gram.Entry.t
      
    val opt_when_expr : Ast.expr Gram.Entry.t
      
    val patt : Ast.patt Gram.Entry.t
      
    val patt_as_patt_opt : Ast.patt Gram.Entry.t
      
    val patt_eoi : Ast.patt Gram.Entry.t
      
    val patt_quot : Ast.patt Gram.Entry.t
      
    val patt_tcon : Ast.patt Gram.Entry.t
      
    val phrase : Ast.str_item Gram.Entry.t
      
    val poly_type : Ast.ctyp Gram.Entry.t
      
    val row_field : Ast.ctyp Gram.Entry.t
      
    val sem_expr : Ast.expr Gram.Entry.t
      
    val sem_expr_for_list : (Ast.expr -> Ast.expr) Gram.Entry.t
      
    val sem_patt : Ast.patt Gram.Entry.t
      
    val sem_patt_for_list : (Ast.patt -> Ast.patt) Gram.Entry.t
      
    val semi : unit Gram.Entry.t
      
    val sequence : Ast.expr Gram.Entry.t
      
    val do_sequence : Ast.expr Gram.Entry.t
      
    val sig_item : Ast.sig_item Gram.Entry.t
      
    val sig_item_quot : Ast.sig_item Gram.Entry.t
      
    val sig_items : Ast.sig_item Gram.Entry.t
      
    val star_ctyp : Ast.ctyp Gram.Entry.t
      
    val str_item : Ast.str_item Gram.Entry.t
      
    val str_item_quot : Ast.str_item Gram.Entry.t
      
    val str_items : Ast.str_item Gram.Entry.t
      
    val type_constraint : unit Gram.Entry.t
      
    val type_declaration : Ast.ctyp Gram.Entry.t
      
    val type_ident_and_parameters : (string * (Ast.ctyp list)) Gram.Entry.t
      
    val type_kind : Ast.ctyp Gram.Entry.t
      
    val type_longident : Ast.ident Gram.Entry.t
      
    val type_longident_and_parameters : Ast.ctyp Gram.Entry.t
      
    val type_parameter : Ast.ctyp Gram.Entry.t
      
    val type_parameters : (Ast.ctyp -> Ast.ctyp) Gram.Entry.t
      
    val typevars : Ast.ctyp Gram.Entry.t
      
    val val_longident : Ast.ident Gram.Entry.t
      
    val value_let : unit Gram.Entry.t
      
    val value_val : unit Gram.Entry.t
      
    val with_constr : Ast.with_constr Gram.Entry.t
      
    val with_constr_quot : Ast.with_constr Gram.Entry.t
      
    val prefixop : Ast.expr Gram.Entry.t
      
    val infixop0 : Ast.expr Gram.Entry.t
      
    val infixop1 : Ast.expr Gram.Entry.t
      
    val infixop2 : Ast.expr Gram.Entry.t
      
    val infixop3 : Ast.expr Gram.Entry.t
      
    val infixop4 : Ast.expr Gram.Entry.t
      
  end
  
module type SyntaxExtension =
  functor (Syn : Syntax) -> Syntax with module Loc = Syn.Loc
    and module Ast = Syn.Ast and module Token = Syn.Token
    and module Gram = Syn.Gram and module Quotation = Syn.Quotation
  

