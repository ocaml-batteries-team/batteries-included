(*
 * Labels -- fast return in OCaml
 * Copyright (C) 2008 David Teller
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


(** Local exceptions/labels/goto.

    This module defines a mechanism akin to SML's exception 
    generators or to C's [return], i.e. the ability to define
    local {i labels}, which may be used for immediately
    terminating an expression and returning a value.
    
    This mechanism is extremely fast (typically as fast as
    exceptions), by opposition to {!Error}'s error management
    mechanism. The drawback is that this mechanism is just
    as unsafe as exceptions, insofar as successfully compiling
    a program does not guarantee that every use of a label is
    properly caught.
    
    Example:
    {[
    let find_in_array a e =
      label (fun return ->
        for i = 0 to Array.length a - 1 do
         if Array.get a i = e then recall return (Some i)
        done;
        None
      )
    ]}
*)

type 'a t
(** A label which may be used to return values of type ['a]*)
    
val label : ('a t -> 'a) -> 'a
(** [label f] creates a new label [x] and invokes
    [f x]. If, during the execution of [f], [recall x v]
    is invoked, the execution of [f x] stops
    immediately and [label f] returns [v].
    Otherwise, if [f x] terminates normally and
    returns [y], [label f] returns [y].

    Calling [recall x v] from outside scope [f]
    is a run-time error and causes termination
    of the program.*)

val recall : 'a t -> 'a -> _
(** Return to a label. [recall l v] returns
    to the point where label [l] was obtained
    and produces value [l].

    Calling [recall l v] from outside the scope
    of [l] (i.e. the call to function [label]
    which produced [l]) is a run-time error
    and causes termination of the program.*)

