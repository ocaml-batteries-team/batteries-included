(* this file is part of Batteries 'qtest' usage; it will be included
   at the top of the generated test runner, and is therefore a good
   location to add functions that would be convenient to write tests
   but have not yet found their place into Batteries proper. *)
open Batteries

module Pervasives = Pervasives[@warning "-3"]
[@@@warning "-52"] (* allow to match the constant payload of exception constructors *)

