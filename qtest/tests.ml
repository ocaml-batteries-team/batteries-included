(*
tests.ml: oUnit test container

Copyright (C) 2007-2008  Mauricio Fernandez <mfp@acm.org>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

*)

(* this module is depended on by all the foo_t modules.  They each
   register a toplevel test function here.  The test_runner program
   pulls all these tests and runs them.  

   This needs to be separate from test_runner, as the execution order
   must be:

   * Tests
   * Foo_t
   * Test_runner
 *)

let verbose = ref true
(* tests should refer to this to determine whether to print verbose messages *)

let tests : OUnit.test list ref = ref []
(* list of tests to be run *)

let register x = tests := x :: !tests
(* call this function to register a test to be run *)
