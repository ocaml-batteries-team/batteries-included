# *qtest* : Quick Test for OCaml (version 2)

In a nutshell, *qtest* is a small program which reads `.ml` and `.mli` source file and extracts inline [oUnit] unit tests from them. It is used internally by the OCaml [Batteries] project, and is shipped with it as of version 2.0, but it does not depend on it and can be compiled and used independently.

[Batteries]: http://batteries.forge.ocamlcore.org/
[oUnit]: http://ounit.forge.ocamlcore.org/

## Using *qtest* : a Quick, Simple Example

Say that you have a file `foo.ml`, which contains the implementation of your new, shiny function `foo`.

```ocaml
    let rec foo x0 f = function
      [] -> 0 | x::xs -> f x (foo x0 f xs)
```

Maybe you don't feel confident about that code; or maybe you do, but you know that the function might be re-implemented less trivially in the future and want to prevent potential regressions. Or maybe you simply think unit tests are good practice anyway. In either case, you feel that building a separate test suite for this would be overkill. Using *qtest*, you can immediately put simple unit tests in comments near `foo`, for instance:

    (*$T foo
      foo  0 ( + ) [1;2;3] = 6
      foo  0 ( * ) [1;2;3] = 0
      foo  1 ( * ) [4;5]   = 20
      foo 12 ( + ) []      = 12
    *)

the syntax is simple: `(*$` introduces a *qtest* "pragma", such as `T` in this case. `T` is by far the most common and represents a "simple" unit test. `T` expects a "header", which is most of the time simply the name of the function under test, here `foo`. Following that, each line is a "statement", which must evaluate to true for the test to pass. Furthermore, `foo` must appear in each statement. 

Now, in order to execute those tests, you need to extract them; this is done with the *qtest* executable. The command

    $ qtest -o footest.ml extract foo.ml
    Target file: `footest.ml'. Extraction : `foo.ml' Done.

will create a file `footest.ml`; it's not terribly human-readable, but you can see that it contains your tests as well as some [oUnit] boilerplate. Now you need to compile the tests, for instance with `ocamlbuild`, and assuming [oUnit] was installed for `ocamlfind`.
    
    $ ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -package oUnit footest.native
    Finished, 10 targets (1 cached) in 00:00:00.

Note that the `-cflags -warn-error,+26` is not indispensable but strongly recommended. Its function will be explained in more detail in the more technical sections of this documentation, but roughly it makes sure that if you write a test for `foo`, via `(*$T foo` for instance, then `foo` is *actually* tested by each statement -- the tests won't compile if not.

**Important note:** in order for this to work, `ocamlbuild` must know where to find `foo.ml`; if `footest.ml` is not in the same directory, you must make provisions to that effect. If `foo.ml` needs some specific flags in order to compile, they must also be passed.

Now there only remains to run the tests:

    $ ./footest.native
    ..FF
    ==============================================================================
    Failure: qtest:0:foo:3:foo.ml:10

    OUnit: foo.ml:10::>  foo 12 ( + ) [] = 12
    ------------------------------------------------------------------------------
    ==============================================================================
    Failure: qtest:0:foo:2:foo.ml:9

    OUnit: foo.ml:9::>  foo 1 ( * ) [4;5] = 20
    ------------------------------------------------------------------------------
    Ran: 4 tests in: 0.00 seconds.
    FAILED: Cases: 4 Tried: 4 Errors: 0 Failures: 2 Skip:0 Todo:0

Oops, something's wrong... either the tests are incorrect of `foo` is. Finding and fixing the problem is left as an exercise for the reader. When this is done, you get the expected

    $ ./footest.native
    ....
    Ran: 4 tests in: 0.00 seconds.

**Tip:** those steps are easy to automate, for instance with a small shell script:

``` bash
    set -e # stop on first error
    qtest -o footest.ml extract foo.ml
    ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -package oUnit footest.native
    ./footest.native
```
    
## More *qtest* Pragmas

### Different Kinds of Tests
    
#### Simple Tests: `T` for "Test"

The most common kind of tests is the simple test, an example of which is given above. It is of the form

    (*$T <header>
      <statement>
      ...
    *)

where each *statement* must be a boolean OCaml expression involving the function (or functions, as we will see when we study headers) referenced in the *header*.
The overall test is considered successful if each *statement* evaluates to **true**. Note that the "close comment" `*)` must appear on a line of its own.

**Tip:** if a statement is a bit too long to fit on one line, if can be broken using a backslash (`\`), immediately followed by the carriage return. This also applies to randomised tests.

#### Randomized Tests: `Q` for "Quickcheck"

Quickcheck is a small library useful for randomized unit tests. Using it is a bit more complex, but much more rewarding than simple tests.

    (*$Q <header>
      <generator> (fun <generated value> -> <statement>)
      ...
    *)

Let us dive into an example straightaway:

    (*$Q foo
      Q.small_int (fun i-> foo i (+) [1;2;3] = List.fold_left (+) i [1;2;3])
    *)

The Quickcheck module is accessible simply as *Q* within inline tests; `small_int` is a generator, yielding a random, small integer. When the test is run, each statement will be evaluated for a large number of random values -- 100 by default. Running this test for the above definition of foo catches the mistake easily:

    law foo.ml:14::>  Q.small_int (fun i-> foo i (+) [1;2;3] = List.fold_left (+) i [1;2;3])
    failed for 2

Note that the random value for which the test failed is provided by the error message -- here it is 2. It is also possible to generate several random values simultaneously using tuples. For instance

``` ocaml
    (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> foo i (+) l = List.fold_left (+) i l)
```
    
will generate both an integer and a list of small integers randomly. A failure will then look like

    law foo.ml:15::>  (Q.pair Q.small_int (Q.list Q.small_int))
        (fun (i,l)-> foo i (+) l = List.fold_left (+) i l)
    failed for (727, [4; 3; 6; 1; 788; 49])

**Available Generators:**

* **Simple generators:**
  `unit`, `bool`, `float`, `pos_float`, `neg_float`, `int`, `pos_int`, `small_int`, `neg_int`, `char`, `printable_char`, `numeral_char`, `string`, `printable_string`, `numeral_string`

* **Structure generators:**
  `list` and `array`. They take one generator as their argument. For instance `(Q.list Q.neg_int)` is a generator of lists of (uniformly taken) negative integers.

* **Tuples generators:**
  `pair` and `triple` are respectively binary and ternary. See above for an example of `pair`.

* **Size-directed generators:**
  `string`, `numeral_string`, `printable_string` `list` and `array` all have `*_of_size` variants that take the size of the structure as their first argument.

#### Raw [oUnit] Tests: `R` for "Raw"

When more specialised test pragmas are too restrictive, for instance if the test is too complex to reasonably fit on one line, then one can use raw [oUnit] tests.

    (*$R <header>
      <raw oUnit test>...
      ...
    *)

Here is a small example, with two tests stringed together:

    (*$R foo
      let thing = foo  1 ( * )
      and li = [4;5] in
      assert_bool "something_witty" (thing li = 20);
      assert_bool "something_wittier" (foo 12 ( + ) [] = 12)
    *)

Note that if the first assertion fails, the second will not be executed; so stringing two assertions in that mode is different in that respect from doing so under a `T` pragma, for instance.

That said, raw tests should only be used as a last resort; for instance you don't automatically get the source file and line number when the test fails. If `T` and `Q` do not satisfy your needs, then it is *probably* a hint that the test is a bit complex and, maybe, belongs in a separate test suite rather than in the middle of the source code.

#### Exception-Throwing Tests: `E` for "Exception"

... not implemented yet...

The current pattern is to use `(*$T` and the following pattern for function `foo` and exception `Bar`:

    (*$T foo
      Result.(catch foo x |> is_exn Bar)
     *)
  
### More Technical Pragmas

 ...coming soon...

 this section will deal with the pragmas for local preambles and private functions and stuff. Once it's implemented, that is.

 It will also deal with ocamldoc comments that defines unit tests from the offered examples

## Technical Considerations and Other Details

What has been said above should suffice to cover at least 90% of use-cases for *qtest*. This section concerns itself with the remaining 10%.

### Function Coverage

The headers of a test are not just there for decoration; three properties are enforced when a test, say, `(*$X foo` is compiled, where `X` is `T`, `R`, `Q`,... :

* `foo` exists; that is to say, it is defined in the scope of the module where the test appears -- though one can play with pragmas to relax this condition somewhat. At the very least, it has to be defined *somewhere*. Failure to conform results in an `Error: Unbound value foo`.

* `foo` is referenced in *each statement* of the test: for `T` and `Q`, that means "each line". For `R`, that means "once somewhere in the test's body". Failure to conform results in a `Warning 26: unused variable foo`, which will be treated as an error if `-warn-error +26` is passed to the compiler. It goes without saying that this is warmly recommended.

* the test possesses at least one statement.

Those two conditions put together offer a strong guarantee that, if a function is referenced in a test header, then it is actually tested at least once. The list of functions referenced in the headers of extracted tests is written by *qtest* into `qtest.targets.log`. Each line is of the form

    foo.ml   42    foo

where `foo.ml` is the file in which the test appears, as passed to `extract` and `42` is the line number where the test pragma appears in foo.ml. Note that a same function can be listed several times for the same source file, if several tests involve it (say, two times if it has both a simple test and a random one). The exact number of statements involving `foo` is currently not taken into account in the logs.

### Headers and Metaheaders

The informal definition of headers given in the above was actually a simplification. In this section we explore to syntaxes available for headers.

#### Aliases

Some functions have exceedingly long names. Case in point :

``` ocaml
let rec pretentious_drivel x0 f = function [] -> x0
  | x::xs -> pretentious_drivel (f x x0) f xs
```
  
    (*$T pretentious_drivel
      pretentious_drivel 1 (+) [4;5] = foo 1 (+) [4;5]
      ... pretentious_drivel of this and that...
    *)

The constraint that each statement must fit on one line does not play well with very long function names. Furthermore, you *known* which function is being tested, it's right there is the header; no need to repeat it a dozen times. Instead, you can define an *alias*, and write equivalently:

    (*$T pretentious_drivel as x
      x 1 (+) [4;5] = foo 1 (+) [4;5]
      ... x of this and that...
    *)

... thus saving many keystrokes, thereby contributing to the preservation of the environment. More seriously, aliases have uses beyond just saving a few keystrokes, as we will see in the next sections.
    
#### Mutually Tested Functions

Most of the time, a test only pertains to one function. There are times, however, when one wishes to test two functions -- or more -- at the same time. For instance

``` ocaml
let rec even = function 0 -> true
  | n -> odd (pred n)
and odd = function 0 -> false
  | n -> even (pred n)
```

Let us say that we have the following test:

    (*$Q <header>
      Q.small_int (fun n-> odd (abs n+3) = even (abs n))
    *)

It involves both `even` and `odd`. That question is: "what is a proper header for this test"? One could simply put "even", and thus it would be referenced as being tested in the logs, but `odd` would not, which is unfair. Putting "odd" is symmetrically unfair. The solution is to put both, separated by a semi-colon:
    
    (*$Q even; odd

That way *both* functions are referenced in the logs:

    foo.ml   37    even
    foo.ml   37    odd

and of course the compiler enforces that both of them are actually referenced in each statement of the test. Of course, each of them can be written under alias, in which case the header could be `even as x; odd as y`.

#### Testing Functions by the Dozen

Let us come back to our functions `foo` (after correction) and `pretentious_drivel`, as defined above.

``` ocaml
let rec foo x0 f = function
  [] -> x0 | x::xs -> f x (foo x0 f xs)
  
let rec pretentious_drivel x0 f = function [] -> x0
  | x::xs -> pretentious_drivel (f x x0) f xs
```

Astute readers (as well as slightly-less-astute readers, in that case) will not have failed to notice that they bear more than a passing resemblance to one another. If you write tests for one, odds are that the same test could be useful verbatim for the other. This is a very common case when you have several *implementations* of the same function, for instance the old, slow, naïve, trustworthy one and the new, fast, arcane, highly optimised version you have just written. For our example, recall that we have the following test for `foo`:

    (*$Q foo
      (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> foo i (+) l = List.fold_left (+) i l)
    *)

The same test would apply to `pretentious_drivel`; you could just copy-and-paste the test and change the header, but it's not terribly elegant. Instead, you can just just add the other function to the header, separating the two by a comma, and defining an alias:

    (*$Q foo, pretentious_drivel as x
      (Q.pair Q.small_int (Q.list Q.small_int)) (fun (i,l)-> x i (+) l = List.fold_left (+) i l)
    *)

This same test will be run once for `x = foo`, and once for `x = pretentious_drivel`. Actually, you need not define an alias: if the header is of the form
    
    (*$Q foo, pretentious_drivel

then it is equivalent to

    (*$Q foo, pretentious_drivel as foo
    
so you do not need to alter the body of the test if you subsequently add new functions. A header which combines more than one "version" of a function in this way is called a *metaheader*.

#### Metaheaders Unleashed

All the constructs above can be combined without constraints: the grammar is as follows:

``` ocaml
    Metaheader  ::=   Binding {";" Binding}
    Binding     ::=   Functions [ "as" ID ]
    Functions   ::=   ID {"," ID}
    ID          ::=   (*OCaml lower-case identifier*)
```

### Warnings and Exceptions Thrown by *qtest*

    Fatal error: exception Failure("Unrecognised qtest pragma: ` T foo'")

You have written something like `(*$ T foo`; there must not be any space between `(*$` and the pragma.

    Warning: likely qtest syntax error: `(* $T foo'. Done.

Self-explanatory; if `$` is the first real character of a comment, it's likely a mistyped qtest pragma. This is only a warning though.

    Fatal error: exception Core.Bad_header_char("M", "Mod.foo")

You have used a qualified name in a header, for instance `(*$T Mod.foo`. You cannot do that, the name must be unqualified and defined under the local scope. Furthermore, it must be public, unless you have used pragmas to deal with private functions.

    Error: Comment not terminated
    Fatal error: exception Core.Unterminated_test(_, 0)

Most probably, you forgot the comment-closing `*)` to close some test.

    Fatal error: exception Failure("runaway test body terminator: n))*)")

The comment-closing `*)` must be on a line of its own; or, put another way, every statement must be ended by a line break.

### Backwards Compatibility

*qtest* is somewhat compatible with its Batteries ancestor. It supports tests closed by `**)` -- this style is deprecated and will only be supported as long as strictly necessary for a smooth transition.

### *qtest* Command-Line Options

      
    $ qtest --help

    ** qtest (qtest)
    USAGE: qtest [options] extract <file.mli?>...

    OPTIONS:
    --output <file.ml>    (-o) def: standard output
      Open or create a file for output; the resulting file will be an OCaml
      source file containing all the tests.

    --preamble <string>   (-p) def: empty
      Add code to the tests' preamble; typically this will be an instruction
      of the form 'open Module;;'


    --help          Displays this help page and stops

