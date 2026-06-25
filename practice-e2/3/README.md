# CMSC 430 PRACTICE Exam 2, Part 3

## Instructions

The yin to `vector->list`'s yang is `list->vector`. You've been provided a
modified implementation of the Hoax language that was presented in class. In
this problem, you're asked to implement `list->vector`, a unary operator that
takes a list and returns a vector with the elements of the list. Here are some
examples:

```racket
> (list->vector '())
'#()
> (list->vector (cons 1 (cons 2 (cons 3 '()))))
'#(1 2 3)
```

Your `list->vector` should work similarly to Racket's:

  * The syntax is: `(list->vector e)`.
  * `e` should evaluate to a list, _l_.
  * The operation creates and returns a vector holding the elements of _l_ in
    the order in which they appear in the list.
  * The operation makes no change to the given list.

The AST types, parser, and interpreter have been updated to implement this new
form, and a stub has been added to the compiler. You must finish the compiler
implementation.

Implement the functionality in `compiler/compile-ops.rkt` so that `list->vector` works.


## Notes

  * A few very simple tests have been added to `test/define-tests.rkt`, which
    will all pass for the interpreter but fail for the compiler.

    - Run `raco test test/run-compile-tests.rkt` to test the compiler.
    - Run `raco test test/run-interp-tests.rkt` to test the interpreter.
    - Run `raco test test/` to run all of the tests.

    These tests are **not** comprehensive. Write your own tests to check your
    work!

  * You do not need to write tests, but we highly encourage it!

  * You may want to review the code to determine how the empty vector is
    represented in this version of the compiler.

  * You do not need to solve problem 2 (or any other problem) in order to do
    this one; they are completely independent.
