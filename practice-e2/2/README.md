# CMSC 430 PRACTICE Exam 2, Part 2


## Instructions

You've been provided a modified implementation of the Hoax language that was
presented in class.

In Hoax we implemented vectors, but we didn't add many vector operations. You
job is to implement `vector->list`, a unary operator that takes a vector and
returns a list with the elements of the vector. Here are some examples:

```racket
> (vector->list (make-vector 3 #t))
'(#t #t #t)
> (let ((v (make-vector 3 #t)))
    (begin (vector-set! v 0 1)
           (begin (vector-set! v 1 2)
                  (begin (vector-set! v 2 3)
                         (vector->list v)))))
'(1 2 3)
```

Your `vector->list` should work similarly to Racket's:

  * The syntax is: `(vector->list e)`.
  * `e` should evaluate to a vector, _v_.
  * The operation creates and returns a list holding the elements of _v_ in the
    order in which they appear in the vector.
  * The operation makes no change to the given vector.

The AST types, parser, and interpreter have been updated to implement this new
form, and a stub has been added to the compiler. You must finish the compiler
implementation.

Implement the functionality in `compiler/compile-ops.rkt` so that `vector->list` works.


## Notes

  * A few very simple tests have been added to `test/define-tests.rkt`, which
    will all pass for the interpreter but fail for the compiler.

    - Run `raco test test/run-compile-tests.rkt` to test the compiler.
    - Run `raco test test/run-interp-tests.rkt` to test the interpreter.
    - Run `raco test test/` to run all of the tests.

    These tests are **not** comprehensive. Write your own
    tests to check your work!

  * You do not need to write tests, but we highly encourage it!

  * You may want to review the code to determine how the empty vector is
    represented in this version of the compiler.

  * HINT: it may be easiest to process the vector from right to left.
