# CMSC 430 PRACTICE Exam 2, Part 6


## Instructions

You've been provided a modified implementation of the Knock language that was
presented in class. There is no compiler provided, since we did not
(sufficiently) cover the Knock compiler in class.

We want to add a useful new pattern for our `match` expressions: `app` patterns.

The `app` pattern takes a function name and a sub-pattern and matches if
applying the function to the value being matched against produces a value that
matches the sub-pattern.

An example:

```racket
(define (first-char s) (string-ref s 0))

(define (starts-with-a? s)
  (match s
    [(app first-char #\a) #t]
	[_ #f]))

(starts-with-a? "abc") ;=> #t
```

The function named in the `app` pattern should be a function of one argument,
otherwise an error should be signalled when matching.

The AST types and parser have been updated to recognize this new form of
patterns and a stub has been added to the interpreter, which you must complete.
The function `interp-match-pat` has also been extended to take a list of
function definitions, which will be needed in implementing the `App` case.


## Notes

  * We have added a few tests at the top of `test/define-tests.rkt`. We recommend
    writing tests of your own there.

    - Run `raco test test/run-interp-tests.rkt` to test the interpreter.
