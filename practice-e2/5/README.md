# CMSC 430 PRACTICE Exam 2, Part 5


## Instructions

Many programming languages give programmers the ability to define functions that
have some parameters that can take on default values when not supplied by the
function's caller. Racket in fact provides such a mechanism.

So for example, this function is defined with a default value for the parameter
`y`:

```racket
(define (f x [y 0]) (+ x y))
```

If called with one argument, e.g. `(f 5)` then `x` is bound to `5` as usual, but
`y` is bound to the default value `0` in the body of the function.

If called with two arguments, e.g. `(f 5 3)` then `x` is bound `5` and `y` is
bound `3`.

If called with any other number of arguments, it results in an error.

This generalizes to any number of parameters with default values, for example:

```racket
(define (f x y z [p e1] [q e2] [r e3]) ...)
```

This function takes between 3 and 6 arguments. If given 6 arguments, each is
bound to the parameters as usual. If given 5 arguments, they are bound to the
first 5 parameters, but the last (`r`) is bound to the value of `e3`. If given 4
arguments, `q` is bound to the value of `e2` and `r` is bound to the value of
`e3`, and so on. Called with any number of arguments less than 3 or more than 6
is an error.

There are a couple things to note:

 * a function may have no required parameters,

 * a function may have no default parameters,

 * a function may have neither required or default parameters,

 * all default parameters must come *after* the required parameters, and

 * the default parameters are given by arbitrary expressions; these expressions
   are evaluated every time the function is called without a corresponding
   argument.

So for example:

```racket
(define (f x [y (read-byte)] [z (read-byte)]) ...)
```

When called with `(f 1 2 3)` will not read any bytes. When called with `(f 1
2)`, one byte is read and bound it to `z`. When called with `(f 1)`, one byte is
read and bound to `y`, then read another byte is read and bound to `z`, *in that
order*.

You're given a modified version of the Iniquity language we studied in class.
Its parser and AST have been extended to accomodate function definitions with
default parameters. In particular, the AST type for function definitions has
been updated to:

```racket
;; type Defn = (Defn Id [Listof Id] [Listof Id] [Listof Expr] Expr)
(struct Defn (f xs ys es e) #:prefab)
```

Here `xs` represents the list of required parameter names, `ys` is the list of
default parameter names, while `es` is the list of default parameter
expressions. The length of `ys` and `es` is guaranteed to be the same. The
expression `e` is the body of the function, for which `xs` and `ys` are in
scope.

So for example, a function like:

```racket
(define (f x y) ...)
```

is represented as:

```racket
(Defn 'f '(x y) '() '() ...)
```

A function like:

```racket
(define (f [x (read-byte)] [y (read-byte)]) ...)
```

is represented by:

```racket
(Defn 'f '() '(x y) (list (Prim0 'read-byte) (Prim0 'read-byte)) ...)
```

And finally, a function like:

```racket
(define (f x [y (read-byte)] [z (read-byte)]) ...)
```

is represented by:

```racket
(Defn 'f '(x) '(y z) (list (Prim0 'read-byte) (Prim0 'read-byte)) ...)
```

In addition to the parser and AST, the interpreter has been updated to implement
this new form of function definition and provides a specification for the
compiler.

Your job is to complete the implementation of the compiler. The provided
compiler works for functions calls that provide arguments for *all* parameters,
but doesn't handle the case that some arguments are left off and should be given
their default values.


## Sketch

In lecture, we discussed how arity checking can be done at run-time
and the provided compiler does implement this check. The idea is
simple: we use a designated register to communicate the number of
arguments given by the caller to the body of the function being
called. Since the function knows how many parameters it expects, it
can include code to check the number of arguments supplied and signal
an error if there's a mismatch.

The code provided in this problem treats calls the same: it writes the number of
arguments into a register.

The compilation of function definitions is slightly different. It checks if the
number of arguments is equal to the total number of parameters, and if so, jumps
down to the code for the body. Otherwise it signals an error (note: this uses
the revised definition of Defn):

```racket
;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs ys es e)
     (let ((body (gensym 'body)))
       (seq (Label (symbol->label f))
            ;; Check if all parameters were given
            (Cmp 'r8 (+ (length xs) (length ys)))
            (Je body)

            ;; TODO: based on the number of arguments given,
            ;; jump below to appropriate place to execute and bind
            ;; default parameters

            (Jmp 'err) ;; an arity error has occured

            ;; TODO: code for executing and binding default parameters

            (Label body)
            (compile-e e (reverse (append xs ys)))
            (Add rsp (* 8 (+ (length xs) (length ys)))) ; pop args
            (Ret)))]))
```

Your job is to revise the part marked `TODO` so the generated code executes the
code for default arguments and binds the values appropriately based on the
actually number of arguments supplied by the caller of the function.


## Notes

  * You may assume each default argument expression is *closed*. (Racket allows
    default expressions to refer to earlier parameters, but you do not need to
    support this aspect of the feature.)

  * If you're not sure of what your compiler should do, remember to try examples
    in the interpreter.

  * A few very simple tests have been added to `test/define-tests.rkt`, which
    will all pass for the interpreter but fail for the compiler.

    - Run `raco test test/run-compile-tests.rkt` to test the compiler.
    - Run `raco test test/run-interp-tests.rkt` to test the interpreter.
    - Run `raco test test/` to run all of the tests.

    These tests are **not** comprehensive. Write your own tests to check your
    work!

  * You do not need to write tests, but we highly encourage it!

  * You do not need to solve any other problem in order to do this one; they are
    completely independent.
