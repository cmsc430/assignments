# CMSC 430 PRACTICE Exam 2, Part 4


## Instructions

One of the most common compiler optimizations is called *function inlining*. The
basic idea is that a call to a function can be replaced by the code in the
definition of that function. This avoids the overhead of setting up a return
pointer, pushing it on the stack, jumping, and returning. It can also enable
other optimizations by moving the code closer to the arguments it operates on.

In the setting of Iniquity, inlining can be realized as a source to source
transformation that replaces AST nodes such as:

```racket
(App f (list e1 e2 e3))
```
with
```racket
(Let (list x1 x2 x3) (list e1 e2 e3) e)
```
if the function `f` had been defined as:
```racket
(Defn f (list x1 x2 x3) e)
```

You are given a modified version of the Iniquity parser and AST definition. It
is exactly like Iniquity, but adds the general form of `let` expressions (like
we did in an assignment).

Your job is to implement a function:

```racket
;; Defn Expr -> Expr
(define (inline d e) ...)
```

This function should inline every call to the function defined by `d` in `e`.

Add your code to `inline.rkt` and use the provided tests to help guide your
solution.

## Notes

  * Note that you should *not* inline within the body of the function `d`,
    otherwise recursive functions would inline forever. Also note that if
    there's an arity mismatch between the function definition and the call, you
    shouldn't inline.

  * You do not need to write tests, but we highly encourage it!


