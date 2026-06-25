#lang racket
(require "ast.rkt")
(provide inline)

;; Defn Expr -> Expr
;; Inline all calls to given function in given expression
(define (inline d e) e)

(module+ test
  (require rackunit)
  (require "parse.rkt")
  (check-equal?
   (inline (parse-define '(define (f x) x))
           (parse-e '(f 5)))
   (parse-e '(let ((x 5)) x)))

  (check-equal?
   (inline (parse-define '(define (f x) x))
           (parse-e '(f (f 5))))
   (parse-e '(let ((x (let ((x 5)) x))) x)))

  (check-equal?
   (inline (parse-define '(define (f x) x))
           (parse-e '(let ((z (f 5))) z)))
   (parse-e '(let ((z (let ((x 5)) x))) z)))

  ;; Notice that the call to f within f is not inlined
  (check-equal?
   (inline (parse-define '(define (f x) (f x)))
           (parse-e '(f 5)))
   (parse-e '(let ((x 5)) (f x))))

  (check-equal?
   (inline (parse-define '(define (f x y) (+ x y)))
           (parse-e '(f 3 4)))
   (parse-e '(let ((x 3) (y 4)) (+ x y))))

  (check-equal?
   (inline (parse-define '(define (f x) x))
           (parse-e '(add1 (f 5))))
   (parse-e '(add1 (let ((x 5)) x))))

  ;; Don't inline if there's an arity error...
  (check-equal?
   (inline (parse-define '(define (f x) x))
           (parse-e '(f 3 4)))
   (parse-e '(f 3 4)))

  ;; ...but you can still inline within the arguments
  (check-equal?
   (inline (parse-define '(define (f x) x))
           (parse-e '(f 3 (f 5))))
   (parse-e '(f 3 (let ((x 5)) x)))))
