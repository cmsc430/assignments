#lang racket
(provide interp interp-e)
(require "../syntax/ast.rkt")
(require "interp-prim.rkt")
(require "env.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (string Character ...)
;; | (vector Value ...)

;; type Answer = Value | 'err

;; type Env = (Listof (List Id Value))

(define (err? x) (eq? x 'err))
;; ClosedExpr -> Answer
;; Prog -> Answer
(define (interp p)
  (with-handlers ([err? identity])
    (match p
      [(Prog ds e)
       (interp-e e '() ds)])))
;l Expr Env Defns -> Value { raises 'err }
(define (interp-e e r ds) ;; where r closes e
  (match e
    [(Var x) (lookup r x)]
    [(Lit d) d]
    [(Eof)   eof]
    [(Prim0 p)
     (interp-prim0 p)]
    [(Prim1 p e)
     (interp-prim1 p (interp-e e r ds))]
    [(Prim2 p e1 e2)
     (interp-prim2 p
                   (interp-e e1 r ds)
                   (interp-e e2 r ds))]
    [(Prim3 p e1 e2 e3)
     (interp-prim3 p
                   (interp-e e1 r ds)
                   (interp-e e2 r ds)
                   (interp-e e3 r ds))]
    [(If e1 e2 e3)
     (if (interp-e e1 r ds)
         (interp-e e2 r ds)
         (interp-e e3 r ds))]
    [(Begin e1 e2)
     (begin (interp-e e1 r ds)
            (interp-e e2 r ds))]
    [(Let x e1 e2)
     (let ((v (interp-e e1 r ds)))
       (interp-e e2 (ext r x v) ds))]
    [(App f es)
     (let ((vs (interp-e* es r ds)))
        (match (defns-lookup ds f)
          [(Defn f xs ys es e)
           ; check arity matches
           (if (or (< (length (append xs ys)) (length vs))
                   (< (length vs) (length xs)))
               (raise 'err)
               (let ((as (interp-e* (drop es (- (length vs) (length xs))) r ds)))
                 (interp-e e (zip (append xs ys) (append vs as)) ds)))]))]))

;; (Listof Expr) REnv Defns -> (Listof Value) { raises 'err }
(define (interp-e* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (cons (interp-e e r ds)
           (interp-e* es r ds))]))

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _ _ _) (eq? f g)])
         ds))
