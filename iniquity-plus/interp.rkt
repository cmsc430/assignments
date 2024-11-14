#lang racket
(provide interp)
(provide interp-env)
(require "ast.rkt")
(require "interp-prim.rkt")

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

;; type Env = (Listof (List Id Value))
;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (interp-env e '() ds)]))
;; Expr Env Defns -> Answer
(define (interp-env e r ds)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r ds)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (match (interp-env e3 r ds)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
    [(If e0 e1 e2)
     (match (interp-env e0 r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v    (interp-env e2 r ds)])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v (interp-env e2 (ext r x v) ds)])]
    [(App f es)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs
        (match (defns-lookup ds f)
          [(Defn _ fun)
           (apply-fun fun vs ds)])])]
    [(Apply f es e)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs
        (match (interp-env e r ds)
          ['err 'err]
          [ws
           (if (list? ws)
               (match (defns-lookup ds f)
                 [(Defn _ fun)
                  (apply-fun fun (append vs ws) ds)])
               'err)])])]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (match (interp-env* es r ds)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Fun [Listof Values] Defns -> Answer
(define (apply-fun f vs ds)
  (match f
    [(FunPlain xs e)
     ; check arity matches-arity-exactly?
     (if (= (length xs) (length vs))
         (interp-env e (zip xs vs) ds)
         'err)]
    [(FunRest xs x e)
     ; check arity is acceptable
     (if (< (length vs) (length xs))
         'err
           (interp-env e
                       (zip (cons x xs)
                            (cons (drop vs (length xs))
                                  (take vs (length xs))))
                       ds))]
    [(FunCase cs)
     (match (select-case-lambda cs (length vs))
       ['err 'err]
       [f (apply-fun f vs ds)])]))

;; [Listof FunCaseClause] Nat -> Fun | 'err
(define (select-case-lambda cs n)
  (match cs
    ['() 'err]
    [(cons (and (FunPlain xs e) f) cs)
     (if (= (length xs) n)
         f
         (select-case-lambda cs n))]
    [(cons (and (FunRest xs x e) f) cs)
     (if (<= (length xs) n)
         f
         (select-case-lambda cs n))]))

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))
