#lang racket
(provide Lit Prim0 Prim1 If Cond Case
         Eof Begin)
;; type Expr = (Lit Datum)
;;           | (Eof)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (If Expr Expr Expr)
;;           | (Cond [Listof Expr] [Listof Expr] Expr)
;;           | (Case Expr [Listof [Listof Datum]] [Listof Expr] Expr)
;;           | (Begin Expr Expr)
;; type Datum = Integer
;;            | Boolean
;;            | Character
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1
;;          | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?

(struct Eof () #:prefab)
(struct Lit (d) #:prefab)
(struct Prim0 (p) #:prefab)
(struct Prim1 (p e) #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Begin (e1 e2) #:prefab)
(struct Cond (cs es el) #:prefab)
(struct Case (e ds es el) #:prefab)

