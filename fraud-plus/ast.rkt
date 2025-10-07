#lang racket
(provide Lit Prim0 Prim1 Prim2 If
         Eof Begin
         Let Let* Var
         Cond Case)
;; type Expr = (Lit Datum)
;;           | (Eof)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)
;;           | (If Expr Expr Expr)
;;           | (Cond [Listof Expr] [Listof Expr] Expr)
;;           | (Case Expr [Listof [Listof Datum]] [Listof Expr] Expr)
;;           | (Begin Expr Expr)
;;           | (Let  [Listof Id] [Listof Expr] Expr)
;;           | (Let* [Listof Id] [Listof Expr] Expr)
;;           | (Var Id)

;; type ClosedExpr = { e ∈ Expr | e contains no free variables }

;; type Id  = Symbol
;; type Datum = Integer
;;            | Boolean
;;            | Character
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1
;;          | 'zero?
;;          | 'abs | '- | 'not
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'integer? | 'boolean?
;;          | 'write-byte | 'eof-object?
;; type Op2 = '+ | '- | '< | '=

(struct Eof () #:prefab)
(struct Lit (d) #:prefab)
(struct Prim0 (p) #:prefab)
(struct Prim1 (p e) #:prefab)
(struct Prim2 (p e1 e2)  #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Begin (e1 e2) #:prefab)
(struct Let (xs es e) #:prefab)
(struct Let* (xs es e) #:prefab)
(struct Cond (cs es el) #:prefab)
(struct Case (e ds es el) #:prefab)
(struct Var (x) #:prefab)

