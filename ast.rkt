#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The grammar of our AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; type Expr =
;; | (Eof)
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Char Character)
;; | (Prim0 Op0)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (PrimVar OpVar (ListOf Expr))
;; | (If Expr Expr Expr)
;; | (Begin Expr Expr)
;; | (Let  (ListOf Binding) Expr)
;; | (Let* (ListOf Binding) Expr)
;; | (Var Id)
;; | (Cond Clauses (Expr))

;; type Clauses = Listof (Expr, Expr)
;; type Id  = Symbol
;; type Op0 = 'read-byte
;; type Op1 = 'add1 | 'sub1 | 'zero? | 'abs | '- | 'not
;;          | 'char? | 'integer? | 'boolean?
;;          | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
;; type Op2 = '+ | '-
;; type OpVar = '+ 
;; type Binding = (Id, Expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The AST data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The AST can be viewed as having 'kinds' of nodes.
;;
;; * The nodes that represent an expression themselves
;;
;; * The nodes that are part of an expression, but no an expression themselves

;; The below are the former:

(struct Eof   ()         #:prefab)
(struct Int   (i)        #:prefab)
(struct Bool  (b)        #:prefab)
(struct Char  (c)        #:prefab)
(struct Prim0 (p)        #:prefab)
(struct Prim1 (p e)      #:prefab)
(struct Prim2 (p e1 e2)  #:prefab)
(struct PrimVar (p es)   #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct Begin (e1 e2)    #:prefab)
(struct Let   (bs e)     #:prefab)
(struct Let*  (bs e)     #:prefab)
(struct Var   (x)        #:prefab)
(struct Cond  (cs e)     #:prefab)

;; The next two are the latter:

;; A clause takes an _arbitrary_ expression and a body.
(struct Clause  (e body) #:prefab)

;; A binding holds a symbol representing the bound variable and
;; Expr that represents the value that will be bound to that variable
(struct Binding (v e) #:prefab)

;; Finally, here's a struct to return when an expression is ill-formed.
(struct IllFormedError () #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (getters)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It will sometimes be useful to get the list of all the variables that are
;; introduced by a `let`
;; [Binding] -> [Symbol]
(define (get-vars bs)
  (match bs
    ['() '()]
    [(cons (Binding v _) bs) (cons v (get-vars bs))]))

;; Get all of the _definitions_ from a list of bindings
;; [Binding] -> [Expr]
(define (get-defs bs)
  (match bs
    ['() '()]
    [(cons (Binding _ def) bs) (cons def (get-defs bs))]))


;; Get all of the predicate expressions from a list of clauses
;; [Clause] -> [Expr]
(define (get-preds cs)
  (match cs
    ['() '()]
    [(cons (Clause p _) cs) (cons p (get-preds cs))]))

;; Get all of the bodies expressions from a list of clauses
;; [Clause] -> [Expr]
(define (get-bods cs)
  (match cs
    ['() '()]
    [(cons (Clause _ b) cs) (cons b (get-bods cs))]))

