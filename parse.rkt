#lang racket
(provide parse)
(require "ast.rkt")

(define (well-formed? e)
  ;; TODO: Replace with your definition
  (well-formed?-e e '())
  )

(define (well-formed?-e e r)
	(match e
	       [(Var v) (member v r)]
	       [(Let bs e) (and (well-formed?-bs bs r '()) (well-formed?-e e (append (reverse (get-vars bs)) r)))]
	       [(Let* bs e) (and (well-formed?-bs* bs r) (well-formed?-e e (append (reverse (get-vars bs)) r)))]
	       [(Prim1 _ e) (well-formed?-e e r)]
	       [(Begin e1 e2) (and (well-formed?-e e1 r) (well-formed?-e e2 r))]
	       [(Prim2 _ e1 e2) (and (well-formed?-e e1 r) (well-formed?-e e2 r))]
	       [(PrimVar '+ es) (andmap (lambda (x) (well-formed?-e x r)) es)]
	       [(If e1 e2 e3) (and (well-formed?-e e1 r) (well-formed?-e e2 r) (well-formed?-e e3 r))]
	       [(Cond cs e) (and (well-formed?-cs cs r) (well-formed?-e e r))]
	       [_ #t]))


(define (well-formed?-bs bs r defined)
	(match bs
	       ['() #t]
	       [(cons (Binding v e) res) (and (not (member v defined)) (well-formed?-e e r) (well-formed?-bs res r (cons v defined)))]))

(define (well-formed?-bs* bs r)
	(match bs
	       ['() #t]
	       [(cons (Binding v e) res) (and (well-formed?-e e r) (well-formed?-bs* res (cons v r)))]))

(define (well-formed?-cs cs r)
	(match cs
	       ['() #t]
	       [(cons (Clause p e) res) (and (well-formed?-e p r) (well-formed?-e e r) (well-formed?-cs res r))]))

;; We move parsing to a helper function to allow for calling well-formed?
;; S-Expr -> Expr
(define (parse s)
  (let ((e (parse-aux s)))
    (if (well-formed? e)
        e
        (IllFormedError))))

(define (parse-aux s)
    (cond
      [(integer? s) (Int s)]
      [(boolean? s) (Bool s)]
      [(char? s)    (Char s)]
      [(symbol? s)  (match s
                      ['eof (Eof)]
                      [_ (Var s)])]
      [else
       (match s
         [(list 'read-byte)       (Prim0 'read-byte)]
         [(list 'peek-byte)       (Prim0 'peek-byte)]
         [(list 'void)            (Prim0 'void)]
         [(list 'add1 e)          (Prim1 'add1 (parse-aux e))]
         [(list 'sub1 e)          (Prim1 'sub1 (parse-aux e))]
         [(list 'zero? e)         (Prim1 'zero? (parse-aux e))]
         ;; Implemented for you: abs, -, not
         [(list 'abs e)  (Prim1 'abs (parse-aux e))]
         [(list '- e)    (Prim1 '- (parse-aux e))]
         [(list 'not e)  (Prim1 'not (parse-aux e))]
         [(list 'char? e)         (Prim1 'char? (parse-aux e))]
         ;; TODO: integer? / boolean?
	 [(list 'integer? e) (Prim1 'integer? (parse-aux e))]
	 [(list 'boolean? e) (Prim1 'boolean? (parse-aux e))]
         [(list 'write-byte e)    (Prim1 'write-byte (parse-aux e))]
         [(list 'eof-object? e)   (Prim1 'eof-object? (parse-aux e))]
         [(list 'integer->char e) (Prim1 'integer->char (parse-aux e))]
         [(list 'char->integer e) (Prim1 'char->integer (parse-aux e))]
         [(list 'begin e1 e2)     (Begin (parse-aux e1) (parse-aux e2))]
         [(list '+ e1 e2)         (Prim2 '+ (parse-aux e1) (parse-aux e2))]
         ;; TODO: Variadic +
         [(cons '+ es)            (PrimVar '+ (map parse-aux es))]
         [(list '- e1 e2)         (Prim2 '- (parse-aux e1) (parse-aux e2))]       
         [(list 'if e1 e2 e3)
          (If (parse-aux e1) (parse-aux e2) (parse-aux e3))]
         ;; TODO: Let and Let* with multiple bindings
	 [(list 'let bs e) (Let (parse-bindings bs) (parse-aux e))]
	 [(list 'let* bs e) (Let* (parse-bindings bs) (parse-aux e))]
         ;; Implemented for you: cond
         [(cons 'cond clauses)
          (let ((res (parse-cond clauses)))
            (Cond (car res) (cdr res)))]
         [_ (error "Parse error" s)])]
      ))

(define (parse-bindings bs)
	(match bs
	       ['() '()]
	       [(cons (list v e) res) (cons (Binding v (parse-aux e)) (parse-bindings res))]))

;; Implemented for you: cond
(define (parse-cond cs)
  (match cs
    [(list (list 'else e)) (cons '() (parse-aux e))]
    [(cons (list p e) css)
     (let ((res (parse-cond css)))
       (cons (cons (Clause (parse-aux p) (parse-aux e)) (car res)) (cdr res)))]
    ))
