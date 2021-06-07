#lang racket
(require "../parse.rkt" "../ast.rkt" "../interp.rkt" rackunit)

(require (prefix-in ru: rackunit))
(define-syntax-rule
    (check-equal? c e)
    (ru:check-equal? (with-handlers ([exn:fail? identity]) c) e))
(define-syntax-rule
    (check pred c e)
    (ru:check pred (with-handlers ([exn:fail? identity]) c) e))

;; Abscond examples
(check-equal? (parse 7) (Int 7))
(check-equal? (parse -8) (Int -8))

;; Blackmail examples
(check-equal? (parse '(add1 (sub1 7)))
              (Prim1 'add1 (Prim1 'sub1 (Int 7))))

;; Con examples
(check-equal? (parse '(if (zero? 0) 1 2))
              (If (Prim1 'zero? (Int 0))
                  (Int 1)
                  (Int 2)))
(check-equal? (parse '(if (zero? (if (zero? 0) 1 0))
                        (if (zero? 1) 1 2)
                        7))
              (If (Prim1 'zero?
                         (If (Prim1 'zero? (Int 0))
                             (Int 1)
                             (Int 0)))
                  (If (Prim1 'zero? (Int 1))
                      (Int 1)
                      (Int 2))
                  (Int 7)))

;; Con+ examples
(check-equal? (parse '(abs 10)) (Prim1 'abs (Int 10)))
(check-equal? (parse '(- (- 10)))
              (Prim1 '- (Prim1 '- (Int 10))))
(check-equal? (parse '(cond [else 5]))
              (Cond '() (Int 5)))
(check-equal? (parse '(cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3]))
              (Cond (list (Clause (Prim1 'zero? (Int 1))
                                  (Int 2))
                          (Clause (Prim1 'zero? (Prim1 'sub1 (Int 1)))
                                  (Int 4))
                          )
                    (Int 3)))

;; Dupe examples
(check-equal? (parse '(if #t 3 4)) (If (Bool #t) (Int 3) (Int 4)))
(check-equal? (parse '(if #f 3 4)) (If (Bool #f) (Int 3) (Int 4)))

;; Dupe+ examples
(check-equal? (parse '(cond [#t 2] [else 3]))
              (Cond (list (Clause (Bool #t) (Int 2)))
                    (Int 3)))

;; Unbound variable failures
(check-equal? (parse '(add1 x))
             (IllFormedError))
(check-equal? (parse '(if #t 3 x))
             (IllFormedError))
(check-equal? (parse '(if #t x 4))
             (IllFormedError))
(check-equal? (parse '(if x 3 4))
             (IllFormedError))
(check-equal? (parse '(begin x 42))
             (IllFormedError))
(check-equal? (parse '(zero? (sub1 (+ (not x) 7))))
             (IllFormedError))

;; Fraud examples
(check-equal? (parse '(let ((x 7)) x))
              (Let (list (Binding 'x (Int 7)))
                   (Var 'x)))
(check-equal? (parse '(let ((x 7)) 2))
              (Let (list (Binding 'x (Int 7)))
                   (Int 2)))
(check-equal? (parse '(let ((x 7)) (add1 x)))
              (Let (list (Binding 'x (Int 7)))
                   (Prim1 'add1 (Var 'x))))
(check-equal? (parse '(let ((x (add1 7))) x))
              (Let (list (Binding 'x (Prim1 'add1 (Int 7))))
                   (Var 'x)))
(check-equal? (parse '(let ((x 7)) (let ((y 2)) x)))
              (Let (list (Binding 'x (Int 7)))
                   (Let (list (Binding 'y (Int 2)))
                        (Var 'x))))
(check-equal? (parse '(let ((x 7)) (let ((x 2)) x)))
              (Let (list (Binding 'x (Int 7)))
                   (Let (list (Binding 'x (Int 2)))
                        (Var 'x))))
(check-equal? (parse '(let ((x 7)) (let ((x (add1 x))) x)))
              (Let (list (Binding 'x (Int 7)))
                   (Let (list (Binding 'x (Prim1 'add1 (Var 'x))))
                        (Var 'x))))
                       
               
;; Fraud+ examples
;; Multi-bind let
(check-equal? (parse '(let () 7))
              (Let '() (Int 7)))
(check-equal? (parse '(let ((x 7) (y 8)) 2))
              (Let (list (Binding 'x (Int 7))
                         (Binding 'y (Int 8)))
                   (Int 2)))
(check-equal? (parse '(let ((x 7) (y 8)) (add1 x)))
              (Let (list (Binding 'x (Int 7))
                         (Binding 'y (Int 8)))
                   (Prim1 'add1 (Var 'x))))
(check-equal? (parse '(let ((x 7) (y 8)) (add1 y)))
              (Let (list (Binding 'x (Int 7))
                         (Binding 'y (Int 8)))
                   (Prim1 'add1 (Var 'y))))
(check-equal? (parse '(let ((x 7) (x 8)) 2))
              (IllFormedError))
(check-equal? (parse '(let ((x 7) (y x)) 2))
              (IllFormedError))
(check-equal? (parse '(let ((x 7) (z 9)) (let ((y 2)) x)))
              (Let (list (Binding 'x (Int 7))
                         (Binding 'z (Int 9)))
                   (Let (list (Binding 'y (Int 2)))
                        (Var 'x))))

;; Multi-bind let*
(check-equal? (parse '(let* () 42))
              (Let* '() (Int 42)))
(check-equal? (parse '(let* ((x 7) (y 8)) 2))
              (Let* (list (Binding 'x (Int 7))
                          (Binding 'y (Int 8)))
                    (Int 2)))
(check-equal? (parse '(let* ((x 7) (y 8)) (add1 x)))
              (Let* (list (Binding 'x (Int 7))
                          (Binding 'y (Int 8)))
                    (Prim1 'add1 (Var 'x))))
(check-equal? (parse '(let* ((x 7) (y 8)) (sub1 y)))
              (Let* (list (Binding 'x (Int 7))
                          (Binding 'y (Int 8)))
                    (Prim1 'sub1 (Var 'y))))

(check-equal? (parse '(let* ((x 6) (y (add1 x))) y))
              (Let* (list (Binding 'x (Int 6))
                          (Binding 'y (Prim1 'add1 (Var 'x))))
                    (Var 'y)))
(check-equal? (parse '(let* ((x 7) (x 9)) x))
              (Let* (list (Binding 'x (Int 7))
                          (Binding 'x (Int 9)))
                    (Var 'x)))
(check-equal? (parse '(let* ((x 7) (x 9)) y))
              (IllFormedError))
(check-equal? (parse '(let* ((x (add1 x)) (y x)) y))
              (IllFormedError))



