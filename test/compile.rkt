#lang racket
(require "../types.rkt" "../ast.rkt" "../parse.rkt" "../compile.rkt" a86/interp rackunit)

(require (prefix-in ru: rackunit))
(define-syntax-rule
    (check-equal? c e)
    (ru:check-equal? (with-handlers ([exn:fail? identity]) c) e))
(define-syntax-rule
    (check pred c e)
    (ru:check pred (with-handlers ([exn:fail? identity]) c) e))

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(define (run e)
  (match (asm-interp (compile (parse e)))
    ['err 'err]
    [v (bits->value v)]))

;; Abscond examples
(check-equal? (run 7) 7)
(check-equal? (run -8) -8)

;; Blackmail examples
(check-equal? (run '(add1 (add1 7))) 9)
(check-equal? (run '(add1 (sub1 7))) 7)

;; Con examples
(check-equal? (run '(if (zero? 0) 1 2)) 1)
(check-equal? (run '(if (zero? 1) 1 2)) 2)
(check-equal? (run '(if (zero? -7) 1 2)) 2)
(check-equal? (run '(if (zero? 0)
                        (if (zero? 1) 1 2)
                        7))
              2)
(check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                        (if (zero? 1) 1 2)
                        7))
              7)

;; Con+ examples
(check-equal? (run '(abs 10)) 10)
(check-equal? (run '(abs -10)) 10)
(check-equal? (run '(- 10)) -10)
(check-equal? (run '(- -10)) 10)
(check-equal? (run '(- (- 10))) 10)
(check-equal? (run '(cond [else 5])) 5)
(check-equal? (run '(cond [(zero? 1) 2] [else 3])) 3)
(check-equal? (run '(cond [(zero? 0) 2] [else 3])) 2)
(check-equal? (run '(cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3])) 4)

;; Dupe examples
(check-equal? (run '#t) #t)
(check-equal? (run '#f) #f)
(check-equal? (run '(if #t 1 2)) 1)
(check-equal? (run '(if 7 1 2)) 1)
(check-equal? (run '(if #f 1 2)) 2)
(check-equal? (run '(cond [#t 1] [#f 2] [else 3])) 1)
(check-equal? (run '(cond [#f 1] [#t 2] [else 3])) 2)
(check-equal? (run '(cond [#f 1] [#f 2] [else 3])) 3)

;; Dupe+ examples
(check-equal? (run '(cond [#t 2] [else 3])) 2)
(check-equal? (run '(cond [#f 2] [else 3])) 3)
(check-equal? (run '(cond [1 2] [else 3])) 2)
(check-equal? (run '(cond [#f 2] [#t 4] [else 3])) 4)
(check-equal? (run '(cond [#t 2] [#f 4] [else 3])) 2)
(check-equal? (run '(cond [#t 2] [#f (add1 #f)] [else 3])) 2)

;; Extort examples
(check-equal? (run '(zero? #t)) 'err)
(check-equal? (run '(zero? #f)) 'err)
(check-equal? (run '(add1 #f)) 'err)
(check-equal? (run '(if (add1 #f) 1 2)) 'err)
(check-equal? (run '(if 1 (add1 #t) 2)) 'err)
(check-equal? (run '(if #f (add1 #t) 2)) 2)
(check-equal? (run '(cond [(add1 #t) 1] [else 2])) 'err)
(check-equal? (run '(cond [#t 1] [(add1 #t) 2] [else 3])) 1)
(check-equal? (run '(cond [#f 1] [(add1 #t) 2] [else 3])) 'err)

;; Extort+ examples
(check-equal? (run '(abs #f)) 'err)
(check-equal? (run '(cond [(add1 #f) 1] [else 2])) 'err)

;; Fraud examples
(check-equal? (run '(let ((x 7)) x)) 7)
(check-equal? (run '(let ((x 7)) 2)) 2)
(check-equal? (run '(let ((x 7)) (add1 x))) 8)
(check-equal? (run '(let ((x (add1 7))) x)) 8)
(check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
(check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
(check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)
(check-equal? (run '(let ((x (add1 #f))) 0)) 'err)
               
;; Fraud+ examples
(check-equal? (run '(let () 7)) 7)
(check-equal? (run '(let ((x 7) (y 8)) 2)) 2)
(check-equal? (run '(let ((x 1) (y 2)) x)) 1)
(check-equal? (run '(let ((x 1) (y 2)) y)) 2)
(check-equal? (run '(let ((x 7) (y 8)) (add1 x))) 8)
(check-equal? (run '(let ((x 7) (y 8)) (add1 y))) 9)
(check-equal? (run '(let ((x (add1 7)) (y 0)) y)) 0)
(check-equal? (run '(let ((x 7) (z 9)) (let ((y 2)) x))) 7)
(check-equal? (run '(let ((x 7) (z 9)) (let ((x 2)) x))) 2)
(check-equal? (run '(let ((x 7) (z 9)) (let ((x (add1 x)) (z z)) x))) 8)
(check-equal? (run '(let ((x (add1 #f)) (z 9)) x)) 'err)
(check-equal? (run '(let ((x 1)) (let ((y 3) (z (+ x 5))) z))) 6)
(check-equal? (run '(let ((x 1)) (let ((y 3) (z (let ((x 42)) (+ x 0)))) z))) 42)
(check-equal? (run '(let ((x 1) (y (let ((z 1)) z))) (let ((y 3) (z (+ x y))) z))) 2)

;; Multi-bind let*
(check-equal? (run '(let* () 42)) 42)
(check-equal? (run '(let* ((x 7) (y 8)) 2)) 2)
(check-equal? (run '(let* ((x 7) (y 8)) (add1 x))) 8)
(check-equal? (run '(let* ((x 7) (y 8)) (add1 y))) 9)
(check-equal? (run '(let* ((x (add1 7)) (y 0)) y)) 0)
(check-equal? (run '(let* ((x 7) (z 9)) (let ((y 2)) x))) 7)
(check-equal? (run '(let* ((x 7) (z 9)) (let ((x 2)) x))) 2)
(check-equal? (run '(let* ((x 7) (z 9)) (let ((x (add1 x)) (z z)) x))) 8)
(check-equal? (run '(let* ((x (add1 #f)) (z 9)) x)) 'err)
(check-equal? (run '(let* ((x 6) (y (add1 x))) y)) 7)
(check-equal? (run '(let* ((x 7) (x 9)) x)) 9)
(check-equal? (run '(let* ((x 1)) (let* ((y 3) (z (+ x 5))) z))) 6)
(check-equal? (run '(let* ((x 1)) (let* ((y 3) (z (let* ((x 42)) (+ x 0)))) z))) 42)
(check-equal? (run '(let* ((x 1) (y (let* ((z 1)) z))) (let* ((y 3) (z (+ x y))) z))) 4)

;; Variadic Plus
(check-equal? (run '(+)) 0)
(check-equal? (run '(+ 1 2)) 3)
(check-equal? (run '(+ 1 2 3 4 5)) 15)
(check-equal? (run '(+ 1 2 (add1 2) 4 (sub1 5))) 14)
(check-equal? (run '(+ 1 2 (sub1 #f) 4 5)) 'err)

;; Boolean?/Integer?
(check-equal? (run '(integer? #\a)) #f)
(check-equal? (run '(integer? 4)) #t)
(check-equal? (run '(integer? #f)) #f)
(check-equal? (run '(boolean? #\a)) #f)
(check-equal? (run '(boolean? 4)) #f)
(check-equal? (run '(boolean? #t)) #t)
(check-equal? (run '#\a) #\a)
(check-equal? (run '#\space) #\space)
(check-equal? (run '#\λ) #\λ)
(check-equal? (run '(char->integer #\a)) (char->integer #\a))
(check-equal? (run '(char->integer #\space)) (char->integer #\space))
(check-equal? (run '(char->integer #\λ)) (char->integer #\λ))
(check-equal? (run '(integer->char 97)) #\a)
(check-equal? (run '(integer->char 32)) #\space)
(check-equal? (run '(integer->char 955)) #\λ)
(check-equal? (run '#\嫂) #\嫂)
(check-equal? (run '(integer->char 23234)) #\嫂)
(check-equal? (run '(integer->char #f)) 'err)
(check-equal? (run '(char->integer #f)) 'err)
(check-equal? (run '(integer->char -1)) 'err)
(check-equal? (run '(integer->char #xD800)) 'err)
(check-equal? (run '(integer->char #xDFFF)) 'err)

