(load "numbers-rational.scm")
(load "numbers-real.scm")

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number) zero?)
  (put 'raise-num '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'sin-num '(scheme-number) (lambda (x) (make-real (sin x))))
  (put 'cos-num '(scheme-number) (lambda (x) (make-real (cos x))))
  (put 'atan-num '(scheme-number) (lambda (x) (make-real (atan x))))
  (put 'square-num '(scheme-number) (lambda (x) (tag (square x))))
  (put 'sqrt-num '(scheme-number) (lambda (x) (make-real (sqrt x))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

