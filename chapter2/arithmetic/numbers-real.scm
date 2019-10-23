(load "numbers-complex.scm")

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real) zero?)
  (put 'raise-num '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project 'real 
       (lambda (x)
         (let ((rat (rationalize
                      (inexact->exact x) 1/100)))
           (make-rational
             (numerator rat)
             (denominator rat))))) 
  'done)

(define (make-real n)
  ((get 'make 'real) n))

