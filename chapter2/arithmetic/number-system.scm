; INIT GLOBAL STATE TABLE FOR DISPATCHING
(define t (make-equal-hash-table))
(define (put method division-id value)
  (hash-table-set! t (list method division-id) value))
(define (get method division-id)
  (hash-table-ref t (list method division-id) (lambda () #f)))
(define coercion-table (make-equal-hash-table))
(define (get-coercion type1 type2)
  (hash-table-ref coercion-table (list type1 type2) (lambda () #f)))
(define (put-coercion type1 type2 func)
  (hash-table-set! coercion-table (list type1 type2) func))

(define (simple->complex n) (make-complex-from-real-imag n 0))
(put-coercion 'scheme-number 'complex simple->complex)

(load "dispatcher.scm")
(load "numbers-scheme.scm")
(load "numbers-rational.scm")
(load "numbers-real.scm")
(load "numbers-complex.scm")
(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-complex-package)

; DEFINE GENERIC METHODS FOR ARITHMETIC SYSTEM
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ? n1 n2) (apply-generic 'equ? n1 n2))
(define (=zero? n) (apply-generic '=zero? n))
(define (raise-num n) (apply-generic 'raise-num n))

(define (drop x)
  (let ((project-proc (get 'project (type-tag x))))
   (if project-proc
       (let ((project-number (project-proc (contents x))))
        (if (equ? x (raise-num project-number))
            (drop project-number)
            x))
       x))) 

; using package

(define c1 (make-complex-from-real-imag 3 3))
(define c2 (make-complex-from-mag-ang 3 30))

c1
c2

(add c1 c2)

(equ? c1 c2)

(=zero? c2)

(define n1 (make-scheme-number 10))
(define n2 (make-scheme-number 22))

(add n1 n2)

(equ? n1 n2)

(add n2 c1)

(=zero? n1)

(define r1 (make-real 0.5))
(define r2 (make-real 0.7))

(add r1 r2)

(add n1 r1)

(add r1 c1)
