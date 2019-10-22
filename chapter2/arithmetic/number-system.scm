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
(load "numbers-imaginary.scm")
(load "numbers-rational.scm")
(install-complex-package)
(install-scheme-number-package)
(install-rational-package)

(add n2 c1)

(add c1 n2)

; DEFINE GENERIC METHODS FOR ARITHMETIC SYSTEM
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ? n1 n2) (apply-generic 'equ n1 n2))
(define (=zero? n) (apply-generic '=zero? n))


; using package

(define c1 (make-complex-from-real-imag 3 3))

(define c2 (make-complex-from-mag-ang 3 30))

c1

c2

(magnitude c1)

(add c1 c2)

(equ? c1 c2)

(=zero? c1)

(define n1 (make-scheme-number 0))

(define n2 (make-scheme-number 22))

(add n1 n2)

(equ? n1 n2)

(add n1 c1)

n1

(=zero? n1)
