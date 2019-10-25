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

(load "dispatcher.scm")
(load "numbers-scheme.scm")
(load "numbers-rational.scm")
(load "numbers-real.scm")
(load "numbers-complex.scm")
(load "polynomials.scm")
(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-polynomial-package)

; DEFINE GENERIC METHODS FOR ARITHMETIC SYSTEM
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (sin-num x) (apply-generic 'sin-num x))
(define (cos-num x) (apply-generic 'cos-num x))
(define (atan-num x) (apply-generic 'atan-num x))
(define (square-num x) (apply-generic 'square-num x))
(define (sqrt-num x) (apply-generic 'sqrt-num x))

(define (negate n) (apply-generic 'negate n))
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

; test polynomials
(define p1 (make-dense-polynomial 'x (list 3 (make-complex-from-real-imag 2 3) 7)))
(define p2 (make-sparse-polynomial 'x (list (list 4 1)
                                            (list 2 (make-rational 2 3))
                                            (list 0 (make-complex-from-real-imag 5 3)))))

p1
p2

(=zero? p1)
(=zero? p2)
(negate p1)
(negate p2)
(add p1 p2)
(add p2 p1)
(sub p1 p2)
(sub p2 p1)
(mul p1 p2)
(mul p2 p1)

(div p1 p2) ; returns list: res + remainder

(mul p1 (car (div p1 p2))) ; res of division is empty list, testing mul of poly on empty

(define p3 (make-sparse-polynomial 'x (list (list 4 1)
                                            (list 0 -1))))
(define p4 (make-dense-polynomial 'x (list 1 0 -1)))

p1
p2
(div p3 p4)

; test complex
(define c1 (make-complex-from-real-imag 3 3))
(define c2 (make-complex-from-mag-ang 3 30))
(add c1 c2)
(sub c1 c2)
(mul c1 c2)
(div c1 c2)
(equ? c1 c2)
(=zero? c2)
(negate c1)

; test scheme number
(define n1 (make-scheme-number 10))
(define n2 (make-scheme-number 22))
(add n1 n2)
(sub n1 n2)
(mul n1 n2)
(div n1 n2)
(equ? n1 n2)
(=zero? n1)
(sin-num n1)
(cos-num n1)
(atan-num n1)
(square-num n1)
(sqrt-num n1)
(negate n1)

; test real
(define r1 (make-real 0.5))
(define r2 (make-real 0.7))
(add r1 r2)
(sub r1 r2)
(mul r1 r2)
(div r1 r2)
(equ? r1 r2)
(=zero? r1)
(sin-num r1)
(cos-num r1)
(atan-num r1)
(square-num r1)
(sqrt-num r1)
(negate r1)

; test rational
(define rat1 (make-rational 4 3))
(define rat2 (make-rational 5 8))
(add rat1 rat2)
(sub rat1 rat2)
(mul rat1 rat2)
(div rat1 rat2)
(equ? rat1 rat2)
(=zero? rat1)
(sin-num rat1)
(cos-num rat1)
(atan-num rat1)
(square-num rat1)
(sqrt-num rat1)
(negate rat1)

; test type coercion
(add n1 n1)
(sub n1 c1)
(mul n1 c1)
(div n1 c1)
(equ? n1 c1)
