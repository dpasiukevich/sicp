(define (deriv expr var)
  (cond ((number? expr) 0)
	((variable? expr)
	 (if (same-variable? expr var) 1 0))
	((sum? expr)
	 (make-sum (deriv (addend expr) var)
		   (deriv (augend expr) var)))
	((product? expr)
	 (make-sum
	    (make-product (multiplier expr)
			  (deriv (multiplicand expr) var))
	    (make-product (deriv (multiplier expr) var)
			  (multiplicand expr))))
	((exponentiation? expr)
	 (let ((n (exponent expr))
	       (u (base expr)))
	   (make-product
	     (make-product n (make-exponentiation u (make-sum n -1)))
	     (deriv u var))))
	(else
	  (error "unknown expression type -- DERIV" expr))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expr num) (and (number? expr) (= expr num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (= (length s) 3) 
    (caddr s)
    (cons '+ (cons (caddr s) (cdddr s)))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1)  (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (= (length p) 3)
    (caddr p)
    (cons '* (cons (caddr p) (cdddr p)))))

(define (make-exponentiation base power) 
  (cond ((=number? power 0) 1)
	((=number? base 1) 1)
	((=number? power 1) base)
	(else (list '** base power))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

; for infix notation (only * and +)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (operator expr) (if (memq '+ expr) '+ '*))
;
;(define (unwrap expr) (if (= (length expr) 1) (car expr) expr))
;
;(define (sum? expr) (eq? (operator expr) '+))
;(define (addend s) 
;  (define (iter result sub-expr)
;    (if (eq? (car sub-expr) '+)
;      result
;      (iter (append result (list (car sub-expr))) (cdr sub-expr))))
;  (unwrap (iter () s)))
;(define (augend s) (unwrap (cdr (memq '+ s))))
;
;(define (product? expr) (eq? (operator expr) '*))
;(define (multiplier p)
;  (define (iter result sub-expr)
;    (if (eq? (car sub-expr) '*)
;      result
;      (iter (append result (list (car sub-expr))) (cdr sub-expr))))
;  (unwrap (iter () p)))
;(define (multiplicand p) (unwrap (cdr (memq '* p))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(deriv '(1 + (2 * x + (7 + 8 * x)) + 4 * x) 'x)

(deriv '(+ x 3 x y z x) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(** x 2) 'x)

(deriv '(** x (** x 2)) 'x)

(deriv '(** x 3) 'x)

(deriv '(* x y (+ x 3)) 'x)

(deriv '(+ x y (+ x 3)) 'x)


