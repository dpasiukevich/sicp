(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define addend cadr)
(define augend caddr)

(define (put table op type func)
  (cons (list op type func) table))
(define (get table op type)
  (define (check-el iter-table)
    (if (null? iter-table)
        (error "unknown expression type -- GET" op type)
        (let ((el (car iter-table)))
          (if (and (eqv? (car el) op) (eqv? (cadr el) type))
              (caddr el)
              (check-el (cdr iter-table))))))
  (check-el table))

(define (make-sum a1 a2) (list '+ a1 a2))   
(define (make-product m1 m2) (list '* m1 m2))

(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-product exp var) 
  (make-sum (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))

(define (deriv-exponentiation expr var)
  (let ((base (base expr))
        (exponent (exponent expr)))
    (make-product exponent
                  (make-product (make-exponentiation base (make-sum exponent -1))
                                (deriv base var)))))

(put '** 'deriv deriv-exponentiation)
(put '+ 'deriv deriv-sum )
(put '* 'deriv deriv-product)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))