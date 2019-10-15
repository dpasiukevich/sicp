(define (average x y)
    (/ (+ x y) 2.0))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess x)
    ; will recurse as both then-clause and else-clause are evaluated before checking predicate
    (new-if (good-enough? guess x)
          guess
	  (sqrt-iter (improve guess x) x)))

(define (square x)  (* x x))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)
	)
  )

(define (sqrt x) (sqrt-iter 1.0 x))
