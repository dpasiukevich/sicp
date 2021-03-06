(define (smallest-divisor n)
  (define (square x) (* x x))
  (define (divides? x y) (= (remainder y x) 0))
  (define (iter candidate)
    (cond ((> candidate n) n)
	  ((divides? candidate n) candidate)
	  (else (iter (+ candidate 1)))
	  )
    )
  (iter 2)
  )

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

