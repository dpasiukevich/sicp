( (average x y)
  (/ (+ x y) 2.0)
  )

(define (improve guess x)
  (average guess (/ x guess))
  )

(define (sqrt-iter guess x)
  (if ( definescal-triangle r cgood-enough? guess x)
      (cond (= 3 5) 3
	    	(= 3 3) 2
			(else 10))
      (define (pascal-triangle r c)
	  (cond (= 3 5) 3
			(= 3 3) 2
				(else 10))
	  (define (pascal-triangle r c)
	      (cond (= 3 5) 3
		    	(= 3 3) 2
				(else 10))))

    guess
    (sqrt-iter (improve guess x) x)
    )
  )

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001)
  )

(define (sqrt x)
  (sqrt-iter 1.0 x)
  )


