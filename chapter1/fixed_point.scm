(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
	(cond ((positive? test-value) (search f neg-point midpoint))
	      ((negative? test-value) (search f midpoint pos-point))
	      (else midpoint)
	      )
	)
      )
    )
  )

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (average x y) (/ (+ x y) 2.0))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b))
	)
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else (error "Values are not of opposite sign" a b)))
    )
  )

(define tolerance 0.00001)

(define (iterative-improve good-enough? improve)
  (define (check x)
    (if (good-enough? x)
      x
      (check (improve x))))
  (lambda (x) (check x)))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve (lambda (x) (close-enough? x (f x)))
		      f)
   first-guess))

;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
;  (define (try guess)
;    ;(display guess) (newline)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;	next
;	(try next))))
;  (try first-guess)
;  )

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
