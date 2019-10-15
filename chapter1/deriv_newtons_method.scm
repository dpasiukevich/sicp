(load "fixed_point.scm")

(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newtons-transform g)
  (lambda (x)
    (- x 
       (/ (g x) 
	  ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))

(define (cubic a b c)
  (fixed-point-of-transform 
    (lambda (x) (+ (* x x x)
		   (* a x x)
		   (* b x)
		   c))
    newton-transform
    1.0
    )
  )

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 0) identity)
	((even? n)  (repeated (compose f f) (/ n 2)))
	(else (compose f (repeated f (- n 1)))))) 

(define (identity x) x)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx))
		    )
		 3.0)))

(define (n-fold f n) ((repeated smooth n) f))

(define (get-max-pow n)
  (define (iter p r)
    (if (< (- n r) 0)
      (- p 1)
      (iter (+ p 1) (* r 2))))
  (iter 1 2))

(define (pow b p)
  (define (square x) (* x x))
  (define (iter res a n)
    (if (= n 0)
      res
      (if (even? n)
	(iter res (square a) (/ n 2))
	(iter (* res a) a (- n 1)))))
  (iter 1 b p))

(define (n-root n x)
  (fixed-point ((repeated average-damp (get-max-pow n))
		(lambda (y) (/ x (pow y (- n 1)))))
	       1.0))

(define (iterative-improve good-enough? improve)
  (define (check x)
    (if (good-enough? x)
      x
      (check (improve x))))
  (lambda (x) (check x)))
