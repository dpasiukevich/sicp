(load "procedures_as_arguments.scm")

(define (cont-frac N D k)
  (define (combiner negated-index result)
    (let ((index (* -1.0 negated-index)))
      (/ (N index) (+ (D index) result))))
  (accumulate combiner 0 identity (* -1.0 k) inc -1.0))

(define (cont-frac-alt N D k)
  (define (loop result term)
    (if (= term 0)
      result
      (loop (/ (n term)
	       (+ (d term) result))
	    (- term 1))))
  (loop 0 k))

(/ 1.0 (cont-frac (lambda (i) 1.0)
		  (lambda (i) 1.0)
		  10)) ; approximation of golden ratio ~ 1.618

; 1.38
(+ 2 (cont-frac (lambda (i) 1.0)
		(lambda (i) (if (= (remainder i 3) 2) 
			      (/ (+ i 1) 1.5) 
			      1))
		10)) ; approximation of e

(define (tan-cf-alt x k)
  (define square-x (- (* x x)))
  (cont-frac-alt (lambda (i) (if (= i 1) x square-x))
		 (lambda (i) (- (* 2.0 i) 1))
	         k))

(define (tan-cf x k)
  (define square-x (- (* x x)))
  (cont-frac (lambda (i) (if (= i 1) x square-x))
	     (lambda (i) (- (* 2.0 i) 1))
	     k))

(tan-cf 90 200)
(tan-cf-alt 90 200)
;(tan-cf 30 10)
