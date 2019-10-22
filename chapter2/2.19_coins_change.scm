(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else(+ (cc amount
		    (except-first-denomination coin-values))
		(cc (- amount
		       (first-denomination coin-values))
		    coin-values)))))

(define (no-more? l) (null? l))
(define (except-first-denomination l) (cdr l))
(define (first-denomination l) (car l))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(time-it cc 100 us-coins "us-coins") 
(time-it cc 100 (reverse us-coins) "reverse-us-coins") 

(time-it cc 100 uk-coins "uk-coins") 
(time-it cc 100 (reverse uk-coins) "reverse-uk-coins") 

(define (time-it func amount coins annotation)
  (let ((start (runtime)))
    (func amount coins)
    (display annotation) (display " ")
    (display (- (runtime) start))))
