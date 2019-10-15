(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m)) m))
	(else
	   (remainder (* base (expmod base (- exp 1) m)) m)
	   )
	)
  )

(define (fermat-test n)
  (define (try num)
    (= (expmod num n n) num)
    )
  (try (+ 1 (random (- n 1))))
  )

(define (miller-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x square)
      (if (and (= square 1)
	       (not (= x 1)) 
	       (not (= x (- m 1))))
	0
	square)) 
    (check-nontrivial-sqrt1 x (remainder (square x) m)))
  (cond ((= exp 0) 1)
	((even? exp)  (squaremod-with-check
			(miller-rabin-expmod base (/ exp 2) m)))
	(else 
	  (remainder (* base (miller-rabin-expmod base (- exp 1) m))
		     m)))) 
  
 (define (miller-rabin-test n)
   (define (try-it a)
     (define (check-it x)
       (and (not (= x 0))  (= x 1)))
     (check-it (miller-rabin-expmod a (- n 1) n)))
   (try-it (+ 1 (random (- n 1))))) 

; probabilistic, O(logn)
(define (fast-prime? n times)
  (cond ((= times 0) true)
	((< n 2) false)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
	(else false)
	)
  )

(define (square x)  (* x x))

(define (smallest-divisor-unoptimized n)
    (define (divides? x y)  (= (remainder y x) 0))
    (define (iter candidate)
      (cond ((> candidate n) n)
	    ((divides? candidate n) candidate)
	    (else (iter (+ candidate 1)))))
    (iter 2))

(define (smallest-divisor n)
    (define (divides? x y)  (= (remainder y x) 0))
    (define (next candidate)
      (if (= candidate 2) 3 (+ candidate 2)))
    (define (iter candidate)
      (cond ((> candidate n) n)
	    ((divides? candidate n) candidate)
	    (else (iter (next candidate)))))
    (iter 2))

; deterministic O(sqrt(n))
(define (prime? n)
  (= (smallest-divisor n) n)
  )

(define (prime-unoptimized? n)
  (= (smallest-divisor-unoptimized n) n)
  )

;(prime? 127)
;(prime? 128)
;(prime-unoptimized? 127)
;(prime-unoptimized? 128)
