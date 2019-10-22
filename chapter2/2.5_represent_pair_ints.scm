; represent pair of nonnegative integers as 2^a * 3^b
(define (cons a b) (* (expt 2 a) (expt 3 b)))

(define (largest-power-of num base)
  (define (iter n result)
    (if (= (remainder n base) 0)
      (iter (/ n base) (+ result 1))
      result))
  (iter num 0))

(define (car p) (largest-power-of p 2))
(define (cdr p) (largest-power-of p 3))

(define test-pair (cons 3 12))
(car test-pair)
(cdr test-pair)

