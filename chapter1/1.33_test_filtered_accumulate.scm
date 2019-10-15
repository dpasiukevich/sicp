(load "prime-test.scm")
(load "procedures_as_arguments.scm")

(define (sum-squares-of-primes a b)
  (define (prime? n) (fast-prime? n 15))
  (filtered-accumulate prime? + 0 identity a inc b))

(define (func-with-nice-filter n)
  (define (filter-func i) (= (gcd i n) 1))
  (filtered-accumulate filter-func * 1 identity 1 inc n))

(func-with-nice-filter 20)
