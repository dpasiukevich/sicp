(load "1.22_1.23_1.24_benchmarks.scm")

(define (bench-test num)
  (timed-prime-test prime? "prime?" num)
  (timed-prime-test prime-unoptimized? "prime-unoptimized?" num)
  (timed-prime-test (fast-prime-test 5) "fast-prime?" num)
  )

(bench-test 9880469)
(bench-test 101030101)
