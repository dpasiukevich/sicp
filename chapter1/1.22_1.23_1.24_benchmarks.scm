(load "prime-test.scm")

(define (timed-prime-test func func-name num)
  (define (start-test start-time)
    (if (func num) (display-report (- (runtime) start-time)))
    )
  (define (display-report elapsed-time)
    (display func-name) (display " taken: ") (display elapsed-time) (newline)
    )
  (start-test (runtime))
  )

;(timed-prime-test prime? "prime?" 9880469)
;(timed-prime-test prime-unoptimized? "prime-unoptimized?" 9880470)

(define (fast-prime-test times)
  (define (wrapped num)
    (fast-prime? num times)
    )
  wrapped
  )
;(timed-prime-test (fast-prime-test 5) "fast-prime?" 9880469)
