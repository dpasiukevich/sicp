(define x 14)

(define f (lambda () (display x)))

(define (test)
  (let ((x 322))
    (f)))

(test)