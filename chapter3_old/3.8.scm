(define f
  (let ((internal 0))
    (lambda (x) (set! internal x))))

(+ (f 0) (f 1))
(+ (f 1) (f 0))