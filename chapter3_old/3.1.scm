(define (make-accumulator n)
  (let ((value n))
    (lambda (x) (begin (set! value (+ value x))
                       value))))

(define M (make-accumulator 15))
(M 3)
(M 4)