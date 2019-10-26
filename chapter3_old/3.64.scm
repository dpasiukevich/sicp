(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2.0))

(stream-ref (sqrt-stream 1488) 5)

(define (stream-limit s t)
  (let ((el1 (stream-car s))
        (el2 (stream-car (stream-cdr s))))
    (if (< (abs (- el1 el2)) t)
      el2
      (stream-limit (stream-cdr s) t))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 1488 0.1)