(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display-stream seq)
(define y (stream-filter even? seq))
(display-stream y)
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(display-stream z)
(stream-ref y 7)
(display-stream z)
(display-stream seq)
(display-stream y)