(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integrate-series A) 
  (stream-map / A integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))
(define sine-series
  (cons-stream 0 (scale-stream (integrate-series cosine-series) -1)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))

(define s (add-streams (mul-series sine-series sine-series)
                       (mul-series cosine-series cosine-series)))

(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)