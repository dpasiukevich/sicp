(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (stream-enumerate-infinite start step)
  (cons-stream start
               (stream-enumerate-infinite (+ start step) step)))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define (stream-scale stream scale)
  (stream-map (lambda (el) (* el scale)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream
                     s1car
                     (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream
                     s2car
                     (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1)
                             (stream-cdr s2)))))))))

(define ones
  (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))
(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs) fibs))))
(define (partial-sums stream)
  (add-streams stream (cons-stream 0 (partial-sums stream))))

(define S (cons-stream 1
                       (merge (stream-scale S 2)
                              (merge (stream-scale S 3)
                                     (stream-scale S 5)))))

(stream-head S 10)

(stream-ref fibs 1000000000)

(define (fib n)
  (define (iter a b n)
    (if (= n 0)
        a
        (iter b (+ a b) (- n 1))))
  (iter 0 1 n))

;(fib 300000) ; O(1) space, ez result
;(stream-ref fibs 300000) ; huita: ABORTING: OUT OF MEMORY BECAUSE OF MEMOIZING ALL PREVIOUS PROMISES, hence O(n) space

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define f (expand 1 7 10))
(define f1 (expand 3 8 10))

(define (integrate-series s) (stream-map / s integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series (cons-stream 1 (stream-scale (integrate-series sine-series) -1.0)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams (add-streams (stream-scale (stream-cdr s1) (stream-car s2))
                              (stream-scale (stream-cdr s2) (stream-car s1)))
                 (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define check-sin-cos ; cos(x)^2 + sin(x)^2 = 1
  (accumulate-stream (add-streams (mul-series cosine-series cosine-series)
                                  (mul-series sine-series sine-series))))

(define (accumulate-stream stream)
  (cons-stream 0 (add-streams stream (accumulate-stream stream))))

(define lul (accumulate-stream integers))

(stream-head check-sin-cos 10)
(stream-head sine-series 10)
