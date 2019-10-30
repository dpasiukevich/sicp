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

(define (invert-unit-series series)
  (define inverted-unit-series
    (cons-stream 1 (scale-stream (mul-streams (stream-cdr series)
                                              inverted-unit-series)
                                 -1)))
  inverted-unit-series)

(define (div-series s1 s2)
  (cond ((eq? 0 (stream-car s2)) (error "constant term of s2 can't be 0!"))
        (else (mul-series s1 (invert-unit-series s2)))))

(define check-sin-cos ; cos(x)^2 + sin(x)^2 = 1
  (accumulate-stream (add-streams (mul-series cosine-series cosine-series)
                                  (mul-series sine-series sine-series))))

(define (accumulate-stream stream)
  (cons-stream 0 (add-streams stream (accumulate-stream stream))))

(define (pi-summands n)
  (display n) (newline)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define (pi-summands1 n)
  (display n) (newline)
  (define twos (cons-stream 2 twos))
  (define stream (cons-stream 1 (add-streams twos stream)))
  (stream-map (lambda (el) (/ -1.0 el)) stream))

(define twos (cons-stream 2 twos))
(define lul
  (cons-stream 1
               (add-streams twos lul)))

(stream-head (pi-summands1 1) 10)
(stream-head (pi-summands 1) 10)

(define pi-stream
  (stream-scale (partial-sums (pi-summands 1)) 4))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2.0))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

(define guesses (cons-stream 1.0 (stream-map (lambda (guess) (sqrt-improve guess 190)) guesses)))

(define (sqrt-stream-ineffective x)
  (cons-stream 1.0 (stream-map
                     (lambda (guess)
                       (sqrt-improve guess x))
                     (sqrt-stream-ineffective x))))

(stream-head (sqrt-stream 180) 10)
(stream-head (sqrt-stream-ineffective 180) 10)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn−1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (stream-limit stream limit)
  (define (iter s1 s2)
    (if (< (abs (- (stream-car s1) (stream-car s2))) limit)
        (stream-car s2)
        (iter (stream-cdr s1) (stream-cdr s2))))
  (iter stream (stream-cdr stream)))

(define (sqrt-with-tolerance x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define alternating-ones (cons-stream 1.0 (cons-stream -1.0 alternating-ones)))
(define ln2 (partial-sums (stream-map / alternating-ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (weighted-pairs weight s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted weight
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

(define (triples S T U)
  (cons-stream
    (list (stream-car S) (stream-car T) (stream-car U))
    (interleave
      (stream-map (lambda (tu-pair) (cons (stream-car S) tu-pair)) (pairs T (stream-cdr U)))
      (triples (stream-cdr S) (stream-cdr T) (stream-cdr U))) ))

(define first-of-integer-pair
  (stream-map car (pairs integers integers)))

(stream-head (triples-slow integers integers integers) 10)

(define (pythagorean? a b c) (= (+ (square a) (square b)) (square c)))

(define pythagorean-triples
  (stream-filter (lambda (triple) (apply pythagorean? triple)) (triples integers integers integers)))

(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (w1 (weight (stream-car s1)))
                (w2 (weight (stream-car s2))))
            (cond ((<= w1 w2)
                   (cons-stream
                     s1car
                     (merge-weighted weight (stream-cdr s1) s2)))
                  ((> w1 w2)
                   (cons-stream
                     s2car
                     (merge-weighted weight s1 (stream-cdr s2)))))))))

(define pairs-ordered-by-sum
  (weighted-pairs (lambda (P) (+ (car P) (cadr P))) integers integers))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (stream-scale integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (inputs v0)
    (define int
      (cons-stream
        v0
        (add-streams (stream-scale inputs R)
                     (integral (stream-scale inputs (/ 1.0 C)) (stream-car int) dt))))
    int))

(define RC1 (RC 5 1 0.5))
(stream-head (RC1 integers 0) 10)

(define (zero-crossings sense-data)
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

