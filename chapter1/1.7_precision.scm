(define (average x y)
  (/ (+ x y) 2.0)
  )

(define (improve guess x)
  (average guess (/ x guess))
  )

(define (sqrt-iter prev-guess guess x)
  (if (small-diff? guess x)
    guess
    (sqrt-iter guess (improve guess x) x)
    )
  )

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001)
  )

(define (small-diff? x y)
  (<= (abs (- x y))
     (* x 0.0001)
     )
  )

(define (sqrt x)
  (sqrt-iter 2.0 1.0 x)
  )

(sqrt 1023423842374029342342342)
(sqrt 0.0000000001)
