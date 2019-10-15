(define (average x y)
  (/ (+ x y) 2.0)
  )

; improve guess for cube root
(define (improve guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3)
  )

(define (sqrt-iter prev-guess guess x)
  (if (small-diff? prev-guess guess)
    guess
    (sqrt-iter guess (improve guess x) x)
    )
  )

(define (square x) (* x x))

(define (small-diff? x y)
  (<= (abs (- x y))
     (* x 0.0001)
     )
  )

(define (sqrt x)
  (sqrt-iter 2.0 1.0 x)
  )

