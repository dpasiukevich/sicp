; f(n) = n if n < 3 else f(n-1) + 2f(n-2) + 3f(n-3)

(define (f-recur n)
  (if (< n 3)
    n
    (+ (f-recur (- n 1)) 
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))
    )
  )

(define (f-iter n)
  (define (iter count a b c)  ; a = f(n-1), b = f(n-2), c = f(n-3)
    (if (= count 0)
      c
      (iter (- count 1) (+ a (* 2 b) (* 3 c)) a b)
      )
    )
  (iter n 2 1 0)
  )

