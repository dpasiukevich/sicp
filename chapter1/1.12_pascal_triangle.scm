(define (pascal-triangle r c)
  (if (or (= r 0) (= c 0) (= r c))
    1
    (+ (pascal-triangle (- r 1) c) (pascal-triangle (- r 1) (- c 1)))
    )
  )
