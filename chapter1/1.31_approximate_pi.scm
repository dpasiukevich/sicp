(load "procedures_as_arguments.scm")

(define (approximate-pi n)
  (define start 3)
  (define (term index)
    (define s (square index))
    (/ (- s 1) s))
  (define (next index) (+ index 2))
  (* 4.0 (product term start next (+ (* 2 (+ n 1)) ; generated indices: 3, 5, 7, 9 ...
				     start)))
  )

(approximate-pi 10)
(approximate-pi 100)
(approximate-pi 1000)
