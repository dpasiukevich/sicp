(load "accumulate-usage.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) 
  (map (lambda (row) (dot-product row v))))

(define (transpose m)
  (accumulate-n cons () m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define m (list (list 1 2 3 4) (list 4 5 6 7) (list 6 7 8 9)))

;(matrix-*-vector m (list 1 1 1 1))
;(transpose m)
(matrix-*-matrix m (transpose m))
