(load "fixed_point.scm")

(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1.0)
